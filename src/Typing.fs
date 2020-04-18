module MinCaml.Typing

open MinCaml.AST

let gentyp(): Type = Type.Var(ref None)

let rec occur (r: Type option ref) (t: Type): bool =
    match t with
    | Type.Fun(xs, y) -> List.exists (occur r) xs || occur r y
    | Type.Tuple(ts) -> List.exists (occur r) ts
    | Type.Array(u) -> occur r u
    | Type.Var(r') when LanguagePrimitives.PhysicalEquality r r' -> true
    | _ -> false

let rec unify (t1: Type) (t2: Type): unit =
    match (t1, t2) with
    | (Type.Unit, Type.Unit) -> ()
    | (Type.Bool, Type.Bool) -> ()
    | (Type.Int, Type.Int) -> ()
    | (Type.Float, Type.Float) -> ()
    | (Type.Fun(xs1, y1), Type.Fun(xs2, y2)) when xs1.Length = xs2.Length -> List.iter2 unify (y1 :: xs1) (y2 :: xs2)
    | (Type.Tuple(ts1), Type.Tuple(ts2)) when ts1.Length = ts2.Length -> List.iter2 unify ts1 ts2
    | (Type.Array(u1), Type.Array(u2)) -> unify u1 u2
    | (Type.Var(r1), Type.Var(r2)) when LanguagePrimitives.PhysicalEquality r1 r2 -> ()
    | (Type.Var({ contents = Some(u1) }), _) -> unify u1 t2
    | (_, Type.Var({ contents = Some(u2) })) -> unify t1 u2
    | (Type.Var({ contents = None } as r1), _) when not (occur r1 t2) -> r1 := Some(t2)
    | (_, Type.Var({ contents = None } as r2)) when not (occur r2 t1) -> r2 := Some(t1)
    | _ -> raise (UnifyError(t1, t2))

let unify' (e: TermWithInfo<SourceLocation>) (t1: Type) (t2: Type): unit =
    try
        unify t1 t2
    with UnifyError(t1, t2) ->
        System.Runtime.ExceptionServices.ExceptionDispatchInfo.Capture(TypingError(e, t1, t2)).Throw()
        failwith ""

let rec run (env: Map<Id, Type>) (e: TermWithInfo<SourceLocation>): Type =
    match e.item with
    | Term.Lit(lit) -> litType lit
    | Term.UnOp(op, e1) ->
        let t = unOpType op
        unify' e1 t (run env e1)
        t
    | Term.BinOp(op, e1, e2) ->
        match op with
        | Add
        | Sub ->
            unify' e1 Type.Int (run env e1)
            unify' e2 Type.Int (run env e2)
        | FAdd
        | FSub
        | FMul
        | FDiv ->
            unify' e1 Type.Float (run env e1)
            unify' e2 Type.Float (run env e2)
        | EQ
        | LE -> unify' e2 (run env e1) (run env e2)
        binOpRetType op
    | Term.If(e1, e2, e3) ->
        unify' e1 Type.Bool (run env e1)
        let t2 = run env e2
        unify' e3 t2 (run env e3)
        t2
    | Term.Let((x, t), e1, e2) ->
        unify' e1 t (run env e1)
        run (Map.add x t env) e2
    | Term.Var(x) ->
        match Map.tryFind x env with
        | None -> raise (UndefinedVariableError(x, e.info))
        | Some(t) -> t
    | Term.LetRec({ name = (x, t); args = yus; body = e1 }, e2) ->
        let env' = Map.add x t env
        let t1 = run (List.fold (fun env'' (y, u) -> Map.add y u env'') env' yus) e1
        unify' { e with item = Term.Var(x) } t (Type.Fun(List.map snd yus, t1))
        run env' e2
    | Term.App(f, es) ->
        let t = gentyp()
        unify' f (run env f) (Type.Fun(List.map (run env) es, t))
        t
    | Term.Tuple(es) -> Type.Tuple(List.map (run env) es)
    | Term.LetTuple(xts, e1, e2) ->
        unify' e1 (Type.Tuple(List.map snd xts)) (run env e1)
        run (List.fold (fun env' (x, t) -> Map.add x t env') env xts) e2
    | Term.Array(e1, e2) ->
        unify' e1 Type.Int (run env e1)
        Type.Array(run env e2)
    | Term.Get(e1, e2) ->
        let t = gentyp()
        unify' e1 (Type.Array(t)) (run env e1)
        unify' e2 Type.Int (run env e2)
        t
    | Term.Put(e1, e2, e3) ->
        let t = run env e3
        unify' e1 (Type.Array(t)) (run env e1)
        unify' e2 Type.Int (run env e2)
        Type.Unit

let rec unwrap (t: Type): Type =
    match t with
    | Type.Unit -> t
    | Type.Bool -> t
    | Type.Int -> t
    | Type.Float -> t
    | Type.Fun(ts, t) -> Type.Fun(List.map unwrap ts, t)
    | Type.Tuple(ts) -> Type.Tuple(List.map unwrap ts)
    | Type.Array(t) -> Type.Array(unwrap t)
    | Type.Var({ contents = Some(t) }) -> unwrap t
    | Type.Var(_) -> Type.Unit

let rec unwrap' (e: TermWithInfo<'info>): TermWithInfo<'info> =
    let item =
        match e.item with
        | Term.Lit(_) -> e.item
        | Term.UnOp(op, e1) -> Term.UnOp(op, unwrap' e1)
        | Term.BinOp(op, e1, e2) -> Term.BinOp(op, unwrap' e1, unwrap' e2)
        | Term.If(e1, e2, e3) -> Term.If(unwrap' e1, unwrap' e2, unwrap' e3)
        | Term.Let((x, t), e1, e2) -> Term.Let((x, unwrap t), unwrap' e1, unwrap' e2)
        | Term.Var(x) -> Term.Var(x)
        | Term.LetRec({ name = (x, t); args = yus; body = e1 }, e2) ->
            Term.LetRec
                ({ name = (x, unwrap t)
                   args = List.map (fun (y, u) -> (y, unwrap u)) yus
                   body = unwrap' e1 }, unwrap' e2)
        | Term.App(f, es) -> Term.App(unwrap' f, List.map unwrap' es)
        | Term.Tuple(es) -> Term.Tuple(List.map unwrap' es)
        | Term.LetTuple(xts, e1, e2) ->
            Term.LetTuple(List.map (fun (x, t) -> (x, unwrap t)) xts, unwrap' e1, unwrap' e2)
        | Term.Array(e1, e2) -> Term.Array(unwrap' e1, unwrap' e2)
        | Term.Get(e1, e2) -> Term.Get(unwrap' e1, unwrap' e2)
        | Term.Put(e1, e2, e3) -> Term.Put(unwrap' e1, unwrap' e2, unwrap' e3)
    { e with item = item }

let toplevel e =
    unify' e Type.Int (run Map.empty e)
    unwrap' e
