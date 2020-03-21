module MinCaml.Typing
open MinCaml.AST

let gentyp (): Type = Type.Var(ref None)

let rec occur (r: Type option ref) (t: Type): bool =
    match t with
    | Type.Fun(xs, y) -> List.exists (occur r) xs || occur r y
    | Type.Tuple(ts) -> List.exists (occur r) ts
    | Type.Array(u) -> occur r u
    | Type.Var(r') when r' = r -> true
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
    | (Type.Var(r1), Type.Var(r2)) when r1 = r2 -> ()
    | (Type.Var({ contents = Some(u1) }), _) -> unify u1 t2
    | (_, Type.Var({ contents = Some(u2) })) -> unify t1 u2
    | (Type.Var({ contents = None } as r1), _) when not (occur r1 t2) -> r1 := Some(t2)
    | (_, Type.Var({ contents = None } as r2)) when not (occur r2 t1) -> r2 := Some(t1)
    | _ -> raise (UnifyError(t1, t2))

let rec run (env: Map<Id, Type>) (e: TermWithInfo<SourceLocation>): Type =
    try
        match e.item with
        | Term.Lit(lit) -> litType lit
        | Term.UnOp(op, e1) ->
            let t = unOpType op
            unify t (run env e1)
            t
        | Term.BinOp(op, e1, e2) ->
            match op with
            | Add | Sub ->
                unify Type.Int (run env e1)
                unify Type.Int (run env e2)
            | FAdd | FSub | FMul | FDiv ->
                unify Type.Float (run env e1)
                unify Type.Float (run env e2)
            | EQ | LE ->
                unify (run env e1) (run env e2)
            binOpRetType op
        | Term.If(e1, e2, e3) ->
            unify Type.Bool (run env e1)
            let t1 = run env e1
            unify t1 (run env e2)
            t1
        | Term.Let((x, t), e1, e2) ->
            unify t (run env e1)
            run (Map.add x t env) e2
        | Term.Var(x) ->
            match Map.tryFind x env with
            | None -> raise (UndefinedVariableError(x, e.info))
            | Some(t) -> t
        | Term.LetRec({ name = (x, t); args = yus; body = e1 }, e2) ->
            let env' = Map.add x t env
            let t1 = run (List.fold (fun env'' (y, u) -> Map.add y u env'') env' yus) e1
            unify t (Type.Fun(List.map snd yus, t1))
            run env' e2
        | Term.App(f, es) ->
            let t = gentyp ()
            unify (run env f) (Type.Fun (List.map (run env) es, t))
            t
        | Term.Tuple(es) ->
            Type.Tuple(List.map (run env) es)
        | Term.LetTuple(xts, e1, e2) ->
            unify (Type.Tuple(List.map snd xts)) (run env e1);
            run (List.fold (fun env' (x, t) -> Map.add x t env') env xts) e2
        | Term.Array(e1, e2) ->
            unify Type.Int (run env e1)
            Type.Array(run env e2)
        | Term.Get(e1, e2) ->
            let t = gentyp ()
            unify (Type.Array(t)) (run env e1)
            unify Type.Int (run env e2)
            t
        | Term.Put(e1, e2, e3) ->
            let t = run env e3
            unify (Type.Array(t)) (run env e1)
            unify Type.Int (run env e2)
            Type.Unit
    with
    | UnifyError(t1, t2) -> raise (TypingError(e, t1, t2))

let toplevel e =
    try
        // unify Type.Unit (run Map.empty e)
        unify Type.Int (run Map.empty e)
        e
    with
    | UnifyError(t1, t2) -> raise (TypingError(e, t1, t2))
