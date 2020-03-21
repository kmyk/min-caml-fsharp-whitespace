module MinCaml.KNormal
open MinCaml.AST

type KTerm =
    | Lit of Literal
    | UnOp of UnaryOp * Id
    | BinOp of BinaryOp * Id * Id
    | IfNonZero of Id * KTerm * KTerm
    | Let of (Id * Type) * KTerm * KTerm
    | Var of Id
    | LetRec of KFunDef * KTerm
    | App of Id * Id list
    | Tuple of Id list
    | LetTuple of (Id * Type) list * Id * KTerm
    | Array of Id * Id
    | Get of Id * Id
    | Put of Id * Id * Id
and KFunDef =
    { name : Id * Type
      args : (Id * Type) list
      body : KTerm }

let insertLet (env: Map<Id, Type>) ((e, t): KTerm * Type) (cont: Map<Id, Type> -> Id -> KTerm * Type): KTerm * Type =
    match e with
    | KTerm.Var(x) -> cont env x
    | _ ->
        let x = gentmp "k"
        let env = Map.add x t env
        let (e', t') = cont env x
        KTerm.Let((x, t), e, e'), t'

let rec run (env: Map<Id, Type>) (e: TermWithInfo<SourceLocation>): KTerm * Type =
    match e.item with
    | Term.Lit(l) ->
        (KTerm.Lit(l), litType l)
    | Term.UnOp(op, e1) ->
        insertLet env (run env e1)
          (fun env x -> (KTerm.UnOp(op, x), unOpType op))
    | Term.BinOp(op, e1, e2) ->
        insertLet env (run env e1)
            (fun env x -> insertLet env (run env e2)
                            (fun env y -> (KTerm.BinOp(op, x, y), binOpRetType op)))
    | Term.If(e1, e2, e3) ->
        let (e2, t2) = run env e2
        let (e3, t3) = run env e3
        if t2 = t3 then
          insertLet env (run env e1)
            (fun env x -> (KTerm.IfNonZero(x, e2, e3), t2))
        else
          raise (KNormalizationError(e))
    | Term.Let((x, t), e1, e2) ->
        let (e1, t1) = run env e1
        let env = Map.add x t env
        let (e2, t2) = run env e2
        (KTerm.Let((x, t), e1, e2), t2)
    | Term.Var(x) ->
        match Map.tryFind x env with
        | None -> raise (KNormalizationError(e))
        | Some(t) -> (KTerm.Var(x), t)
    | Term.LetRec(def, e) ->
        let (x, t) = def.name
        let env = Map.add x t env
        let (e, _) = run env e
        let env = List.fold (fun env (x, t) -> Map.add x t env) env def.args
        let (body, _) = run env def.body
        let def: KFunDef = { name = def.name; args = def.args; body = body }
        (KTerm.LetRec(def, e), t)
    | Term.App(e1, e2s) ->
        match run env e1 with
        | (e1, (Type.Fun(us, t) as t1)) ->
            let rec bind env f us acc e2s: KTerm * Type =
              match (us, e2s) with
              | ([], []) -> (KTerm.App(f, acc), t)
              | (us, []) -> (KTerm.App(f, acc), Type.Fun(us, t))
              | (u :: us, e2 :: e2s) ->
                  let (_, t2) as e2' = run env e2
                  if t2 = u then
                    insertLet env e2'
                      (fun env x -> bind env f us (acc @ [x]) e2s)
                  else
                    raise (KNormalizationError(e))
              | _ -> raise (KNormalizationError(e))
            insertLet env (e1, t1)
              (fun env f -> bind env f us [] e2s)
        | _ -> raise (KNormalizationError(e))
    | Term.Tuple(es) ->
        let rec bind env acc acc' =
          function
          | [] -> (KTerm.Tuple(acc), Type.Tuple(acc'))
          | (e :: es) ->
              let (_, t) as e' = run env e
              insertLet env e'
                (fun env x -> bind env (acc @ [x]) (acc' @ [t]) es)
        bind env [] [] es
    | Term.LetTuple(def, e1, e2) ->
        let (e2, t2) = run env e2
        insertLet env (run env e1)
          (fun env x -> (KTerm.LetTuple(def, x, e2), t2))
    | Term.Array(e1, e2) ->
        let e1' = run env e1
        let (_, t) as e2' = run env e2
        insertLet env e1'
          (fun env x -> insertLet env e2'
                          (fun env y -> (KTerm.Array(x, y), Type.Array(t))))
    | Term.Get(e1, e2) ->
        let (_, t1) as e1' = run env e1
        match t1 with
        | Type.Array(t) ->
            insertLet env e1'
              (fun env x -> insertLet env (run env e2)
                              (fun env y -> (KTerm.Get(x, y), t)))
        | _ -> raise (KNormalizationError(e1))
    | Term.Put(e1, e2, e3) ->
        insertLet env (run env e1)
          (fun env x -> insertLet env (run env e2)
                          (fun env y -> insertLet env (run env e3)
                                          (fun env z -> (KTerm.Put(x, y, z), Type.Unit))))

let run' (e: TermWithInfo<SourceLocation>): KTerm = fst (run Map.empty e)
