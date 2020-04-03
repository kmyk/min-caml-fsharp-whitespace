module MinCaml.Alpha

open MinCaml.AST
open MinCaml.KNormal

let replace (env: Map<Id, Id>) (x: Id): Id =
    match Map.tryFind x env with
    | Some(y) -> y
    | None -> raise (AlphaConversionError(env, x))

let rename (env: Map<Id, Id>) (x: Id): Map<Id, Id> * Id =
    match Map.tryFind x env with
    | Some(_) -> raise (AlphaConversionError(env, x))
    | None ->
        let y = gentmp "alpha"
        let env = Map.add x y env
        env, y


let rec run (env: Map<Id, Id>) (e: KTerm): KTerm =
    match e with
    | KTerm.Lit(l) -> KTerm.Lit(l)
    | KTerm.UnOp(op, x) -> KTerm.UnOp(op, replace env x)
    | KTerm.BinOp(op, x, y) -> KTerm.BinOp(op, replace env x, replace env y)
    | KTerm.IfNonZero(x, e1, e2) -> KTerm.IfNonZero(replace env x, run env e1, run env e2)
    | KTerm.Let((x, t), e1, e2) ->
        let e1 = run env e1
        let (env, x) = rename env x
        let e2 = run env e2
        KTerm.Let((x, t), e1, e2)
    | KTerm.Var(x) -> KTerm.Var(replace env x)
    | KTerm.LetRec(def, e1) ->
        let (env, x) = rename env (fst def.name)
        let name = (x, snd def.name)
        let e1 = run env e1

        let (env, args) =
            List.fold (fun (env, args) (x, t) ->
                let (env, x) = rename env x in (env, args @ [ (x, t) ])) (env, []) def.args

        let body = run env def.body
        KTerm.LetRec
            ({ name = name
               args = args
               body = body }, e1)
    | KTerm.App(x, ys) -> KTerm.App(replace env x, List.map (replace env) ys)
    | KTerm.Tuple(xs) -> KTerm.Tuple(List.map (replace env) xs)
    | KTerm.LetTuple(xts, y, e1) ->
        let y = replace env y

        let (env, xts) =
            List.fold (fun (env, xts) (x, t) ->
                let (env, x) = rename env x in (env, xts @ [ (x, t) ])) (env, []) xts

        let e1 = run env e1
        KTerm.LetTuple(xts, y, e1)
    | KTerm.Array(x, y) -> KTerm.Array(replace env x, replace env y)
    | KTerm.Get(x, y) -> KTerm.Get(replace env x, replace env y)
    | KTerm.Put(x, y, z) -> KTerm.Put(replace env x, replace env y, replace env z)

let run' (e: KTerm): KTerm = run Map.empty e
