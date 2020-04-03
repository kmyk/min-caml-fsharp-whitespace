module MinCaml.Beta

open MinCaml.AST
open MinCaml.KNormal

let rec replace (env: Map<Id, Id>) (x: Id): Id =
    match Map.tryFind x env with
    | Some(y) -> replace env y
    | None -> x


let rec run (env: Map<Id, Id>) (e: KTerm): KTerm =
    match e with
    | KTerm.Lit(l) -> KTerm.Lit(l)
    | KTerm.UnOp(op, x) -> KTerm.UnOp(op, replace env x)
    | KTerm.BinOp(op, x, y) -> KTerm.BinOp(op, replace env x, replace env y)
    | KTerm.IfNonZero(x, e1, e2) -> KTerm.IfNonZero(replace env x, run env e1, run env e2)
    | KTerm.Let((x, t), e1, e2) ->
        match e1 with
        | Var(y) -> run (Map.add y x env) e2
        | _ -> KTerm.Let((replace env x, t), run env e1, run env e2)
    | KTerm.Var(x) -> KTerm.Var(replace env x)
    | KTerm.LetRec(def, e1) ->
        let name = (replace env (fst def.name), snd def.name)
        let args = List.map (fun (x, t) -> (replace env x, t)) def.args
        let body = run env def.body

        let def =
            { name = name
              args = args
              body = body }
        KTerm.LetRec(def, run env e1)
    | KTerm.App(x, ys) -> KTerm.App(replace env x, List.map (replace env) ys)
    | KTerm.Tuple(xs) -> KTerm.Tuple(List.map (replace env) xs)
    | KTerm.LetTuple(xts, y, e1) ->
        let xts = List.map (fun (x, t) -> (replace env x, t)) xts
        KTerm.LetTuple(xts, replace env y, run env e1)
    | KTerm.Array(x, y) -> KTerm.Array(replace env x, replace env y)
    | KTerm.Get(x, y) -> KTerm.Get(replace env x, replace env y)
    | KTerm.Put(x, y, z) -> KTerm.Put(replace env x, replace env y, replace env z)

let run' (e: KTerm): KTerm = run Map.empty e
