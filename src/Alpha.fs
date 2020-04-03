module MinCaml.Alpha

open MinCaml.AST
open MinCaml.KNormal

let rename (env: Map<Id, Id> ref) (x: Id): Id =
    match Map.tryFind x !env with
    | Some(y) -> y
    | None ->
        let y = gentmp "alpha"
        env := Map.add x y !env
        y


let rec run (env: Map<Id, Id> ref) (e: KTerm): KTerm =
    match e with
    | KTerm.Lit(l) -> KTerm.Lit(l)
    | KTerm.UnOp(op, x) -> KTerm.UnOp(op, rename env x)
    | KTerm.BinOp(op, x, y) -> KTerm.BinOp(op, rename env x, rename env y)
    | KTerm.IfNonZero(x, e1, e2) -> KTerm.IfNonZero(rename env x, run env e1, run env e2)
    | KTerm.Let((x, t), e1, e2) -> KTerm.Let((rename env x, t), run env e1, run env e2)
    | KTerm.Var(x) -> KTerm.Var(rename env x)
    | KTerm.LetRec(def, e1) ->
        let name = (rename env (fst def.name), snd def.name)
        let args = List.map (fun (x, t) -> (rename env x, t)) def.args
        let body = run env def.body

        let def =
            { name = name
              args = args
              body = body }
        KTerm.LetRec(def, run env e1)
    | KTerm.App(x, ys) -> KTerm.App(rename env x, List.map (rename env) ys)
    | KTerm.Tuple(xs) -> KTerm.Tuple(List.map (rename env) xs)
    | KTerm.LetTuple(xts, y, e1) ->
        let xts = List.map (fun (x, t) -> (rename env x, t)) xts
        KTerm.LetTuple(xts, rename env y, run env e1)
    | KTerm.Array(x, y) -> KTerm.Array(rename env x, rename env y)
    | KTerm.Get(x, y) -> KTerm.Get(rename env x, rename env y)
    | KTerm.Put(x, y, z) -> KTerm.Put(rename env x, rename env y, rename env z)

let run' (e: KTerm): KTerm = run (ref Map.empty) e
