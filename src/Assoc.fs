module MinCaml.Assoc

open MinCaml.AST
open MinCaml.KNormal

let rec run (e: KTerm): KTerm =
    match e with
    | KTerm.IfNonZero(x, e1, e2) -> KTerm.IfNonZero(x, run e1, run e2)
    | KTerm.Let(xt, e1, e2) ->
        match e1 with
        | KTerm.Let(yt, e3, e4) -> KTerm.Let(yt, e3, (KTerm.Let(xt, e4, e2)))
        | KTerm.LetRec(def, e3) -> KTerm.LetRec(def, (KTerm.Let(xt, e3, e2)))
        | KTerm.LetTuple(yts, z, e3) -> KTerm.LetTuple(yts, z, run (KTerm.Let(xt, e3, e2)))
        | e1 -> KTerm.Let(xt, e1, run e2)
    | KTerm.LetRec(def, e1) -> KTerm.LetRec({ def with body = run def.body }, run e1)
    | KTerm.LetTuple(xts, y, e1) -> KTerm.LetTuple(xts, y, run e1)
    | _ -> e
