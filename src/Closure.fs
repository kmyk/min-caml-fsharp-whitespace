module MinCaml.Closure

open MinCaml.AST
open MinCaml.KNormal

type Closure =
    { entry: Id
      actualFreeVars: Id list }

type CTerm =
    | Lit of Literal
    | UnOp of UnaryOp * Id
    | BinOp of BinaryOp * Id * Id
    | IfNonZero of Id * CTerm * CTerm
    | Let of (Id * Type) * CTerm * CTerm
    | Var of Id
    | MakeClosure of (Id * Type) * Closure * CTerm
    | AppClosure of Id * Id list
    | AppDirect of Id * Id list
    | Tuple of Id list
    | LetTuple of (Id * Type) list * Id * CTerm
    | Array of Id * Id
    | Get of Id * Id
    | Put of Id * Id * Id

type CFunDef =
    { name: Id * Type
      args: (Id * Type) list
      formalFreeVars: (Id * Type) list
      body: CTerm }

type CProg =
    { functions: CFunDef list
      main: CTerm }

let rec listFreeVars (e: CTerm): Set<Id> =
    match e with
    | CTerm.Lit(_) -> Set.empty
    | CTerm.UnOp(_, x) -> Set.singleton x
    | CTerm.BinOp(_, x, y) -> Set.ofList [ x; y ]
    | CTerm.IfNonZero(x, e1, e2) -> Set.add x (Set.union (listFreeVars e1) (listFreeVars e2))
    | CTerm.Let((x, t), e1, e2) -> Set.remove x (Set.union (listFreeVars e1) (listFreeVars e2))
    | CTerm.Var(x) -> Set.singleton x
    | CTerm.MakeClosure((x, t), closure, e) ->
        Set.remove x (Set.union (Set.ofList closure.actualFreeVars) (listFreeVars e))
    | CTerm.AppClosure(x, ys) -> Set.ofList (x :: ys)
    | CTerm.AppDirect(_, ys) -> Set.ofList ys
    | CTerm.Tuple(xs) -> Set.ofList xs
    | CTerm.LetTuple(xts, y, e1) -> Set.union (Set.ofList (y :: List.map fst xts)) (listFreeVars e1)
    | CTerm.Array(x, y) -> Set.ofList [ x; y ]
    | CTerm.Get(x, y) -> Set.ofList [ x; y ]
    | CTerm.Put(x, y, z) -> Set.ofList [ x; y; z ]

let toList (xs: Set<'a>): List<'a> = Set.fold (fun xs x -> x :: xs) [] xs

let rec go (toplevel: CFunDef list ref) (env: Map<Id, Type>) (known: Set<Id>) (e: KTerm): CTerm =
    match e with
    | KTerm.Lit(l) -> CTerm.Lit(l)
    | KTerm.UnOp(op, x) -> CTerm.UnOp(op, x)
    | KTerm.BinOp(op, x, y) -> CTerm.BinOp(op, x, y)
    | KTerm.IfNonZero(x, e1, e2) -> CTerm.IfNonZero(x, go toplevel env known e1, go toplevel env known e2)
    | KTerm.Let((x, t), e1, e2) ->
        CTerm.Let((x, t), go toplevel env known e1, go toplevel (Map.add x t env) known e2)
    | KTerm.Var(x) -> CTerm.Var(x)
    | KTerm.LetRec(def, e) ->
        let actualFreeVars =
            toList
                (List.fold (fun vars (x, t) -> Set.remove x vars) (KNormal.listFreeVars def.body)
                     (def.name :: def.args))
        let formalFreeVars = List.map (fun x -> (x, Map.find x env)) actualFreeVars
        let env1 = Map.add (fst def.name) (snd def.name) env
        let env2 = List.fold (fun env (x, t) -> Map.add x t env) env (def.name :: def.args)
        if List.isEmpty actualFreeVars then
            let known = Set.add (fst def.name) known
            let body = go toplevel env2 known def.body
            toplevel := { name = def.name
                          args = def.args
                          formalFreeVars = formalFreeVars
                          body = body }
                        :: !toplevel
            go toplevel env1 known e
        else
            let y = gentmp ("closure/" + fst def.name)
            let body = go toplevel env2 known def.body
            toplevel := { name = (y, snd def.name)
                          args = def.args
                          formalFreeVars = formalFreeVars
                          body = body }
                        :: !toplevel
            CTerm.MakeClosure
                (def.name,
                 { entry = y
                   actualFreeVars = actualFreeVars }, go toplevel env1 known e)
    | KTerm.App(x, ys) ->
        if Set.contains x known then CTerm.AppDirect(x, ys) else CTerm.AppClosure(x, ys)
    | KTerm.Tuple(xs) -> CTerm.Tuple(xs)
    | KTerm.LetTuple(xts, y, e) -> CTerm.LetTuple(xts, y, go toplevel env known e)
    | KTerm.Array(x, y) -> CTerm.Array(x, y)
    | KTerm.Get(x, y) -> CTerm.Get(x, y)
    | KTerm.Put(x, y, z) -> CTerm.Put(x, y, z)

let rec run (e: KTerm): CTerm * CFunDef list =
    let toplevel = ref []
    let e = go toplevel Map.empty Set.empty e
    (e, !toplevel)
