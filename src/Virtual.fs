module MinCaml.Virtual

open MinCaml.AST
open MinCaml.Closure

type Label = int

type VTerm =
    | Ans of VExp
    | Let of (Id * Type) * VExp * VTerm

and VExp =
    | Nop // unit
    | Const of int // int
    | Mov of Id // (a ->) a
    | Load of Id // (int ->) a
    | Store of Id * Id // (int * a) -> unit
    | Not of Id // bool
    | Neg of Id // int
    | Add of Id * Id // int
    | Sub of Id * Id // int
    | FNeg of Id // float
    | FAdd of Id * Id // float
    | FSub of Id * Id // float
    | FMul of Id * Id // float
    | FDiv of Id * Id // float
    | FMod of Id * Id // float
    | IfZero of Id * VTerm * VTerm // a
    | IfNegative of Id * VTerm * VTerm // a
    | CallClosure of Id * Id list // a
    | CallDirect of Id * Id list // a
    | OutputChar of Id // unit
    | OutputInt of Id // unit
    | InputChar // int
    | InputInt // int

type VFunDef =
    { name: Id * Type
      args: (Id * Type) list
      formalFreeVars: (Id * Type) list
      body: VTerm }


let rec concatLet (xt: Id * Type) (e1: VTerm) (e2: VTerm): VTerm =
    match e1 with
    | Ans(e1) -> Let(xt, e1, e2)
    | Let(yt, e3, e1) -> Let(yt, e3, concatLet xt e1 e2)

let insertLet (t: Type) (e: VExp) (cont: Id -> VTerm): VTerm =
    let x = gentmp "x"
    Let((x, t), e, cont x)

let storeAnd (x: Id) (y: Id) (e: VTerm): VTerm = Let((gentmp "unit", Type.Unit), Store(x, y), e)

// even addresses are for stack
// odd addresses are for heap
let var_stack: Id = gentmp "virtaul/base"
let var_heap: Id = gentmp "virtaul/heap"
let var_stack_addr: int = 0
let var_heap_addr: int = 1

let rec go (env: Map<Id, Type>) (e: CTerm): VTerm =
    match e with
    | CTerm.Lit(l) ->
        match l with
        | Literal.Unit -> Ans(Nop)
        | Literal.Bool(p) ->
            Ans(Const(if p then 1 else 0))
        | Literal.Int(n) -> Ans(Const(n))
        | Literal.Float(x) -> failwith "not implemented" // Ans(FConst(x))
    | CTerm.UnOp(op, x) ->
        match op with
        | UnaryOp.Not -> Ans(Not(x))
        | UnaryOp.Neg -> Ans(Neg(x))
        | UnaryOp.FNeg -> Ans(FNeg(x))
    | CTerm.BinOp(op, x, y) ->
        match op with
        | BinaryOp.Add -> Ans(Add(x, y))
        | BinaryOp.Sub -> Ans(Sub(x, y))
        | BinaryOp.FAdd -> Ans(FAdd(x, y))
        | BinaryOp.FSub -> Ans(FSub(x, y))
        | BinaryOp.FMul -> Ans(FMul(x, y))
        | BinaryOp.FDiv -> Ans(FDiv(x, y))
        | BinaryOp.EQ ->
            let delta =
                if Map.find x env = Type.Float then FSub(x, y) else Sub(x, y)
            insertLet Type.Int delta (fun x -> Ans(IfZero(x, Ans(Const(1)), Ans(Const(0)))))
        | BinaryOp.LE ->
            let delta =
                if Map.find x env = Type.Float then FSub(y, x) else Sub(y, x)
            insertLet Type.Int delta (fun x -> Ans(IfNegative(x, Ans(Const(0)), Ans(Const(1)))))
    | CTerm.IfNonZero(x, e1, e2) -> Ans(IfZero(x, go env e2, go env e1))
    | CTerm.Let((x, t), e1, e2) ->
        let e1 = go env e1
        let e2 = go (Map.add x t env) e2
        concatLet (x, t) e1 e2
    | CTerm.Var(x) ->
        match Map.find x env with
        | Type.Unit -> Ans(Nop)
        | _ -> Ans(Mov(x))
    | CTerm.MakeClosure((x, t), closure, e1) ->
        let z = gentmp "closure"
        let e = Let((x, t), Mov(z), go (Map.add x t env) e1)
        let yts = List.map (fun y -> (y, Map.find y env)) closure.actualFreeVars

        let rec f (offset, store) (y, t) =
            let store =
                match t with
                | Type.Unit -> store
                | _ ->
                    insertLet Type.Int (Const(2 * offset))
                        (fun k -> insertLet Type.Int (Add(z, k)) (fun addr -> storeAnd addr y store))
            (offset + 1, store)

        let (offset, store) = List.fold f (1, e) yts
        Let
            ((z, t), Load(var_heap),
             insertLet Type.Int (Const(2 * offset))
                 (fun k ->
                     insertLet Type.Int (Add(z, k))
                         (fun value -> storeAnd z var_heap (storeAnd var_heap value store))))
    | CTerm.AppClosure(x, ys) -> Ans(CallClosure(x, ys))
    | CTerm.AppDirect(x, ys) -> Ans(CallDirect(x, ys))
    | CTerm.Tuple(xs) ->
        let y = gentmp "tuple"
        let xts = List.map (fun x -> (x, Map.find x env)) xs

        let f (offset, store) (x, t) =
            let store =
                match t with
                | Type.Unit -> store
                | _ ->
                    insertLet Type.Int (Const(2 * offset))
                        (fun k -> insertLet Type.Int (Add(y, k)) (fun addr -> storeAnd addr x store))
            (offset + 1, store)

        let (offset, store) = List.fold f (0, Ans(Mov(y))) xts
        let t = Type.Tuple(List.map snd xts)
        Let
            ((y, t), Load(var_heap),
             insertLet Type.Int (Const(2 * offset))
                 (fun k -> insertLet Type.Int (Add(y, k)) (fun value -> storeAnd var_heap value store)))
    | CTerm.LetTuple(xts, y, e) ->
        let freeVars = Closure.listFreeVars e

        let rec f env offset xts =
            match xts with
            | [] -> go env e
            | (x, t) :: xts ->
                match t with
                | Type.Unit -> f env (offset + 1) xts
                | _ when not (Set.contains x freeVars) -> f env (offset + 1) xts
                | _ ->
                    let load = f (Map.add x t env) (offset + 1) xts
                    insertLet Type.Int (Const(2 * offset))
                        (fun k -> insertLet Type.Int (Add(y, k)) (fun addr -> Let((x, t), Load(addr), load)))
        f env 0 xts
    | CTerm.Array(x, y) -> failwith "not implemented"
    | CTerm.Get(x, y) -> failwith "not implemented"
    | CTerm.Put(x, y, z) -> failwith "not implemented"

let run (main: CTerm) (toplevel: CFunDef list): VTerm * VFunDef list =
    let main = go Map.empty main

    let f (def: CFunDef): VFunDef =
        let env = Map.ofList (def.args @ def.formalFreeVars)
        { name = def.name
          args = def.args
          formalFreeVars = def.formalFreeVars
          body = go env def.body }

    let toplevel = List.map f toplevel
    (main, toplevel)
