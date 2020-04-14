module MinCaml.Virtual

open MinCaml.AST
open MinCaml.Closure

type Label = int

type VTerm =
    | Ans of VExp
    | Let of (Id * Type) * VExp * VTerm

and VExp =
    | Nop
    | Const of int
    | Mov of Id
    | Load of Id
    | Store of Id * Id
    | Not of Id
    | Neg of Id
    | Add of Id * Id
    | Sub of Id * Id
    | FNeg of Id
    | FAdd of Id * Id
    | FSub of Id * Id
    | FMul of Id * Id
    | FDiv of Id * Id
    | FMod of Id * Id
    | IfZero of Id * VTerm * VTerm
    | IfNegative of Id * VTerm * VTerm
    | CallClosure of Id * Id list
    | CallDirect of Id * Id list
    | OutputChar of Id
    | OutputInt of Id
    | InputChar
    | InputInt

let rec concatLet (xt: Id * Type) (e1: VTerm) (e2: VTerm): VTerm =
    match e1 with
    | Ans(e1) -> Let(xt, e1, e2)
    | Let(yt, e3, e1) -> Let(yt, e3, concatLet xt e1 e2)

let insertLet (t: Type) (e: VExp) (cont: Id -> VTerm): VTerm =
    let x = gentmp "x"
    Let((x, t), e, cont x)

let storeAnd (x: Id) (y: Id) (e: VTerm): VTerm = Let((gentmp "unit", Type.Unit), Store(x, y), e)

let var_base: Id = gentmp "virtaul/base"
let var_heap: Id = gentmp "virtaul/heap"

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
                if unwrap (Map.find x env) = Type.Float then FSub(x, y) else Sub(x, y)
            insertLet Type.Int delta (fun x -> Ans(IfZero(x, Ans(Const(1)), Ans(Const(0)))))
        | BinaryOp.LE ->
            let delta =
                if unwrap (Map.find x env) = Type.Float then FSub(y, x) else Sub(y, x)
            insertLet Type.Int delta (fun x -> Ans(IfNegative(x, Ans(Const(0)), Ans(Const(1)))))
    | CTerm.IfNonZero(x, e1, e2) -> Ans(IfZero(x, go env e2, go env e1))
    | CTerm.Let((x, t), e1, e2) ->
        let e1 = go env e1
        let e2 = go (Map.add x t env) e2
        concatLet (x, t) e1 e2
    | CTerm.Var(x) ->
        match unwrap (Map.find x env) with
        | Type.Unit -> Ans(Nop)
        | _ -> Ans(Mov(x))
    | CTerm.MakeClosure((x, t), closure, e1) ->
        let z = gentmp "closure"
        let e = Let((x, t), Mov(z), go (Map.add x t env) e1)
        let yts = List.map (fun y -> (y, Map.find y env)) closure.actualFreeVars

        let rec f (offset, store) (y, t) =
            let store =
                match unwrap t with
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
                match unwrap t with
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
                match unwrap t with
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

let run (main: CTerm) (toplevel: CFunDef list): VTerm = go Map.empty main

// let fun_eq: Id = gentmp "virtual/eq"
// let fun_le: Id = gentmp "virtual/le"
// let fun_fneg: Id = gentmp "virtual/fneg"
// let fun_fadd: Id = gentmp "virtual/fadd"
// let fun_fsub: Id = gentmp "virtual/fsub"
// let fun_fmul: Id = gentmp "virtual/fmul"
// let fun_fdiv: Id = gentmp "virtual/fdiv"
// let fun_feq: Id = gentmp "virtual/feq"
// let fun_fle: Id = gentmp "virtual/fle"


// type Asm =
//     | StackPush of int  // stk -> int :: stk
//     | StackDup  // a :: stk -> a :: a :: stk
//     | StackSwap  // a :: b :: stk -> b :: a :: stk
//     | StackPop  // a :: stk -> stk
//     | ArithAdd  // int :: int :: stk -> int :: stk
//     | ArithSub  // int :: int :: stk -> int :: stk
//     | ArithDiv  // int :: int :: stk -> int :: stk
//     | ArithMul  // int :: int :: stk -> int :: stk
//     | ArithMod  // int :: int :: stk -> int :: stk
//     | HeapWrite  // a :: int :: stk -> stk
//     | HeapRead  // int :: stk -> a :: stk
//     | FlowLabel  of Label  // stk -> stk
//     | FlowCall of Label  // stk -> stk
//     | FlowJump of Label  // stk -> stk
//     | FlowJumpIfZero of Label  // int :: stk -> stk
//     | FlowJumpIfNegative of Label  // int :: stk -> stk
//     | FlowReturn  // int :: stk -> stk
//     | FlowExit  // int :: stk -> stk
//     | IOWriteChar  // char :: stk -> stk
//     | IOWriteInt  // int :: stk -> stk
//     | IOReadChar  //stk -> char :: stk
//     | IOReadInt  // stk -> int :: stk

// let rec go (genlabel: () -> Label) (label: Map<Id, Label>) (env: Map<Id, Type>) (addr: Map<Id, int>) (e: CTerm) (acc: Asm list) : Asm list =
//     match e with
//     | CTerm.Lit(l) ->
//         match l with
//         | Literal.Unit -> StackPush 0 :: acc
//         | Literal.Bool(p) -> StackPush (if p then 1 else 0) :: acc
//         | Literal.Int(n) -> StackPush n :: acc
//         | Literal.Float(x) -> raise NoFloatError
//     | CTerm.UnOp(op, x) ->
//         match op with
//         | UnaryOp.Not -> ArithSub :: HeapRead :: StackPush (Map.find x addr) :: StackPush 1 :: acc
//         | UnaryOp.Neg -> ArithSub :: HeapRead :: StackPush (Map.find x addr) :: StackPush 0 :: acc
//         | UnaryOp.FNeg -> FlowCall (Map.find id_fneg label) :: HeapRead :: StackPush (Map.find x addr) :: acc
//     | CTerm.BinOp(op, x, y) ->
//         let acc = HeapRead :: StackPush (Map.find x addr) :: acc
//         let acc = HeapRead :: StackPush (Map.find y addr) :: acc
//         match op with
//         | UnaryOp.Add -> ArithAdd :: acc
//         | UnaryOp.Sub -> ArithSub :: acc
//         | UnaryOp.FAdd -> FlowCall (Map.find id_fadd label) :: acc
//         | UnaryOp.FSub -> FlowCall (Map.find id_fsub label) :: acc
//         | UnaryOp.FMul -> FlowCall (Map.find id_fmul label) :: acc
//         | UnaryOp.FDiv -> FlowCall (Map.find id_fdiv label) :: acc
//         | UnaryOp.EQ ->
//             match Map.find env x with
//             | Bool -> ArithSub :: StackSwap :: StackPush 1 :: ArithSub :: acc
//             | Int -> FlowCall (Map.find id_eq label) :: acc
//             | Float -> FlowCall (Map.find id_feq label) :: acc
//             | _ -> failwith "something wrong"
//         | UnaryOp.LE ->
//             match Map.find env x with
//             | Bool ->
//                 // x <= y iff x = 0 or y = 1 iff 1 - (x * (1 - y))
//                 ArithSub :: StackSwap :: StackPush 1 :: ArithMul :: ArithSub :: StackSwap :: StackPush 1 :: acc
//             | Int -> FlowCall (Map.find id_le label) :: acc
//             | Float -> FlowCall (Map.find id_fle label) :: acc
//             | _ -> failwith "something wrong"
//     | CTerm.IfNonZero(x, e1, e2) ->
//         let label_else = genlabel ()
//         let label_done = genlabel ()
//         let acc = FlowJumpIfZero label_else :: HeapRead :: StackPush (Map.find x addr) :: acc
//         let acc = go genlabel label env addr e1 acc
//         let acc = FlowLabel label_else :: FlowJump label_done :: acc
//         let acc = go genlabel label env addr e2 acc
//         FlowLabel label_done :: acc
//     | CTerm.Let((x, t), e1, e2) ->
//         let acc = HeapRead :: StackPush (Map.find x addr) :: acc
//         let acc = go genlabel label env addr e1 acc
//     | CTerm.Var(x) ->
//         HeapRead :: StackPush (Map.find x addr) :: acc
//     | CTerm.MakeClosure((x, t), closure, e) ->
//     | CTerm.AppClosure(x, ys) ->
//     | CTerm.AppDirect(_, ys) ->
//     | CTerm.Tuple(xs) ->
//     | CTerm.LetTuple(xts, y, e) ->
//     | CTerm.Array(x, y) ->
//         // *heap = value + x;
//         let acc = HeapRead :: StackPush (Map.find id_heap addr) :: acc
//         // int value = *heap;
//         let acc = StackPush (Map.find id_heap addr) :: acc
//         let acc = HeapRead :: StackPush (Map.find id_heap addr) :: acc
//         let acc = HeapRead :: StackPush (Map.find x addr) :: acc
//         let acc = ArithAdd :: acc
//         let acc = HeapWrite :: acc
//         // return 2 * value + 3;
//         ArithAdd :: StackPush 3 :: ArithAdd :: StackDup :: acc
//     | CTerm.Get(x, y) ->
//         // return *(x + 2 * y)
//         let acc = HeapRead :: StackPush (Map.find x addr) :: acc
//         let acc = HeapRead :: StackPush (Map.find y addr) :: acc
//         let acc = ArithAdd :: StackDup :: acc
//         HeapRead :: ArithAdd :: acc
//     | CTerm.Put(x, y, z) ->
//         // *(x + 2 * y) = z
//         let acc = HeapRead :: StackPush (Map.find x addr) :: acc
//         let acc = HeapRead :: StackPush (Map.find y addr) :: acc
//         let acc = ArithAdd :: StackDup :: acc
//         let acc = ArithAdd :: acc
//         let acc = HeapRead :: StackPush (Map.find z addr) :: acc
//         HeapWrite :: acc


// let run (main: CTerm) (toplevel: CFunDef list): Asm list =
//     let genlabel =
//         let counter = ref 0
//         let f () =
//             let label = !counter
//             counter := !counter + 1
//             label
//         f
//     let label = [
//             (id_eq, genlabel ()),
//             (id_le, genlabel ()),
//             (id_fneg, genlabel ()),
//             (id_fadd, genlabel ()),
//             (id_fsub, genlabel ()),
//             (id_fmul, genlabel ()),
//             (id_fdiv, genlabel ()),
//             (id_fmod, genlabel ()),
//             (id_feq, genlabel ()),
//             (id_fle, genlabel ()),
//         ] @ List.map (fun def -> (fst def.name, genlabel())) toplevel
//     let label = Map.ofList label
//     let addr = Map.ofList [
//             (id_base, 0),
//             (id_heap, 1),
//         ]
//     let main = List.reverse (FlowExit :: go genlabel label Map.empty addr main [])
//     return main
