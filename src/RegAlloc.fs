module MinCaml.RegAlloc

open MinCaml.AST
open MinCaml.Virtual
open MinCaml.Asm

let addr (env: Map<Id, int>) (x: Id): Asm list =
    List.rev
        [ StackPush(var_stack_addr)
          HeapRead
          StackPush(2 * (Map.find x env))
          ArithAdd ]

let load (env: Map<Id, int>) (x: Id): Asm list = HeapRead :: addr env x // stk -> a :: stk
let store (env: Map<Id, int>) (x: Id): Asm list = HeapWrite :: StackSwap :: addr env x // a :: stk -> stk

let genlabel (nextLabel: int ref): int =
    let label = !nextLabel
    nextLabel := label + 1
    label

let rec go
        (env: Map<Id, int>)
        (table: Map<Id, Label>)
        (nextAddr: int)
        (nextLabel: int ref)
        (e: VTerm)
        (ops: Asm list)
        : Asm list =
    match e with
    | Ans(e) -> go' env table nextAddr nextLabel e ops
    | Let((x, _), e1, e2) ->
        let ops = go' env table nextAddr nextLabel e1 ops
        let env = (Map.add x nextAddr env)
        let ops = store env x @ ops
        go env table (nextAddr + 1) nextLabel e2 ops

and go'
    (env: Map<Id, int>)
    (table: Map<Id, Label>)
    (nextAddr: int)
    (nextLabel: int ref)
    (e: VExp)
    (ops: Asm list)
    : Asm list =
    let load = load env
    let store = store env

    match e with
    | Nop -> ops
    | Const(n) -> StackPush(n) :: ops
    | Mov(x) -> load x @ ops
    | Load(x) -> HeapRead :: load x @ ops
    | Store(x, y) -> HeapWrite :: load y @ load x @ ops
    | Not(x) -> ArithSub :: load x @ StackPush(1) :: ops
    | Neg(x) -> ArithSub :: load x @ StackPush(0) :: ops
    | Add(x, y) -> ArithAdd :: load y @ load x @ ops
    | Sub(x, y) -> ArithSub :: load y @ load x @ ops
    | FNeg(x) -> failwith "floating-point numbers are not implemented"
    | FAdd(x, y) -> failwith "floating-point numbers are not implemented"
    | FSub(x, y) -> failwith "floating-point numbers are not implemented"
    | FMul(x, y) -> failwith "floating-point numbers are not implemented"
    | FDiv(x, y) -> failwith "floating-point numbers are not implemented"
    | FMod(x, y) -> failwith "floating-point numbers are not implemented"
    | IfZero(x, e1, e2) ->
        let label1 = genlabel nextLabel
        let label2 = genlabel nextLabel
        let ops = FlowJumpIfZero(label1) :: load x @ ops
        let ops = go env table nextAddr nextLabel e2 ops
        let ops = FlowLabel(label1) :: FlowJump(label2) :: ops
        let ops = go env table nextAddr nextLabel e1 ops
        let ops = FlowLabel(label2) :: ops
        ops
    | IfNegative(x, e1, e2) ->
        let label1 = genlabel nextLabel
        let label2 = genlabel nextLabel
        let ops = FlowJumpIfNegative(label1) :: load x @ ops
        let ops = go env table nextAddr nextLabel e2 ops
        let ops = FlowLabel(label1) :: FlowJump(label2) :: ops
        let ops = go env table nextAddr nextLabel e1 ops
        let ops = FlowLabel(label2) :: ops
        ops
    | CallClosure(x, ys)
    | CallDirect(x, ys) ->
        let g (i, ops) y =
            let ops = HeapRead :: StackPush(var_stack_addr) :: ops
            let ops = ArithAdd :: StackPush(2 * i + 2 * nextAddr) :: ops
            let ops = HeapWrite :: load y @ ops
            (i + 1, ops)

        let (_, ops) = List.fold g (1, ops) ys
        // save current stack top
        let ops = ArithAdd :: StackPush(2 * nextAddr) :: HeapRead :: StackPush(var_stack_addr) :: ops
        let ops = HeapRead :: StackPush(var_stack_addr) :: ops
        let ops = HeapWrite :: ops
        // update stack top
        let ops = StackPush(var_stack_addr) :: ops
        let ops = ArithAdd :: StackPush(2 * nextAddr) :: HeapRead :: StackPush(var_stack_addr) :: ops
        let ops = HeapWrite :: ops
        FlowCall(Map.find x table) :: ops
    | OutputChar(x) -> IOWriteChar :: load x @ ops
    | OutputInt(x) -> IOWriteInt :: load x @ ops
    | InputChar -> IOReadChar :: ops
    | InputInt -> IOReadInt :: ops

let run (main: VTerm) (toplevel: VFunDef list): Asm list =
    let (_, table) =
        List.foldBack (fun def (i, table) -> (i + 1, Map.add (fst def.name) i table)) toplevel (0, Map.empty)
    let nextLabel = ref (List.length toplevel)

    let f (def: VFunDef): Asm list =
        let env = Map.empty
        let g (i, env) (x, _) = (i + 1, Map.add x i env)
        let (i, env) = List.fold g (1, env) (def.args @ def.formalFreeVars)
        let ops = [ FlowLabel(Map.find (fst def.name) table) ]

        let ops =
            let debug = false
            if debug then
                let ops =
                    IOWriteChar
                    :: StackPush(int ' ')
                       :: List.fold (fun ops c -> IOWriteChar :: StackPush(int c) :: ops) ops
                              (Seq.toList (fst def.name))
                let ops =
                    List.fold
                        (fun ops (x, _) -> IOWriteChar :: StackPush(int ' ') :: IOWriteInt :: load env x @ ops) ops
                        (def.args @ def.formalFreeVars)
                let ops = IOWriteChar :: StackPush(int '(') :: ops
                let ops = IOWriteInt :: HeapRead :: StackPush(var_stack_addr) :: ops
                let ops = IOWriteChar :: StackPush(int ')') :: ops
                let ops = IOWriteChar :: StackPush(int '\n') :: ops
                ops
            else
                ops

        let ops = go env table i nextLabel def.body ops
        // restore previous stack top
        let ops = StackPush(var_stack_addr) :: ops
        let ops = HeapRead :: HeapRead :: StackPush(var_stack_addr) :: ops
        let ops = HeapWrite :: ops
        FlowReturn :: ops

    let main =
        FlowExit :: IOWriteChar :: StackPush(int '\n') :: IOWriteInt :: go Map.empty table 1 nextLabel main []
    let toplevel = List.map f toplevel
    List.foldBack List.append (List.map List.rev (main :: toplevel)) []
