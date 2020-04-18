module MinCaml.Emit

open MinCaml.Asm

let S: string = string ' '
let T: string = string '\t'
let N: string = string '\n'

let convertInt (n: int): string =
    if n < 0 then
        failwith "numbers must be non-negative"
    else
        let rec go n acc =
            match n with
            | 0 -> S :: acc
            | n ->
                go (n / 2)
                    ((if n % 2 = 0 then S else T)
                     :: acc)
        String.concat "" (Array.ofList (go n [ N ]))

let convert (op: Asm): string =
    let stack = S
    let arith = T + S
    let heap = T + T
    let flow = N
    let io = T + N
    match op with
    | StackPush(n) -> stack + S + convertInt n
    | StackDup -> stack + N + S
    | StackSwap -> stack + N + T
    | StackPop -> stack + N + N
    | ArithAdd -> arith + S + S
    | ArithSub -> arith + S + T
    | ArithDiv -> arith + S + N
    | ArithMul -> arith + T
    | ArithMod -> arith + N
    | HeapWrite -> heap + S
    | HeapRead -> heap + T
    | FlowLabel(label) -> flow + S + S + convertInt label
    | FlowCall(label) -> flow + S + T + convertInt label
    | FlowJump(label) -> flow + S + N + convertInt label
    | FlowJumpIfZero(label) -> flow + T + S + convertInt label
    | FlowJumpIfNegative(label) -> flow + T + T + convertInt label
    | FlowReturn -> flow + T + N
    | FlowExit -> flow + N + N
    | IOWriteChar -> io + S + S
    | IOWriteInt -> io + S + T
    | IOReadChar -> io + T + S
    | IOReadInt -> io + T + T

let run (ops: Asm list): string = String.concat "" (List.map convert ops)
