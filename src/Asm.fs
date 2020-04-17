module MinCaml.Asm

type Label = int

type Asm =
    | StackPush of int // stk -> int :: stk
    | StackDup // a :: stk -> a :: a :: stk
    | StackSwap // a :: b :: stk -> b :: a :: stk
    | StackPop // a :: stk -> stk
    | ArithAdd // int :: int :: stk -> int :: stk
    | ArithSub // int :: int :: stk -> int :: stk
    | ArithDiv // int :: int :: stk -> int :: stk
    | ArithMul // int :: int :: stk -> int :: stk
    | ArithMod // int :: int :: stk -> int :: stk
    | HeapWrite // a :: int :: stk -> stk
    | HeapRead // int :: stk -> a :: stk
    | FlowLabel of Label // stk -> stk
    | FlowCall of Label // stk -> stk
    | FlowJump of Label // stk -> stk
    | FlowJumpIfZero of Label // int :: stk -> stk
    | FlowJumpIfNegative of Label // int :: stk -> stk
    | FlowReturn // int :: stk -> stk
    | FlowExit // int :: stk -> stk
    | IOWriteChar // char :: stk -> stk
    | IOWriteInt // int :: stk -> stk
    | IOReadChar //stk -> char :: stk
    | IOReadInt // stk -> int :: stk
