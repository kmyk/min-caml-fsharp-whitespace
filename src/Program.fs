module MinCaml.Program

open System
open FSharp.Text.Lexing
open MinCaml.AST
open MinCaml.Lexer
open MinCaml.Parser
open MinCaml.Typing
open MinCaml.KNormal

[<EntryPoint>]
let main argv =
    let lexbuf = LexBuffer<char>.FromTextReader Console.In
    setInitialPos lexbuf "/dev/stdin"

    let parsed =
        try
            Parser.start Lexer.token lexbuf
        with e when e.Message.Equals "parse error" ->
            eprintf "SyntaxError: Unexpected token: \"%s\" Line: %d Column: %d" (LexBuffer<_>.LexemeString lexbuf)
                (lexbuf.StartPos.Line + 1) (lexbuf.StartPos.Column + 1)
            reraise()

    let typed =
        try
            Typing.toplevel parsed
        with
        | TypingError(e, t1, t2) -> reraise()
        | UndefinedVariableError(x, loc) -> reraise()

    eprintfn "typed:\n%A" typed

    let knorm =
        try
            KNormal.run' typed
        with KNormalizationError(_) -> reraise()
    eprintfn "knorm:\n%A" knorm

    let alpha = Alpha.run' knorm
    let beta = Beta.run' alpha
    let assoc = Assoc.run beta
    let (closure, toplevel) = Closure.run assoc
    eprintfn "closure:\n(%A, %A)" closure toplevel

    let (main, toplevel) = Virtual.run closure toplevel
    eprintfn "virtual:\n(%A, %A)" main toplevel

    let asm = RegAlloc.run main toplevel
    eprintfn "asm:"
    for op in asm do
        eprintfn "%A" op

    let code = Emit.run asm
    printf "%s" code
    0
