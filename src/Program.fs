module MinCaml.Program

open System
open FSharp.Text.Lexing
open MinCaml.Lexer
open MinCaml.Parser
open MinCaml.Typing

[<EntryPoint>]
let main argv =
    let lexbuf = LexBuffer<char>.FromTextReader Console.In
    setInitialPos lexbuf "/dev/stdin"

    let parsed =
        try
            Parser.start Lexer.token lexbuf
        with
        | e when e.Message.Equals "parse error" -> raise (Exception(sprintf "SyntaxError: Unexpected token: \"%s\" Line: %d Column: %d" (LexBuffer<_>.LexemeString lexbuf) (lexbuf.StartPos.Line + 1) (lexbuf.StartPos.Column + 1)))

    let typed =
        try
            Typing.toplevel parsed
        with
        | TypingError(e, t1, t2) -> raise (Exception(sprintf "TypeError: the term \"%A\" has the expected type \"%A\" but actually has the type \"%A\" Line: %d Column: %d" e.item t1 t2 e.info.startLine e.info.startColumn))
        | UndefinedVariableError(x, loc) -> raise (Exception(sprintf "SyntaxError: Undefined variable: \"%s\" Line: %d Column: %d" x loc.startLine loc.startColumn))
    printf "%A\n" typed
    0
