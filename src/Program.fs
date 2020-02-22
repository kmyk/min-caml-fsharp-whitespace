module MinCaml.Program

open System
open FSharp.Text.Lexing
open MinCaml.Lexer
open MinCaml.Parser

[<EntryPoint>]
let main argv =
    let lexbuf = LexBuffer<char>.FromTextReader Console.In
    setInitialPos lexbuf "/dev/stdin"

    let parsed =
        try
            Parser.start Lexer.token lexbuf
        with e when e.Message.Equals "parse error" -> raise (Exception(sprintf "SyntaxError: Unexpected token: \"%s\" Line: %d Column: %d" (LexBuffer<_>.LexemeString lexbuf) (lexbuf.StartPos.Line + 1) lexbuf.StartPos.Column))

    printf "%A\n" parsed
    0
