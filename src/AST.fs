module MinCaml.AST

[<Struct>]
type With<'T, 'U> =
    { item: 'T
      info: 'U }
    override x.ToString() = x.item.ToString()

[<Struct>]
type SourceLocation =
    { fileName: string
      startLine: int
      startColumn: int
      endLine: int
      endColumn: int }
    override this.ToString() = sprintf "%s(%i/%i)" this.fileName this.startLine this.startColumn

type Id = string

type Type =
    | Unit
    | Bool
    | Int
    | Float
    | Fun of Type list * Type
    | Tuple of Type list
    | Array of Type
    | Var of Type option ref

type Literal =
    | Unit
    | Bool of bool
    | Int of int
    | Float of float

type UnaryOp =
    | Not
    | Neg
    | FNeg

type BinaryOp =
    | Add
    | Sub
    | FAdd
    | FSub
    | FMul
    | FDiv
    | EQ
    | LE

type Term<'Info> =
    | Lit of Literal
    | UnOp of UnaryOp * TermWithInfo<'Info>
    | BinOp of BinaryOp * TermWithInfo<'Info> * TermWithInfo<'Info>
    | If of TermWithInfo<'Info> * TermWithInfo<'Info> * TermWithInfo<'Info>
    | Let of (Id * Type) * TermWithInfo<'Info> * TermWithInfo<'Info>
    | Var of Id
    | LetRec of FunDef<'Info> * TermWithInfo<'Info>
    | App of TermWithInfo<'Info> * TermWithInfo<'Info> list
    | Tuple of TermWithInfo<'Info> list
    | LetTuple of (Id * Type) list * TermWithInfo<'Info> * TermWithInfo<'Info>
    | Array of TermWithInfo<'Info> * TermWithInfo<'Info>
    | Get of TermWithInfo<'Info> * TermWithInfo<'Info>
    | Put of TermWithInfo<'Info> * TermWithInfo<'Info> * TermWithInfo<'Info>
and FunDef<'Info> =
    { name : Id * Type
      args : (Id * Type) list
      body : TermWithInfo<'Info> }
and TermWithInfo<'Info> = With<Term<'Info>, 'Info>
