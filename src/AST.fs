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
    override this.ToString() = sprintf "%s:line %i:column %i" this.fileName this.startLine this.startColumn

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
    { name: Id * Type
      args: (Id * Type) list
      body: TermWithInfo<'Info> }

and TermWithInfo<'Info> = With<Term<'Info>, 'Info>

exception UnifyError of Type * Type

exception TypingError of TermWithInfo<SourceLocation> * Type * Type with
    override this.Message =
        let e = this.Data0
        let t1 = this.Data1
        let t2 = this.Data2
        sprintf "the term \"%A\" has the expected type \"%A\" but actually has the type \"%A\" at %s" e.item t1 t2
            (e.info.ToString())

exception UndefinedVariableError of Id * SourceLocation with
    override this.Message =
        let x = this.Data0
        let loc = this.Data1
        sprintf "undefined variable \"%s\" at %s" x (loc.ToString())

exception KNormalizationError of TermWithInfo<SourceLocation> with
    override this.Message =
        let e = this.Data0
        sprintf "something wrong \"%A\" at %s" e.item (e.info.ToString())

exception AlphaConversionError of Map<Id, Id> * Id with
    override this.Message =
        let env = this.Data0
        let x = this.Data0
        sprintf "something wrong %A %A" env x

let gentmp: string -> Id =
    let counter = ref 0

    let f s: Id =
        counter := !counter + 1
        sprintf "%s.%d" s !counter
    f

let litType (l: Literal): Type =
    match l with
    | Literal.Unit -> Type.Unit
    | Literal.Bool(_) -> Type.Bool
    | Literal.Int(_) -> Type.Int
    | Literal.Float(_) -> Type.Float

let unOpType (op: UnaryOp): Type =
    match op with
    | Not -> Type.Bool
    | Neg -> Type.Int
    | FNeg -> Type.Float

let binOpRetType (op: BinaryOp): Type =
    match op with
    | Add
    | Sub -> Type.Int
    | FAdd
    | FSub
    | FMul
    | FDiv -> Type.Float
    | EQ
    | LE -> Type.Bool

let rec unwrap (t: Type): Type =
    match t with
    | Type.Unit -> t
    | Type.Bool -> t
    | Type.Int -> t
    | Type.Float -> t
    | Type.Fun(ts, t) -> Type.Fun(List.map unwrap ts, t)
    | Type.Tuple(ts) -> Type.Tuple(List.map unwrap ts)
    | Type.Array(t) -> Type.Array(unwrap t)
    | Type.Var({ contents = Some(t) }) -> unwrap t
    | Type.Var(_) -> t
