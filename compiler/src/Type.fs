module Type

open Format

/// index into the unification tables
type TyVar =
    { Idx: int }

    override this.ToString() = show this
    interface IShow with
        member this.Show() = sprintf "τ%d" this.Idx


[<RequireQualifiedAccess>]
type TyKind =
    | Int
    | Bool
    | Err
    | Tuple of list<Ty>
    | Fn of Ty * Ty
    | TyVar of TyVar

    override this.ToString() = show this
    interface IShow with
        member this.Show() =
            match this with
            | Int -> "int"
            | Bool -> "bool"
            | Err -> "err"
            | Tuple(xs) -> sprintf "(%s)" (showList xs ",")
            | Fn(param, ret) -> sprintf "(%s -> %s)" (show param) (show ret)
            | TyVar(tyvar) -> (show tyvar)

and Ty =
    { Kind: TyKind }

    override this.ToString() = show this

    interface IShow with
        member this.Show() = show this.Kind


and Substs = array<Ty>

let mkTy kind = { Kind = kind }

[<RequireQualifiedAccess>]
type TypeError =
    | Mismatch of Ty * Ty

    override this.ToString() = show this

    interface IShow with
        member this.Show() =
            match this with
            | Mismatch(expected, found) -> sprintf "expected type `%s`, found `%s`" (show expected) (show found)

type TypeResult<'a> = Result<'a, TypeError>

/// formats the type variables nicely by starting from `a` etc.
type TypeFormatter =
    { counter: int
      map: Map<TyVar, int> }
