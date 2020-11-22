module Type

type Tyvid =
    { Idx: int }

[<RequireQualifiedAccess>]
type Ty =
    | Int
    | Bool
    | Tuple of list<Ty>
    | Fn of Ty * Ty
