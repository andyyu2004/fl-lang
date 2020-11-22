module Type

[<RequireQualifiedAccess>]
type Ty =
    | Int
    | Bool
    | Fn of Ty * Ty
