module Type

type Type =
    | Int
    | Bool
    | Fn of Type * Type
