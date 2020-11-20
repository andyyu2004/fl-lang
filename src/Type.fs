module Type

type Ty =
    | TyInt
    | TyBool
    | TyFn of Ty * Ty
