module Format

type IShow =
    abstract Show: unit -> string

let show (showable: IShow) = showable.Show()

let rec showList showables sep =
    match showables with
    | [] -> ""
    | [ x ] -> show x
    | x :: xs -> sprintf "%s%s%s" (show x) sep (showList xs sep)
