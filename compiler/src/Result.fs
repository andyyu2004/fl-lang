module Result

type ResultBuilder() =
    member _.Return(x) = Ok x
    member _.ReturnFrom(x) = x
    member _.Bind(r, f) = Result.bind f r
    member _.Zero() = None

let result = ResultBuilder()

type OptionalBuilder() =
    member _.Return(x) = Some x
    member _.ReturnFrom(x) = x

    member _.Bind(opt, f) =
        match opt with
        | None -> None
        | Some x -> f x

    member _.Zero() = None

let optional = OptionalBuilder()
