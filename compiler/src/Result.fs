module Result

type ResultBuilder() =
    member _.Return(x) = Ok x
    member _.ReturnFrom(x) = x
    member _.Bind(r, f) = Result.bind f r
    member _.Zero() = None

let result = ResultBuilder()
