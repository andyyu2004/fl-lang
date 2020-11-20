module State

type State<'s, 'a> = S of ('s -> ('s * 'a))

let runState (S f) s = f s


type StateBuilder() =
    member _x.Return t = S(fun s -> (s, t))
    member _x.ReturnFrom(x) = x

    member _x.Bind(t, f) =
        S(fun s ->
                let (s', t') = runState t s
                runState (f t') s')

    member _x.Zero() = failwith ""
    member x.Combine(p, q) = x.Bind(p, (fun _ -> q))
    member _x.Delay(f) =
        S(fun s -> let (S g) = f() in g s)

let state = StateBuilder()

let get = S(fun s -> (s, s))
let put s = S(fun _ -> (s, ()))