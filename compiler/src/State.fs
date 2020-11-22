module State

type State<'s, 'a> = S of ('s -> ('a * 's))

let runState (S f) s = f s


type StateBuilder() =
    member _x.Return t = S(fun s -> (t, s))
    member _x.ReturnFrom(x) = x

    member _x.Bind(t, f) =
        S(fun s ->
                let (t', s') = runState t s
                runState (f t') s')

    member _x.Zero() = S(fun s -> ((), s))

    member x.Combine(p, q) = x.Bind(p, (fun _ -> q))
    member _x.Delay(f) =
        S(fun s -> let (S g) = f() in g s)

let state = StateBuilder()

let get = S(fun s -> (s, s))
let put s = S(fun _ -> ((), s))

let rec mapM' f =
    function
    | [] -> state { return () }
    | p :: ps ->
        state {
            do! f p
            return! mapM' f ps
        }

let rec mapM f =
    function
    | [] -> state { return [] }
    | p :: ps ->
        state {
            let! x = f p
            let! xs = mapM f ps
            return x :: xs }

let rec sequence xs = mapM id xs
