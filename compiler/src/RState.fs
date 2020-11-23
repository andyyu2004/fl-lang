module RState


/// ResultState (fallible state operations)
/// note the state is outside the result, so is returned on both error and success
type RState<'s, 'e, 'a> = R of ('s -> (Result<'a, 'e> * 's))

let runRState (R f) s = f s

type ResultStateBuilder() =
    member _x.Return t = R(fun s -> (Ok t, s))
    member _x.ReturnFrom(x) = x

    member _x.Bind(t, f) =
        R(fun s ->
                let (r, s') = runRState t s
                match r with
                | Ok x -> runRState (f x) s'
                // return the original state before the failing operation was run
                | Error err -> (Error err, s))

    member x.Combine(p, q) = x.Bind(p, (fun _ -> q))

    member x.Zero() = R(fun s -> (Ok(), s))

    member _x.Delay(f) =
        R(fun s -> let (R g) = f() in g s)

let rstate = ResultStateBuilder()

/// the `or` combinator
// note the intentional choice return `q s` instead of `q s'` on error
// this allows this combinator to be used as a backtrack over multiple options
let (<|>) (R p) (R q) =
    R(fun s ->
            match p s with
            | (Ok a, s') -> (Ok a, s')
            | (Error _, _s') -> q s)

/// fmap (<$>), but <$> is reserved in f#
let (<+>) f x =
    rstate {
        let! x = x
        return f x }

let (<*>) f x =
    rstate {
        let! f = f
        let! x = x
        return f x }


// again, we return `s` not `s'` on the error state
let catch (R p) t =
    R(fun s ->
            match p s with
            | (Error _, _) -> (Ok t, s)
            | (Ok a, s') -> (Ok a, s'))


let get = R(fun s -> (Ok s, s))
let put s = R(fun _ -> (Ok(), s))

let rec mapM' f =
    function
    | [] -> rstate { return () }
    | p :: ps ->
        rstate {
            do! f p
            return! mapM' f ps
        }

let rec mapM f =
    function
    | [] -> rstate { return [] }
    | p :: ps ->
        rstate {
            let! x = f p
            let! xs = mapM f ps
            return x :: xs }

let rec sequence xs = mapM id xs
