module Eval

open State

type EvalCtxt =
    { nothing: unit }

type private Eval<'a> = State<EvalCtxt, 'a>
