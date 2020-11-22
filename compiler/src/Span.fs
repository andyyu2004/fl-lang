module Span

open Format

type Span =
    { Lo: int
      Hi: int }

let (++) s t =
    { Lo = min s.Lo t.Lo
      Hi = max s.Hi t.Hi }


type Symbol = string

type Ident =
    { Symbol: Symbol
      Span: Span }

    interface IShow with
        member this.Show() = this.Symbol