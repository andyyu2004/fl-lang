module Unify

open Type

type KeyValue<'v> =
    { Key: Tyvid
      Value: 'v
      Rank: int }

    static member New key value =
        { Key = key
          Value = value
          Rank = 0 }

type UnificationTable<'v>() =
    let mutable storage = ResizeArray()

    member _.NewKey(v: 'v) =
        let idx = storage.Count
        let tyvid = { Idx = idx }
        storage.Add <| KeyValue<'v>.New tyvid v
        tyvid

    member this.Find(k: Tyvid): Tyvid =
        let kv = storage.[k.Idx]
        if k = kv.Key then
            k
        else
            let root = this.Find kv.Key
            storage.[k.Idx] <- { kv with Key = root }
            root

    // TODO merge more efficiently (using rank)
    member this.Union x y =
        let i = this.Find x
        let j = this.Find y
        storage.[i.Idx] <- { storage.[i.Idx] with Key = j }
