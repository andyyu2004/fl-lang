module Unify

open Format
open System
open Type

[<RequireQualifiedAccess>]
type TyVarValue =
    | Known of Ty
    | Unconstrained

    interface IShow with
        member this.Show() =
            match this with
            | Known ty -> show ty
            | Unconstrained -> "u"

type KeyValue =
    { Key: TyVar
      Value: TyVarValue
      Rank: int }

    static member New key value =
        { Key = key
          Value = value
          Rank = 0 }

type UnificationTable() =

    let mutable storage = ResizeArray<KeyValue>()

    member _.NewKey v =
        let idx = storage.Count
        let tyvid = { Idx = idx }
        storage.Add <| KeyValue.New tyvid v
        tyvid

    member this.Find(k: TyVar): TyVar =
        let kv = storage.[k.Idx]
        if k = kv.Key then
            k
        else
            let root = this.Find kv.Key
            storage.[k.Idx] <- { kv with Key = root }
            root

    member _this.UpdateValue k f = storage.[k.Idx] <- f storage.[k.Idx]

    // TODO merge more efficiently (using rank)
    member this.Union x y =
        let i = this.Find x
        let j = this.Find y
        this.UpdateValue i (fun v -> { v with Key = j })

    /// value of a key `k`
    member _this.Value k = storage.[k.Idx].Value

    member _this.UnifyValues v w =
        match (v, w) with
        | (TyVarValue.Known _, TyVarValue.Known _) -> failwith "unifying two known values"
        | (TyVarValue.Known _, TyVarValue.Unconstrained) -> v
        | (TyVarValue.Unconstrained, TyVarValue.Known _) -> w
        | (TyVarValue.Unconstrained, TyVarValue.Unconstrained) -> TyVarValue.Unconstrained

    member this.UnifyKV (k: TyVar) (v: TyVarValue) =
        let root = this.Find k
        let value = this.UnifyValues v (this.Value root)
        this.UpdateValue root (fun v -> { v with Value = value })

    member this.ProbeValue k =
        let root = this.Find k
        this.Value root

    override this.ToString() = show this

    interface IShow with
        member this.Show() =
            let kvs =
                query {
                    for kv in storage do
                        select kv
                }

            kvs
            |> Seq.mapi (fun i kv ->
                sprintf "\n\tτ%d = %s" i
                <| show (this.ProbeValue kv.Key))
            |> fun xs -> String.Join(";", Seq.toArray xs)
            |> sprintf "UnificationTable { %s }"
