module TypeContext

open Ast
open RState
open Type
open Resolve
open Unify

[<NoEquality; NoComparison>]
type TyCtxt =
    { Resolutions: Resolutions
      ItemTypes: Map<NodeId, Ty>
      Tyvars: UnificationTable
      NodeTypes: Map<NodeId, Ty> }

    static member New resolutions =
        { Resolutions = resolutions
          Tyvars = UnificationTable()
          ItemTypes = Map []
          NodeTypes = Map [] }

type TypecheckOutputs =
    { NodeTypes: Map<NodeId, Ty> }

type Tcx<'a> = RState<TyCtxt, TypeError, 'a>

let runTcx = runRState

let execTcx tcx x = runTcx tcx x |> fst
let evalTcx tcx x = runTcx tcx x |> snd

let tcx = rstate

let recordSig nodeId ty =
    tcx {
        let! tcx = get
        if tcx.ItemTypes.ContainsKey nodeId then
            return ()
        else
            let itemTypes = tcx.ItemTypes.Add(nodeId, ty)
            do! put { tcx with ItemTypes = itemTypes }
    }

let rec mkTyErr: Tcx<Ty> = tcx { return Ty.Err }

let getRes (path: Path): Tcx<Res> =
    tcx {
        let! tcx = get
        return tcx.Resolutions.NodeResolutions.[path.Id] }

let nodeTy nodeId: Tcx<_> =
    tcx {
        let! tcx = get
        let ty = tcx.NodeTypes.[nodeId]
        return ty
    }

let recordTy nodeId ty: Tcx<_> =
    tcx {
        let! tcx = get
        let nodeTypes = tcx.NodeTypes.Add(nodeId, ty)
        do! put { tcx with NodeTypes = nodeTypes }
        return ty
    }

let probeTyvar var: Tcx<TyVarValue> =
    tcx {
        let! tcx = get
        return tcx.Tyvars.ProbeValue var }

/// resolves a type variable
let rec resolveTy ty =
    tcx {
        match ty with
        | Ty.TyVar var ->
            match! probeTyvar var with
            | TyVarValue.Known ty' -> return! resolveTy ty'
            | TyVarValue.Unconstrained -> return ty
        | _ -> return ty
    }
