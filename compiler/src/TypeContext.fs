module TypeContext

open Ast
open RState
open Type
open Resolve
open Unify

[<NoEquality; NoComparison>]
type TyCtxt =
    { Resolutions: Resolutions
      Tyvars: UnificationTable
      NodeTypes: Map<NodeId, Ty> }

    static member New resolutions =
        { Resolutions = resolutions
          Tyvars = UnificationTable()
          NodeTypes = Map [] }

type TypecheckOutputs =
    { NodeTypes: Map<NodeId, Ty> }

type Tcx<'a> = RState<TyCtxt, TypeError, 'a>

let runTcx = runRState

let execTcx tcx x = runTcx tcx x |> fst
let evalTcx tcx x = runTcx tcx x |> snd

let tcx = rstate


let recordSig nodeId ty: Tcx<unit> =
    tcx {
        let! tcx = get
        if tcx.NodeTypes.ContainsKey nodeId then
            failwith "signature set more than once"
        else
            let itemTypes = tcx.NodeTypes.Add(nodeId, ty)
            do! put { tcx with NodeTypes = itemTypes }
    }

let rec mkTyErr: Tcx<Ty> = tcx { return mkTy TyKind.Err }

let getRes (path: Path): Tcx<Res> =
    tcx {
        let! tcx = get
        return tcx.Resolutions.NodeResolutions.[path.Id] }

let nodeTyOpt nodeId: Tcx<option<Ty>> =
    tcx {
        let! tcx = get
        let ty = tcx.NodeTypes.TryFind nodeId
        return ty
    }


let nodeTy nodeId: Tcx<_> =
    tcx {
        let! optTy = nodeTyOpt nodeId
        return optTy.Value }

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
