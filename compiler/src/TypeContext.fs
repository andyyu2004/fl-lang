module TypeContext

open Ast
open State
open Type
open Resolve

[<NoEquality; NoComparison>]
type TyCtxt =
    { Resolutions: Resolutions
      FnTypes: Map<NodeId, Ty>
      NodeTypes: Map<NodeId, Ty> }

    static member New resolutions =
        { Resolutions = resolutions
          FnTypes = Map []
          NodeTypes = Map [] }


let runTcx = runState

let execTcx tcx x = runTcx tcx x |> fst

let tcx = state

let recordTy nodeId ty =
    tcx {
        let! tcx = get
        let fuck = tcx.NodeTypes.Add(nodeId, ty)
        do! put { tcx with NodeTypes = fuck }
    }
