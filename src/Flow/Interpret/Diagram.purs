module Flow.Interpret.Diagram
  ( toMermaid
  ) where

import Prelude

import Data.Array as Data.Array
import Data.Either as Data.Either
import Data.Exists as Data.Exists
import Data.Maybe as Data.Maybe
import Data.String as Data.String
import Data.Tuple as Data.Tuple
import Flow.Types as Flow.Types

type DiagramState =
  { nextId :: Int
  , lines :: Array String
  }

type NodeResult =
  { entryId :: String
  , exitId :: String
  , state :: DiagramState
  }

freshId :: DiagramState -> Data.Tuple.Tuple String DiagramState
freshId st =
  Data.Tuple.Tuple
    ("N" <> show st.nextId)
    (st { nextId = st.nextId + 1 })

addLine :: String -> DiagramState -> DiagramState
addLine line st = st { lines = Data.Array.snoc st.lines line }

addNode :: String -> String -> DiagramState -> DiagramState
addNode nodeId label = addLine (nodeId <> "[" <> escapeLabel label <> "]")

addDiamond :: String -> String -> DiagramState -> DiagramState
addDiamond nodeId label = addLine (nodeId <> "{" <> escapeLabel label <> "}")

addEdge :: String -> String -> DiagramState -> DiagramState
addEdge fromId toId = addLine (fromId <> " --> " <> toId)

addLabeledEdge :: String -> String -> String -> DiagramState -> DiagramState
addLabeledEdge fromId toId label = addLine (fromId <> " -->|" <> escapeLabel label <> "| " <> toId)

escapeLabel :: String -> String
escapeLabel s = Data.String.replaceAll (Data.String.Pattern "\"") (Data.String.Replacement "'") s

isPure :: forall i o a b. Flow.Types.Workflow i o a b -> Boolean
isPure (Flow.Types.Pure _) = true
isPure _ = false

toMermaid :: forall i o a b. Flow.Types.Workflow i o a b -> String
toMermaid workflow =
  let
    initialState :: DiagramState
    initialState = { nextId: 0, lines: [] }

    result :: NodeResult
    result = processWorkflow workflow initialState

    header :: String
    header = "flowchart TD"

    body :: String
    body = Data.String.joinWith "\n    " result.state.lines
  in
    header <> "\n    " <> body

processWorkflow :: forall i o a b. Flow.Types.Workflow i o a b -> DiagramState -> NodeResult
processWorkflow (Flow.Types.Pure _) st =
  let
    Data.Tuple.Tuple nodeId st' = freshId st
  in
    { entryId: nodeId, exitId: nodeId, state: st' }

processWorkflow (Flow.Types.Step name inner) st =
  let
    Data.Tuple.Tuple nodeId st' = freshId st
    st'' = addNode nodeId name st'
  in
    case isPure inner of
      true ->
        { entryId: nodeId, exitId: nodeId, state: st'' }
      false ->
        let
          innerResult = processWorkflow inner st''
          st''' = addEdge nodeId innerResult.entryId innerResult.state
        in
          { entryId: nodeId, exitId: innerResult.exitId, state: st''' }

processWorkflow (Flow.Types.Seq exists) st =
  Data.Exists.runExists (processSeq st) exists

processWorkflow (Flow.Types.Par parCps) st =
  processParCPS parCps st

processWorkflow (Flow.Types.Choice choiceCps) st =
  processChoiceCPS choiceCps st

processWorkflow (Flow.Types.Leaf label leafCps) st =
  processLeafCPS label leafCps st

processWorkflow (Flow.Types.Encap label encapCps) st =
  processEncapCPS label encapCps st

processWorkflow (Flow.Types.MapArray mapArrayCps) st =
  processMapArrayCPS mapArrayCps st

processWorkflow (Flow.Types.Timeout timeoutCps) st =
  processTimeoutCPS timeoutCps st

processWorkflow (Flow.Types.Retry retryCps) st =
  processRetryCPS retryCps st

processWorkflow (Flow.Types.Railway railwayCps) st =
  processRailwayCPS railwayCps st

processSeq :: forall i o a b x. DiagramState -> Flow.Types.SeqF i o a b x -> NodeResult
processSeq st (Flow.Types.SeqF w1 w2) =
  case isPure w1, isPure w2 of
    true, true ->
      let
        Data.Tuple.Tuple nodeId st' = freshId st
      in
        { entryId: nodeId, exitId: nodeId, state: st' }
    true, false ->
      processWorkflow w2 st
    false, true ->
      processWorkflow w1 st
    false, false ->
      let
        result1 = processWorkflow w1 st
        result2 = processWorkflow w2 result1.state
        st' = addEdge result1.exitId result2.entryId result2.state
      in
        { entryId: result1.entryId, exitId: result2.exitId, state: st' }

processParCPS :: forall i o a b. Flow.Types.ParCPS i o a b -> DiagramState -> NodeResult
processParCPS (Flow.Types.ParCPS k) st =
  k \_ _ _ w1 w2 ->
    case isPure w1, isPure w2 of
      true, true ->
        let
          Data.Tuple.Tuple nodeId st' = freshId st
        in
          { entryId: nodeId, exitId: nodeId, state: st' }
      true, false ->
        processWorkflow w2 st
      false, true ->
        processWorkflow w1 st
      false, false ->
        processParWorkflows w1 w2 st

processParWorkflows
  :: forall i o a1 b1 a2 b2
   . Flow.Types.Workflow i o a1 b1
  -> Flow.Types.Workflow i o a2 b2
  -> DiagramState
  -> NodeResult
processParWorkflows w1 w2 st =
  let
    Data.Tuple.Tuple subgraphId st' = freshId st
    subgraphName = "parallel_" <> subgraphId

    st1 = addLine ("subgraph " <> subgraphName <> " [Parallel]") st'
    result1 = processWorkflow w1 st1
    result2 = processWorkflow w2 result1.state
    st2 = addLine "end" result2.state

    Data.Tuple.Tuple forkId st3 = freshId st2
    st4 = addNode forkId "fork" st3

    Data.Tuple.Tuple joinId st5 = freshId st4
    st6 = addNode joinId "join" st5

    st7 = addEdge forkId result1.entryId st6
    st8 = addEdge forkId result2.entryId st7
    st9 = addEdge result1.exitId joinId st8
    st10 = addEdge result2.exitId joinId st9
  in
    { entryId: forkId, exitId: joinId, state: st10 }

processChoiceCPS :: forall i o a b. Flow.Types.ChoiceCPS i o a b -> DiagramState -> NodeResult
processChoiceCPS (Flow.Types.ChoiceCPS k) st =
  k \_ leftW rightW -> processChoiceWorkflows leftW rightW st

processChoiceWorkflows
  :: forall i o x y b
   . Flow.Types.Workflow i o x b
  -> Flow.Types.Workflow i o y b
  -> DiagramState
  -> NodeResult
processChoiceWorkflows leftW rightW st =
  let
    Data.Tuple.Tuple decisionId st' = freshId st
    st1 = addDiamond decisionId "Decision" st'

    leftResult = processWorkflow leftW st1
    rightResult = processWorkflow rightW leftResult.state

    st2 = addLabeledEdge decisionId leftResult.entryId "Left" rightResult.state
    st3 = addLabeledEdge decisionId rightResult.entryId "Right" st2

    Data.Tuple.Tuple mergeId st4 = freshId st3
    st5 = addNode mergeId "merge" st4

    st6 = addEdge leftResult.exitId mergeId st5
    st7 = addEdge rightResult.exitId mergeId st6
  in
    { entryId: decisionId, exitId: mergeId, state: st7 }

processLeafCPS :: forall i o a b. String -> Flow.Types.LeafCPS i o a b -> DiagramState -> NodeResult
processLeafCPS label (Flow.Types.LeafCPS _) st =
  let
    Data.Tuple.Tuple nodeId st' = freshId st
    st1 = addNode nodeId label st'
  in
    { entryId: nodeId, exitId: nodeId, state: st1 }

processEncapCPS :: forall i o a b. String -> Flow.Types.EncapCPS i o a b -> DiagramState -> NodeResult
processEncapCPS label (Flow.Types.EncapCPS k) st =
  k \_ inner ->
    case label of
      "" ->
        processWorkflow inner st
      _ ->
        let
          Data.Tuple.Tuple subgraphId st' = freshId st
          st1 = addLine ("subgraph encap_" <> subgraphId <> " [" <> escapeLabel label <> "]") st'
          innerResult = processWorkflow inner st1
          st2 = addLine "end" innerResult.state
        in
          { entryId: innerResult.entryId, exitId: innerResult.exitId, state: st2 }

processMapArrayCPS :: forall i o a b. Flow.Types.MapArrayCPS i o a b -> DiagramState -> NodeResult
processMapArrayCPS (Flow.Types.MapArrayCPS k) st =
  k \inner _ _ ->
    let
      Data.Tuple.Tuple subgraphId st' = freshId st
      st1 = addLine ("subgraph foreach_" <> subgraphId <> " [For Each]") st'
      innerResult = processWorkflow inner st1
      st2 = addLine "end" innerResult.state
    in
      { entryId: innerResult.entryId, exitId: innerResult.exitId, state: st2 }

processTimeoutCPS :: forall i o a b. Flow.Types.TimeoutCPS i o a b -> DiagramState -> NodeResult
processTimeoutCPS (Flow.Types.TimeoutCPS k) st =
  k \(Flow.Types.Milliseconds ms) inner _ ->
    let
      Data.Tuple.Tuple subgraphId st' = freshId st
      st1 = addLine ("subgraph timeout_" <> subgraphId <> " [Timeout " <> show ms <> "ms]") st'
      innerResult = processWorkflow inner st1
      st2 = addLine "end" innerResult.state
    in
      { entryId: innerResult.entryId, exitId: innerResult.exitId, state: st2 }

processRetryCPS :: forall i o a b. Flow.Types.RetryCPS i o a b -> DiagramState -> NodeResult
processRetryCPS (Flow.Types.RetryCPS k) st =
  k \policy inner _ ->
    let
      (Flow.Types.RetryPolicy p) = policy
      Data.Tuple.Tuple subgraphId st' = freshId st
      label = "Retry " <> show p.maxAttempts <> "x"
      st1 = addLine ("subgraph retry_" <> subgraphId <> " [" <> label <> "]") st'
      innerResult = processWorkflow inner st1
      st2 = addLine "end" innerResult.state
    in
      { entryId: innerResult.entryId, exitId: innerResult.exitId, state: st2 }

processRailwayCPS :: forall i o a b. Flow.Types.RailwayCPS i o a b -> DiagramState -> NodeResult
processRailwayCPS (Flow.Types.RailwayCPS k) st =
  k \w1 w2 _ ->
    let
      Data.Tuple.Tuple subgraphId st' = freshId st
      st1 = addLine ("subgraph railway_" <> subgraphId <> " [Railway]") st'

      stepResults = collectRailwaySteps w1 w2 st1

      st2 = connectSequential stepResults.results stepResults.state
      st3 = addLine "end" st2

      Data.Tuple.Tuple errorId st4 = freshId st3
      st5 = addLine (errorId <> "([Error])") st4

      st6 = addErrorEdges stepResults.results errorId st5

      firstEntry = case Data.Array.head stepResults.results of
        Data.Maybe.Just r -> r.entryId
        Data.Maybe.Nothing -> subgraphId

      lastExit = case Data.Array.last stepResults.results of
        Data.Maybe.Just r -> r.exitId
        Data.Maybe.Nothing -> subgraphId
    in
      { entryId: firstEntry, exitId: lastExit, state: st6 }

type StepCollectResult =
  { results :: Array NodeResult
  , state :: DiagramState
  }

collectRailwaySteps :: forall i o a e x b. Flow.Types.Workflow i o a (Data.Either.Either e x) -> Flow.Types.Workflow i o x (Data.Either.Either e b) -> DiagramState -> StepCollectResult
collectRailwaySteps w1 w2 st =
  let
    leftSteps = collectFromLeft w1 st
    rest = collectFromRight w2 leftSteps.state
  in
    { results: leftSteps.results <> rest.results, state: rest.state }

collectFromLeft :: forall i o a e b. Flow.Types.Workflow i o a (Data.Either.Either e b) -> DiagramState -> StepCollectResult
collectFromLeft (Flow.Types.Railway railwayCps) st =
  collectFromLeftRailwayCPS railwayCps st
collectFromLeft w st =
  let
    r = processWorkflow w st
  in
    { results: [ r ], state: r.state }

collectFromLeftRailwayCPS :: forall i o a b. Flow.Types.RailwayCPS i o a b -> DiagramState -> StepCollectResult
collectFromLeftRailwayCPS (Flow.Types.RailwayCPS k) st =
  k \w1 w2 _ -> collectRailwaySteps w1 w2 st

collectFromRight :: forall i o a e b. Flow.Types.Workflow i o a (Data.Either.Either e b) -> DiagramState -> StepCollectResult
collectFromRight (Flow.Types.Railway railwayCps) st =
  collectFromRailwayCPS railwayCps st
collectFromRight w st =
  let
    r = processWorkflow w st
  in
    { results: [ r ], state: r.state }

collectFromRailwayCPS :: forall i o a b. Flow.Types.RailwayCPS i o a b -> DiagramState -> StepCollectResult
collectFromRailwayCPS (Flow.Types.RailwayCPS k) st =
  k \w1 w2 _ -> collectRailwaySteps w1 w2 st

connectSequential :: Array NodeResult -> DiagramState -> DiagramState
connectSequential results st = case Data.Array.uncons results of
  Data.Maybe.Nothing ->
    st
  Data.Maybe.Just { head: first, tail } ->
    (Data.Array.foldl connectPair { prev: first, state: st } tail).state
  where
  connectPair acc curr = { prev: curr, state: addEdge acc.prev.exitId curr.entryId acc.state }

addErrorEdges :: Array NodeResult -> String -> DiagramState -> DiagramState
addErrorEdges results errorId st =
  Data.Array.foldl (\s r -> addLabeledEdge r.exitId errorId "Err" s) st results
