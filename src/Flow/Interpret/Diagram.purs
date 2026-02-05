module Flow.Interpret.Diagram
  ( toMermaid
  ) where

import Prelude

import Data.Array as Data.Array
import Data.Exists as Data.Exists
import Data.String as Data.String
import Data.Tuple as Data.Tuple
import Flow.Types as Flow.Types

-- | State for diagram generation
-- | Tracks the next available node ID and accumulated lines
type DiagramState =
  { nextId :: Int
  , lines :: Array String
  }

-- | Result of processing a workflow node
-- | Contains the entry/exit node IDs and updated state
type NodeResult =
  { entryId :: String
  , exitId :: String
  , state :: DiagramState
  }

-- | Generate a fresh node ID
freshId :: DiagramState -> Data.Tuple.Tuple String DiagramState
freshId st =
  Data.Tuple.Tuple
    ("N" <> show st.nextId)
    (st { nextId = st.nextId + 1 })

-- | Add a line to the diagram output
addLine :: String -> DiagramState -> DiagramState
addLine line st = st { lines = Data.Array.snoc st.lines line }

-- | Add a node definition
addNode :: String -> String -> DiagramState -> DiagramState
addNode nodeId label = addLine (nodeId <> "[" <> escapeLabel label <> "]")

-- | Add a diamond (decision) node
addDiamond :: String -> String -> DiagramState -> DiagramState
addDiamond nodeId label = addLine (nodeId <> "{" <> escapeLabel label <> "}")

-- | Add an edge between two nodes
addEdge :: String -> String -> DiagramState -> DiagramState
addEdge fromId toId = addLine (fromId <> " --> " <> toId)

-- | Add a labeled edge
addLabeledEdge :: String -> String -> String -> DiagramState -> DiagramState
addLabeledEdge fromId toId label = addLine (fromId <> " -->|" <> escapeLabel label <> "| " <> toId)

-- | Escape special characters in labels for Mermaid
escapeLabel :: String -> String
escapeLabel s = Data.String.replaceAll (Data.String.Pattern "\"") (Data.String.Replacement "'") s

-- | Convert a Workflow to a Mermaid flowchart string.
-- |
-- | The generated diagram shows:
-- | - Pure: skipped (invisible pass-through)
-- | - Step: labeled rectangular node
-- | - Seq: connected nodes with arrows
-- | - Par: subgraph containing parallel branches
-- | - Choice: diamond decision node with labeled branches
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

-- | Process a workflow and return its entry/exit points
processWorkflow :: forall i o a b. Flow.Types.Workflow i o a b -> DiagramState -> NodeResult
processWorkflow (Flow.Types.Pure _) st =
  -- NOTE: Pure nodes are pass-through; create invisible node for connectivity
  let
    Data.Tuple.Tuple nodeId st' = freshId st
    st'' = addNode nodeId "..." st'
  in
    { entryId: nodeId, exitId: nodeId, state: st'' }

processWorkflow (Flow.Types.Step name inner) st =
  let
    Data.Tuple.Tuple nodeId st' = freshId st
    st'' = addNode nodeId name st'
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

-- | Process sequential composition (unwrap existential)
processSeq :: forall i o a b x. DiagramState -> Flow.Types.SeqF i o a b x -> NodeResult
processSeq st (Flow.Types.SeqF w1 w2) =
  let
    result1 = processWorkflow w1 st
    result2 = processWorkflow w2 result1.state
    st' = addEdge result1.exitId result2.entryId result2.state
  in
    { entryId: result1.entryId, exitId: result2.exitId, state: st' }

-- | Process parallel composition (unwrap CPS)
processParCPS :: forall i o a b. Flow.Types.ParCPS i o a b -> DiagramState -> NodeResult
processParCPS (Flow.Types.ParCPS k) st =
  k \_ _ _ w1 w2 -> processParWorkflows w1 w2 st

-- | Process two parallel workflows
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

-- | Process choice/branching (unwrap CPS)
processChoiceCPS :: forall i o a b. Flow.Types.ChoiceCPS i o a b -> DiagramState -> NodeResult
processChoiceCPS (Flow.Types.ChoiceCPS k) st =
  k \_ leftW rightW -> processChoiceWorkflows leftW rightW st

-- | Process choice between two workflows
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
