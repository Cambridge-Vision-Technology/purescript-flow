# Plan: Add retry combinator

**Issue:** #1 - Add retry combinator
**Branch:** `issue-1-retry-combinator`

## Summary

Add a `retry` combinator for workflows that may fail, enabling automatic retry logic for transient failures.

## Proposed API

```purescript
retry
  :: forall i o a e b
   . Int
  -> Workflow i o a (Either e b)
  -> Workflow i o a (Tuple Int (Either e b))
```

Returns the number of attempts made along with the final result.

## Tasks

### Task 1: Add `Retry` constructor to Workflow type

- [ ] Add `Retry` constructor to `Workflow` ADT in `src/Flow/Types.purs`
- [ ] Use CPS encoding pattern consistent with existing constructors
- [ ] Add `mkRetry` smart constructor

### Task 2: Add `retry` smart constructor to Core

- [ ] Export `retry` function from `src/Flow/Core.purs`
- [ ] Re-export from `src/Flow/Flow.purs`

### Task 3: Implement pure interpretation

- [ ] Add `runRetryCPS` in `src/Flow/Interpret/Effect.purs`
- [ ] Handle in `runWorkflow` function
- [ ] Logic: execute, retry on Left until max attempts

### Task 4: Implement monadic interpretation

- [ ] Add `runRetryCPSM` in `src/Flow/Interpret/Effect.purs`
- [ ] Handle in `runWorkflowM` function

### Task 5: Implement diagram generation

- [ ] Add retry visualization in `src/Flow/Interpret/Diagram.purs`
- [ ] Render as `[Retry Nx]` annotation or subgraph

### Task 6: Add tests

- [ ] `test/Test/Effect.purs` - Behavior tests:
  - Immediate success returns `Tuple 1 (Right result)`
  - Exhausted attempts returns `Tuple N (Left error)`
  - Single attempt (N=1) means no retry
  - Composition with other combinators
- [ ] `test/Test/Diagram.purs` - Diagram output contains retry notation
- [ ] `test/Test/Compose.purs` - Type-level verification compiles

## Test Cases

| Test | Input | Expected Output |
|------|-------|-----------------|
| Immediate success | `retry 3 (Pure (Right <<< (*2)))` with input `5` | `Tuple 1 (Right 10)` |
| Always fails | `retry 3 (Pure (const (Left "err")))` with input `5` | `Tuple 3 (Left "err")` |
| Single attempt | `retry 1 (Pure (const (Left "err")))` with input `5` | `Tuple 1 (Left "err")` |

## Design Decisions

1. **CPS encoding** - Consistent with `Par`, `Choice`, `Request` constructors
2. **Attempt count starts at 1** - First execution is attempt 1, not 0
3. **Input preserved** - Each retry receives the original input `a`
4. **Pure retry** - No delay between attempts (consumer can add delays in monadic interpreter)
