package be.unamur.info.infom227.small.interpreter
import be.unamur.info.infom227.small.ast.*

type ThreadId = String

case class State(
                  globals: Map[String, VariableType],
                  threads: Map[ThreadId, ThreadState]
                )

// The (y, s2) part of a frame: where to store the return value,
// and the code to resume after the call.
case class Frame(returnVariable: String, continuation: List[Statement])

// T(t) = (code, environment).
// `environments` is the stack Σ•σ (head = active σ).
// `frames` holds one Frame per function call in progress.
case class ThreadState(
                        code: List[Statement],
                        environments: Environments[VariableType],
                        frames: List[Frame] = Nil
                      )