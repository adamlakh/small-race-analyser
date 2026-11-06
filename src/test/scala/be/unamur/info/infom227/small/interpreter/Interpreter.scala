package be.unamur.info.infom227.small.interpreter

import be.unamur.info.infom227.small.{ast, cst}
import be.unamur.info.infom227.small.ast.Program
import org.antlr.v4.runtime.CharStreams
import org.scalatest.funsuite.AnyFunSuite

import scala.util.{Failure, Success}

class TestInterpreter extends AnyFunSuite {

  type Globals = Map[String, VariableType]

  def parseProgram(source: String): Program =
    (for {
      tree <- cst.parse(CharStreams.fromString(source))
      program <- ast.build(tree)
    } yield program).get

  // Explores EVERY interleaving. Returns every possible outcome:
  // Right(final globals) or Left(error message).
  def outcomes(program: Program, fuel: Int = 300): Set[Either[String, Globals]] = {
    val initial = initialState(program, "main", Nil).get

    def explore(state: State, fuel: Int): Set[Either[String, Globals]] =
      if (state.threads.isEmpty) Set(Right(state.globals))
      else if (fuel == 0) Set(Left("out of fuel"))
      else enabled(state) match {
        case Nil => Set(Left("deadlock"))
        case ids =>
          ids.toSet.flatMap { id =>
            step(program, state, id) match {
              case Success(next) => explore(next, fuel - 1)
              case Failure(e)    => Set(Left(e.getMessage))
            }
          }
      }

    explore(initial, fuel)
  }

  test("sequential: call + while still work") {
    val p = parseProgram("""
      global r = 0;
      function sum(n) { i = 0; s = 0; while (i < n) { s = s + i; i = i + 1; } return s; }
      function main() { r = sum(5); return 0; }
    """)
    assert(outcomes(p) == Set(Right(Map("r" -> 10))))
  }

  test("spawn + join: child's effect is visible after join, whatever the schedule") {
    val p = parseProgram("""
      global c = 0;
      function inc() { c = c + 1; return 0; }
      function main() { spawn(t1, inc()); join(t1); c = c + 10; return 0; }
    """)
    assert(outcomes(p) == Set(Right(Map("c" -> 11))))
  }

  test("atomic increment: 2 threads always give 2") {
    val p = parseProgram("""
      global c = 0;
      function inc() { c = c + 1; return 0; }
      function main() {
        spawn(t1, inc()); spawn(t2, inc());
        join(t1); join(t2);
        return 0;
      }
    """)
    assert(outcomes(p) == Set(Right(Map("c" -> 2))))
  }

  test("racy increment (read / compute / write): lost update possible, so 1 or 2") {
    val p = parseProgram("""
      global c = 0;
      function inc() { tmp = c; tmp = tmp + 1; c = tmp; return 0; }
      function main() {
        spawn(t1, inc()); spawn(t2, inc());
        join(t1); join(t2);
        return 0;
      }
    """)
    assert(outcomes(p) == Set(Right(Map("c" -> 1)), Right(Map("c" -> 2))))
  }

  test("spawn arguments are evaluated at spawn time, locals are per thread") {
    val p = parseProgram("""
      global c = 0;
      function add(n) { c = c + n; return 0; }
      function main() {
        spawn(t1, add(5)); spawn(t2, add(7));
        join(t1); join(t2);
        return 0;
      }
    """)
    assert(outcomes(p) == Set(Right(Map("c" -> 12))))
  }

  test("join on itself deadlocks") {
    val p = parseProgram("""
      function main() { join(main); return 0; }
    """)
    assert(outcomes(p) == Set(Left("deadlock")))
  }

  test("spawning an existing thread name fails") {
    val p = parseProgram("""
      function f() { return 0; }
      function main() { spawn(main, f()); return 0; }
    """)
    assert(outcomes(p).forall(_.isLeft))
  }

  test("error in a child thread is reported") {
    val p = parseProgram("""
      function bad() { x = 1 / 0; return 0; }
      function main() { spawn(t1, bad()); join(t1); return 0; }
    """)
    assert(outcomes(p).forall(_.isLeft))
  }

  test("run() works with different schedulers") {
    val p = parseProgram("""
      global c = 0;
      function inc() { c = c + 1; return 0; }
      function main() { spawn(t1, inc()); spawn(t2, inc()); join(t1); join(t2); return 0; }
    """)
    for (sched <- Seq[Scheduler]((_, ids) => ids.head, (_, ids) => ids.last)) {
      val result = be.unamur.info.infom227.small.interpreter.execute(p, "main", Nil, sched)
      assert(result.get.globals == Map("c" -> 2))
    }
  }
}