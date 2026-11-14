package be.unamur.info.infom227.small.analysis

import be.unamur.info.infom227.small.ast.*
import be.unamur.info.infom227.small.cfg.Cfg

enum RaceAnalysisAbstractValue  extends Lattice[RaceAnalysisAbstractValue]:
  case Unknown
  case Race
  case NoRace
  case Bottom

  override def join(other: RaceAnalysisAbstractValue): RaceAnalysisAbstractValue  = {
    (this, other) match {
      case (RaceAnalysisAbstractValue.Bottom, _) => other
      case (_, RaceAnalysisAbstractValue.Bottom) => this
      case (RaceAnalysisAbstractValue.Unknown, _) => this
      case (_, RaceAnalysisAbstractValue.Unknown) => other
      case (RaceAnalysisAbstractValue.Race, _) => this
      case (_, RaceAnalysisAbstractValue.Race) => other
      case _ => RaceAnalysisAbstractValue.NoRace
    }
  }

  override def meet(other: RaceAnalysisAbstractValue): RaceAnalysisAbstractValue  = {
    (this, other) match {
      case (RaceAnalysisAbstractValue.Bottom, _) => this
      case (_, RaceAnalysisAbstractValue.Bottom) => other
      case (RaceAnalysisAbstractValue.Unknown, _) => other
      case (_, RaceAnalysisAbstractValue.Unknown) => this
      case (RaceAnalysisAbstractValue.Race, _) => other
      case (_, RaceAnalysisAbstractValue.Race) => this
      case _ => RaceAnalysisAbstractValue.NoRace
    }
  }

  override def toString: String = {
    this match {
      case RaceAnalysisAbstractValue.Unknown => "U"
      case RaceAnalysisAbstractValue.Bottom => "⊥"
      case RaceAnalysisAbstractValue.Race => "Race"
      case RaceAnalysisAbstractValue.NoRace => "NoRace"
    }
  }


class RaceAnalysis(cfg: Cfg, globals: Set[String], accesses: Map[String, Access]) extends ForwardMayAnalyzer[RaceAbstractState[RaceAnalysisAbstractValue]](cfg):

  override def entryAbstractState(): RaceAbstractState[RaceAnalysisAbstractValue] = RaceAbstractState(AbstractState(globals.map {global => global -> RaceAnalysisAbstractValue.NoRace}.toMap))

  override def analyseStatement(raceAbstractState: RaceAbstractState[RaceAnalysisAbstractValue], statement: Statement): RaceAbstractState[RaceAnalysisAbstractValue] = {
    statement match {
      case SpawnStatement(lineNumber, threadName, functionCall: FunctionCall) => {
        val newState = raceAbstractState.spawnThread(threadName, functionCall.name)
        //threads spawned inside the new thread
        val innerThreads = accesses(functionCall.name).spawned

        val threadsConflicts = (for {
          SpawnedThread(threadName1, functionName1) <- newState.threads
          SpawnedThread(threadName2, functionName2) <- newState.threads
          if threadName1 != threadName2
          global <- getRaceConflict(accesses(functionName1), accesses(functionName2))
        } yield global).toSet

        val argumentAccess = Access(getReadsOfStatement(statement).filter(x => globals.contains(x)), Set.empty)
        val argumentsConflicts = (for {
          SpawnedThread(_, functionName) <- raceAbstractState.threads
          global <- getRaceConflict(argumentAccess, accesses(functionName))
        } yield global).toSet

        val conflicts = threadsConflicts ++ argumentsConflicts

        if (conflicts.isEmpty) {
          RaceAbstractState(AbstractState(newState.variables), newState.threads ++ innerThreads)
        } else {
          RaceAbstractState(AbstractState(newState.variables ++ conflicts.map(global => global -> RaceAnalysisAbstractValue.Race)),
            newState.threads ++ innerThreads
          )
        }
      }
      case JoinStatement(lineNumber, threadName) => {
        raceAbstractState.joinThread(threadName)
      }
      case AssignStatement(_, _, functionCall: FunctionCall) => {
        val summary = if (accesses.contains(functionCall.name)) {
          accesses(functionCall.name)
        } else {
          Access(Set.empty, Set.empty)
        }

        val statementAccess = Access(
          getReadsOfStatement(statement).filter(globals.contains) ++ summary.reads,
          getWritesOfStatement(statement).filter(globals.contains) ++ summary.writes
        )

        val conflicts = (for {
          SpawnedThread(_, functionName) <- raceAbstractState.threads
          global <- getRaceConflict(statementAccess, accesses(functionName))
        } yield global).toSet

        RaceAbstractState(
          AbstractState(raceAbstractState.variables ++ conflicts.map(global => global -> RaceAnalysisAbstractValue.Race)),
          raceAbstractState.threads ++ summary.spawned
        )
      }
      case _ => {
        val statementAccess = Access(
          getReadsOfStatement(statement).filter(globals.contains),
          getWritesOfStatement(statement).filter(globals.contains)
        )

        val conflicts = (for {
          SpawnedThread(_, functionName) <- raceAbstractState.threads
          global <- getRaceConflict(statementAccess, accesses(functionName))
        } yield global).toSet

        if (conflicts.isEmpty) {
          raceAbstractState
        } else {
          RaceAbstractState(
            AbstractState(raceAbstractState.variables ++ conflicts.map(global => global -> RaceAnalysisAbstractValue.Race)),
            raceAbstractState.threads
          )
        }
      }
    }
  }

  override def conditionUpdate(raceAbstractState: RaceAbstractState[RaceAnalysisAbstractValue], condition: BooleanExpression): Option[RaceAbstractState[RaceAnalysisAbstractValue]] = {
    condition match {
      case BooleanConstant(false) => None
      case _ => Some(raceAbstractState)
    }
  }