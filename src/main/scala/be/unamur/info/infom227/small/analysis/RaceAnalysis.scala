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


class RaceAnalysis(cfg: Cfg, globals: Set[String], accesses: Map[String, Access]) extends ForwardMayAnalyzer[AbstractState[RaceAnalysisAbstractValue]](cfg):
  private val threads = scala.collection.mutable.Map.empty[String, String]

  override def entryAbstractState(): AbstractState[RaceAnalysisAbstractValue] = AbstractState(globals.map {global => global -> RaceAnalysisAbstractValue.NoRace}.toMap)

  override def analyseStatement(abstractState: AbstractState[RaceAnalysisAbstractValue], statement: Statement): AbstractState[RaceAnalysisAbstractValue] = {
    statement match {
      case SpawnStatement(lineNumber, threadName, functionCall: FunctionCall) => {
        threads += (threadName -> functionCall.name)

        val conflicts = (for {
          (threadName1, functionName1) <- threads
          (threadName2, functionName2) <- threads
          if threadName1 != threadName2
          global <- getRaceConflict(accesses(functionName1), accesses(functionName2))
        } yield global).toSet

        if (conflicts.isEmpty) {
          abstractState
        } else {
          AbstractState(abstractState.variables ++ conflicts.map(global => global -> RaceAnalysisAbstractValue.Race))
        }
      }
      case JoinStatement(lineNumber, threadName) => {
        threads -= threadName
        abstractState
      }
      case _ => abstractState
    }
  }

  override def conditionUpdate(abstractState: AbstractState[RaceAnalysisAbstractValue], condition: BooleanExpression): Option[AbstractState[RaceAnalysisAbstractValue]] = {
    condition match {
      case BooleanConstant(false) => None
      case _ => Some(abstractState)
    }
  }