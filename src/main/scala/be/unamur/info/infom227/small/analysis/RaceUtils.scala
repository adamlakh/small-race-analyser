package be.unamur.info.infom227.small.analysis

import be.unamur.info.infom227.small.ast.*

def getGlobals(program: Program): Set[String] = program.globals.keySet

def getBodies(program: Program): Map[String, List[Statement]] = {
  program.functions.map { (name, function) =>
    name -> function.body
  }
}

def getAllStatements(body: List[Statement],
                     bodies: Map[String, List[Statement]],
                     visited: Set[String]): List[Statement] = {
  body.flatMap(statement => statement match {
    case IfStatement(_, _, ifBody, elseBody) =>
      statement :: (getAllStatements(ifBody, bodies, visited) ::: getAllStatements(elseBody, bodies, visited))
    case WhileStatement(_, _, whileBody) =>
      statement :: getAllStatements(whileBody, bodies, visited)
    case AssignStatement(_, _, functionCall: FunctionCall) if bodies.contains(functionCall.name) && !visited.contains(functionCall.name) => {
      statement :: getAllStatements(bodies(functionCall.name), bodies, visited + functionCall.name)
    }
    case SpawnStatement(_, _, functionCall: FunctionCall) if bodies.contains(functionCall.name) && !visited.contains(functionCall.name) => {
      statement :: getAllStatements(bodies(functionCall.name), bodies, visited + functionCall.name)
    }
    case _ => List(statement)
  })
}


def getWritesOfStatement(statement: Statement): Set[String] = statement match {
  case AssignStatement(_, variable: String, _) => Set(variable)
  case _ => Set.empty
}

def getReadsOfExpression(expression: Expression): Set[String] = expression match {
  case Variable(name) => Set(name)
  case ArithmeticConstant(_) => Set.empty
  case BooleanConstant(_) => Set.empty
  case ArithmeticBinaryOperation(left, _, right) => getReadsOfExpression(left) ++ getReadsOfExpression(right)
  case BooleanBinaryOperation(left, _, right) => getReadsOfExpression(left) ++ getReadsOfExpression(right)
  case BooleanNegOperation(value) => getReadsOfExpression(value)
  case IntegerComparisonOperation(left, _, right) => getReadsOfExpression(left) ++ getReadsOfExpression(right)
}

def getReadsOfStatement(statement: Statement): Set[String] = statement match {
  case AssignStatement(_, _, expression) => expression match {
    case expression: Expression => getReadsOfExpression(expression)
    case functionCall: FunctionCall => functionCall.arguments.flatMap(getReadsOfExpression).toSet
  }
  case IfStatement(_, booleanExpression: BooleanExpression, _, _) => getReadsOfExpression(booleanExpression)
  case WhileStatement(_,booleanExpression: BooleanExpression, _) => getReadsOfExpression(booleanExpression)
  case ReturnStatement(_, expression: Expression) => getReadsOfExpression(expression)
  case SpawnStatement(_, _, functionCall) => functionCall.arguments.flatMap(getReadsOfExpression).toSet
  case JoinStatement(_, _) => Set.empty
}

case class Access(reads: Set[String], writes: Set[String], spawned: Set[SpawnedThread] = Set.empty )

def getReadsAndWrites(program: Program): Map[String, Access] = {
  val globals = getGlobals(program)
  val bodies = getBodies(program)
  for {
    (name, body) <- bodies
    statements = getAllStatements(body, bodies, Set(name))
    reads = statements.flatMap(x => getReadsOfStatement(x)).toSet.filter(x => globals.contains(x))
    writes = statements.flatMap(x => getWritesOfStatement(x)).toSet.filter(x => globals.contains(x))
    spawned = statements.flatMap(x => x match {
      case SpawnStatement(_, threadName, call) => Set(SpawnedThread(threadName, call.name))
      case _ => Set.empty[SpawnedThread]
    }).toSet
  } yield name -> Access(reads, writes, spawned)
}

def getRaceConflict(first: Access, second: Access): Set[String] = {
  first.writes.filter(x => second.reads.contains(x) || second.writes.contains(x)) ++ second.writes.filter(x => first.reads.contains(x))
  }