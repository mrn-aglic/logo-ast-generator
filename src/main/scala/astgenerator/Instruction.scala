package astgenerator

/**
  * Created by Marin on 08/05/16.
  */
trait Instruction
trait BodyInstruction

case class ProcedureCallExtraInput(name: String, expressions: List[Expression]) extends BodyInstruction with Instruction
case class ProcedureCall(name: String, expressions: List[Expression]) extends BodyInstruction with Instruction

case class Body(bodyInstruction: List[BodyInstruction]) extends Instruction
case class ProcedureDef(name: String, variables: List[Variable], body: Body) extends Instruction