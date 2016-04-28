package astgenerator

/**
  * Created by Marin on 27/03/16.
  */
abstract class Command

abstract class BasicCommand extends Command
abstract class FunctionDef extends Command
abstract class Call extends Command

case class Invoke(id: Id, exp: Option[Expression]) extends Call

case class Commands(commands: List[Command]) extends Command
case class OneOrMoreCommands(command: List[Command]) extends Command

case class Block(oneOrMoreCommands: OneOrMoreCommands)

case class To(id: Id, variable: Option[Variable], block: Block) extends FunctionDef

case class If(exp: Expression, block: Block) extends BasicCommand
case class IfElse(exp: Expression, block1: Block, block2: Block) extends BasicCommand
case class Repeat(exp: Expression, block: Block) extends BasicCommand

case class Forward(exp: Expression) extends BasicCommand
case class Back(exp: Expression) extends BasicCommand
case class Left(exp: Expression) extends BasicCommand
case class Right(exp: Expression) extends BasicCommand
case class SetXY(exp1: Expression, exp2: Expression) extends BasicCommand
case class SetX(exp: Expression) extends BasicCommand
case class SetY(exp: Expression) extends BasicCommand
case object Home extends BasicCommand

case object ShowTurtle extends BasicCommand
case object HideTurtle extends BasicCommand
case object Clean extends BasicCommand
case object ClearScreen extends BasicCommand
case object PenDown extends BasicCommand
case object PenUp extends BasicCommand
case class PenColor(color: String) extends BasicCommand

case class Local(quotedId: String) extends BasicCommand
case class Make(quotedId: String, exp: Option[Expression]) extends BasicCommand