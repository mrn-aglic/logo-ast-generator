package astgenerator

/**
  * Created by Marin on 25/03/16.
  */

abstract class Expression

case class IntegerConst(value: Int) extends Expression
case class DoubleConst(value: Double) extends Expression
case class StringConst(value: String) extends Expression

case class Power(base: Expression, powe: Expression) extends Expression
case class Negate(value: Expression) extends Expression

case class Id(name: String) extends Expression
case class Variable(name: String) extends Expression

case object Xcor extends Expression
case object Ycor extends Expression
case object Heading extends Expression

case class Towards(exp1: Expression, exp2: Expression) extends Expression

case object IsPendown extends Expression

case class Sum(exp1: Expression, exp2: Expression) extends Expression
case class Difference(exp1: Expression, exp2: Expression) extends Expression
case class Product(exp1: Expression, exp2: Expression) extends Expression
case class Quotient(exp1: Expression, exp2: Expression) extends Expression
case class Remainder(exp1: Expression, exp2: Expression) extends Expression

case class WordExp(x: String) extends Expression

case class LogoArray(xs: Expression*) extends Expression {

    override def toString = {

        s"LogoArray(${xs.mkString(",")})"
    }
}

case class LogoArrayWord(xs: String) extends Expression

case class LogoList(xs: Expression*) extends Expression {

    override def toString = {

        s"LogoList(${xs.mkString(",")})"
    }
}

case class LogoListWord(xs: String) extends Expression

case class IsLess(exp1: Expression, exp2: Expression) extends Expression
case class IsGreater(exp1: Expression, exp2: Expression) extends Expression
case class IsLessOrEqual(exp1: Expression, exp2: Expression) extends Expression
case class IsGreaterOrEqual(exp1: Expression, exp2: Expression) extends Expression
case class IsEqual(exp1: Expression, exp2: Expression) extends Expression
case class IsNotEqual(exp1: Expression, exp2: Expression) extends Expression