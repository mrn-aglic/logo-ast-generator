package logogrammar

import astgenerator._
import lexical.{MyLexical, MyTokenParsers, MyTokens}

import scala.collection.mutable
import scala.language.{implicitConversions, postfixOps}

/**
  * Created by Marin on 25/03/16.
  */
object LogoParser extends MyTokenParsers {

    lexical.delimiters ++= List("\n", "+", "-", "*", "/", "^", "(", ")", "{", "}", "[", "]", "<", ">", "<=", ">=", "==", "<>", ",", " ")

    lexical.reserved ++= new mutable.HashMap[String, Int]
    (
      "xcor" -> 0, "ycor" -> 0, "heading" -> 0, "towards" -> 1, "pendown" -> 0, "pendown?" -> 0,
      "penup" -> 0, "penup?" -> 0, "pd" -> 0, "pu" -> 0
      )

    /*lexical.reserved ++= mutable.HashSet("xcor", "ycor", "heading", "towards", "pendown?",
    "sum", "difference", "product", "quotient", "remainder", "minus", "less?", "greater?", "equal?",
    "notequal?", "to", "end", "if", "ifelse", "repeat", "forward", "back", "left", "right", "setxy",
    "setx", "sety", "home", "showturtle", "hideturtle", "clean", "clearscreen", "pendown", "penup", "local", "make", "list")
*/

    def isChar = elem("isChar", x => x.toString.charAt(0).isLetter)
    def isDelim = elem("isDelim", x => lexical.delimiters.toList.filter(x => x != "[" && x != " " && x != "]").contains(x.chars)) ^^ { _.chars }
    def isAllow(xs: List[Char]) = elem("isAllow", x => x.chars.forall(y => xs.contains(y) || y.isDigit) )

    def listAllow = isAllow(List('[', ' ', ']'))
    def arrayAllow = isAllow(List('[', ' ', ']'))

    def variableExp = variable ^^ { case x => Variable(x) }

    // stringValue is analagous to QuotedWord
    def stringValue = stringLit ^^ { case x => StringConst(x) }
    def numericValue = numericLit ^^ { case x => DoubleConst(x) }

    def value = variableExp | stringValue | numericValue

    def nullPrecedenceBinaryOp = firstPrecedenceBinaryOp * (

      "<"   ^^^ { (a: Expression, b: Expression) => IsLess(a, b) } |
      ">"   ^^^ { (a: Expression, b: Expression) => IsGreater(a, b) } |
      "<="  ^^^ { (a: Expression, b: Expression) => IsLessOrEqual(a, b) } |
      ">="  ^^^ { (a: Expression, b: Expression) => IsGreaterOrEqual(a, b) } |
      "=="  ^^^ { (a: Expression, b: Expression) => IsEqual(a, b) } |
      "<>"  ^^^ { (a: Expression, b: Expression) => IsNotEqual(a, b) }
      )

    def firstPrecedenceBinaryOp = secondPrecedenceBinaryOp * (

        "+" ^^^ { (a: Expression, b: Expression) => Sum(a, b) } |
        "-" ^^^ { (a: Expression, b: Expression) => Difference(a, b) }
      )

    def secondPrecedenceBinaryOp = thirdPrecedenceBinaryOp * (

        "*" ^^^ { (a: Expression, b: Expression) => Product(a, b) } |
        "/" ^^^ { (a: Expression, b: Expression) => Quotient(a, b) }
      )

    def thirdPrecedenceBinaryOp = value * (

        "^" ^^^ { (a: Expression, b: Expression) => Power(a, b) }
      )

    def binaryOp: Parser[Expression] = nullPrecedenceBinaryOp

    def unaryMinus: Parser[Negate] = "-" ~> expr ^^ { case x => Negate(x) }

    def parens: Parser[Expression] = "(" ~> expr <~ ")"

    def array: Parser[LogoArray] = "{" ~> ((guard(arrayAllow) ~> (array | value | unaryMinus))*) <~ "}" ^^ { case x => LogoArray(x: _*) }

    def list: Parser[LogoList] = "[" ~> ((guard(listAllow) ~> (list | value | unaryMinus))*) <~ "]" ^^ { case x => LogoList(x: _*) }

    def wordA: Parser[WordExp] = word("{" | "}")
    def wordL: Parser[WordExp] = word("[" | "]")

    def word(sp: Parser[String]): Parser[WordExp] = rep1(isDelim | isChar | value | sp) ^^ { case rest =>

        val xs = rest.map {

            case DoubleConst(a) => a
            case IntegerConst(a) => a
            case StringConst(a) => a
            case t => t
        }

        WordExp(xs mkString "")
    }

    def arrayWord: Parser[LogoArray] = rep1("{") ~> wordA ^^ { case x => LogoArray(x) }

    @deprecated
    def arrayWord2: Parser[LogoArrayWord] = rep1("{") ~> rep(isDelim | isChar | value | "{" | "}") ^^ { case rest =>

        val xs = rest.map {

            case DoubleConst(a) => a
            case IntegerConst(a) => a
            case StringConst(a) => a
            case t => t
        }

        LogoArrayWord(xs mkString "")
    }

    def listWord: Parser[LogoList] = rep1("[") ~> wordL ^^ { case x => LogoList(x) }

    @deprecated
    def listWord2: Parser[LogoListWord] = rep1("[") ~> rep(isDelim | isChar | value | "[" | "]") ^^ { case rest =>

        val xs = rest.map {

            case DoubleConst(a) => a
            case IntegerConst(a) => a
            case StringConst(a) => a
            case t => t
        }

        LogoListWord(xs mkString "")
    }

    def term = value | parens | unaryMinus


    def arrayOpt = array | arrayWord
    def listOpt =  list | listWord

    def expr = binaryOp | arrayOpt | listOpt | term

    def body = procedure_call

    def procedure_def = "to" ~ ident ~ rep(variableExp) ~ rep(body) ~ "end"

    //def procedure_call_extra_input = ???
    def procedure_call = ???

    def instruction = procedure_def

    def prog = rep(instruction)

    def program = rep(expr)

    def parse(s: String) = {

        val tokens = new lexical.Scanner(s)

        phrase(program)(tokens)
    }
}