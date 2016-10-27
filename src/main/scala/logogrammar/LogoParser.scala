package logogrammar

import astgenerator._
import lexical.MyTokenParsers

import scala.language.{implicitConversions, postfixOps}

/**
  * Created by Marin on 25/03/16.
  */
object LogoParser extends MyTokenParsers {

    lexical.delimiters ++= List("\n", "+", "-", "*", "/", "^", "(", ")", "{", "}", "[", "]", "<", ">", "<=", ">=", "==", "<>", ",", " ")

    lexical.identifiers ++= IncludedInstructions.getInstructions

    def isChar = elem("isChar", x => x.toString.charAt(0).isLetter)

    def isDelim = elem("isDelim", x => lexical.delimiters.toList.filter(x => x != "[" && x != " " && x != "]").contains(x.chars)) ^^ {
        _.chars
    }

    def isAllow(xs: List[Char]) = elem("isAllow", x => x.chars.forall(y => xs.contains(y) || y.isDigit))

    def listAllow = isAllow(List('[', ' ', ']'))

    def arrayAllow = isAllow(List('[', ' ', ']'))

    def variableExp = variable ^^ (x => Variable(x))

    // stringValue is analagous to QuotedWord
    def stringValue = stringLit ^^ (x => StringConst(x))

    def numericValue = numericLit ^^ (x => DoubleConst(x))

    def value = variableExp | stringValue | numericValue

    def nullPrecedenceBinaryOp = firstPrecedenceBinaryOp * (

        "<" ^^^ { (a: Expression, b: Expression) => IsLess(a, b) } |
        ">" ^^^ { (a: Expression, b: Expression) => IsGreater(a, b) } |
        "<=" ^^^ { (a: Expression, b: Expression) => IsLessOrEqual(a, b) } |
        ">=" ^^^ { (a: Expression, b: Expression) => IsGreaterOrEqual(a, b) } |
        "==" ^^^ { (a: Expression, b: Expression) => IsEqual(a, b) } |
        "<>" ^^^ { (a: Expression, b: Expression) => IsNotEqual(a, b) }
      )

    def firstPrecedenceBinaryOp = secondPrecedenceBinaryOp * (

      "+" ^^^ { (a: Expression, b: Expression) => Sum(a, b) } |
        "-" ^^^ { (a: Expression, b: Expression) => Difference(a, b) }
      )

    def secondPrecedenceBinaryOp = thirdPrecedenceBinaryOp * (

      "*" ^^^ { (a: Expression, b: Expression) => Product(a, b) } |
        "/" ^^^ { (a: Expression, b: Expression) => Quotient(a, b) }
      )

    def thirdPrecedenceBinaryOp = (value | unaryMinus) * (

      "^" ^^^ { (a: Expression, b: Expression) => Power(a, b) }
      )

    def binaryOp: Parser[Expression] = nullPrecedenceBinaryOp

    def unaryMinus: Parser[Negate] = "-" ~> expr ^^ (x => Negate(x))

    def parens: Parser[Expression] = "(" ~> expr <~ ")"

    def array: Parser[LogoArray] = "{" ~> ((guard(arrayAllow) ~> (array | value | unaryMinus)) *) <~ "}" ^^ (x => LogoArray(x: _*))

    def list: Parser[LogoList] = "[" ~> ((guard(listAllow) ~> (list | value | unaryMinus)) *) <~ "]" ^^ (x => LogoList(x: _*))

    def wordA: Parser[WordExp] = word("{" | "}")

    def wordL: Parser[WordExp] = word("[" | "]")

    def word(sp: Parser[String]): Parser[WordExp] = rep1(isDelim | isChar | value | sp) ^^ { rest =>

        val xs = rest.map {

            case DoubleConst(a) => a
            case IntegerConst(a) => a
            case StringConst(a) => a
            case t => t
        }

        WordExp(xs mkString "")
    }

    def arrayWord: Parser[LogoArray] = rep1("{") ~> wordA ^^ (x => LogoArray(x))

    /*
    * @deprecated
    * the method will be removed later on
    * */
    @deprecated
    def arrayWord2: Parser[LogoArrayWord] = rep1("{") ~> rep(isDelim | isChar | value | "{" | "}") ^^ { rest =>

        val xs = rest.map {

            case DoubleConst(a) => a
            case IntegerConst(a) => a
            case StringConst(a) => a
            case t => t
        }

        LogoArrayWord(xs mkString "")
    }

    def listWord3: Parser[LogoList] = rep1("[") ~> wordL ^^ (x => LogoList(x))
    def listWord: Parser[LogoList] = "[" ~> rep(isDelim | isChar | value | listWord) <~ "]" ^^ { x =>

        val xs = x.map {

            case DoubleConst(a) => a
            case IntegerConst(a) => a
            case StringConst(a) => a
            case t => t
        }

        LogoList(WordExp(xs mkString ""))
    }
    /*
    * @deprecated
    * the method will be removed later on
    * */
    @deprecated
    def listWord2: Parser[LogoListWord] = rep1("[") ~> rep(isDelim | isChar | value | "[" | "]") ^^ { rest =>

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

    def listOpt = list | listWord

    def expr = binaryOp | arrayOpt | listOpt | term

    def body: Parser[Body] = rep(procedure_call | procedure_call_extra_input) ^^ (x => Body(x))

    def procedure_def: Parser[Instruction] = "to" ~> ident ~ rep(variableExp) >> { case name ~ vars =>

        if(!(lexical.identifiers.contains(name) || lexical.reserved.contains(name))){

            lexical.identifiers += name -> vars.length
            success(name) ~ success(vars) ~ body <~ "end"
        } else {

            err("Reserved already contains definition of said procedure")
        }
    } ^? ({
        case name ~ vars ~ procBody => //if !lexical.reserved.contains(name) =>

            //lexical.reserved += name -> vars.length
            ProcedureDef(name, vars, procBody)
    }, e => s"error while parsing procedure_def: $e")

    def procedure_call_extra_input = "(" ~> ident ~ rep(expr) <~ ")" ^? ({
        case name ~ exprs if lexical.identifiers.contains(name) =>

            ProcedureCallExtraInput(name, exprs)
    }, e => s"error while parsing procedure_call_extra_input")

    def procedure_call: Parser[BodyInstruction] = ident >> (x => {

        success(x) ~ repN(lexical.identifiers(x), expr)
    }) ^? ({
        case name ~ exprs if lexical.identifiers.contains(name) =>

            ProcedureCall(name, exprs)
    }, e => s"error while parsing procedure_call: $e")

    def instruction = procedure_def | procedure_call_extra_input | procedure_call

    //def program = rep(expr)
    def program = rep(instruction)

    def parse(s: String) = {

        val tokens = new lexical.Scanner(s)

        phrase(program)(tokens)
    }
}