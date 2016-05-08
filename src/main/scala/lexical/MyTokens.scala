package lexical

import scala.collection.mutable
import scala.util.parsing.combinator.syntactical.TokenParsers
import scala.util.parsing.combinator.token.Tokens
import scala.language.implicitConversions

/**
  * Created by Marin on 28/03/16.
  */
trait MyTokens extends Tokens {

    case class Keyword(chars: String) extends Token {

        override def toString = chars
    }

    case class NumericLit(chars: String) extends Token {

        override def toString = chars
    }

    case class StringLit(chars: String) extends Token {

        override def toString = "\""+chars+"\""
    }

    case class Identifier(chars: String) extends Token {

        override def toString = "Identifier " + chars
    }

    case class Variable(chars: String) extends Token {

        override def toString = "Variable " + chars
    }
}

trait TMyTokenParsers extends TokenParsers {

    type Tokens <: MyTokens

    import lexical.{Identifier, Keyword, NumericLit, StringLit, Variable}

    protected val keywordCache = mutable.HashMap[String, Parser[String]]()

    implicit def keyword(chars: String): Parser[String] = {
        keywordCache.getOrElseUpdate(chars, (accept(Keyword(chars)): Parser[Elem]) ^^ (x => x.chars))
    }
        //keywordCache.getOrElseUpdate(chars, accept(Keyword(chars)) ^^ (_.chars))

    def numericLit: Parser[Double] =
        elem("number", _.isInstanceOf[NumericLit]) ^^ (_.chars.toDouble)

    def stringLit: Parser[String] =
        elem("string literal", _.isInstanceOf[StringLit]) ^^ (_.chars)

    def ident: Parser[String] =
        elem("identifier", _.isInstanceOf[Identifier]) ^^ (_.chars)

    def variable: Parser[String] =
        elem("variable", _.isInstanceOf[Variable]) ^^ (_.chars)
}

class MyTokenParsers extends TMyTokenParsers {

    type Tokens = MyTokens
    val lexical = new MyLexical

    override implicit def keyword(chars : String): Parser[String] =
        if(lexical.reserved.contains(chars) || lexical.delimiters.contains(chars)) super.keyword(chars)
        else failure("You are trying to parse \""+chars+"\", but it is neither contained in the delimiters list, nor in the reserved keyword list of your lexical object")
}