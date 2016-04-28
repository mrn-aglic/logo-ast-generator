package logoast


import scala.collection.mutable
import scala.util.parsing.combinator.syntactical.TokenParsers
import scala.util.parsing.combinator.token.Tokens

/**
  * Created by Marin on 03/04/16.
  */
trait LogoTokens extends Tokens {

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

    case class Formal(chars: String) extends Token {

        override def toString = "formal " + chars
    }
}

trait LogoTokenParsers extends TokenParsers {

    type Tokens = LogoTokens
    import lexical.{Identifier, Keyword, NumericLit, StringLit, Formal}

    protected val keywordCache = mutable.HashMap[String, Parser[String]]()

    implicit def keyword(chars: String): Parser[String] =
        keywordCache.getOrElseUpdate(chars, accept(Keyword(chars)) ^^ (_.chars))

    def numericLit: Parser[Double] =
        elem("number", _.isInstanceOf[NumericLit]) ^^ (_.chars.toDouble)

    def stringLit: Parser[String] =
        elem("string literal", _.isInstanceOf[StringLit]) ^^ (_.chars)

    def ident: Parser[String] =
        elem("identifier", _.isInstanceOf[Identifier]) ^^ (_.chars)

    def formal: Parser[String] =
        elem("formal", _.isInstanceOf[Formal]) ^^ (_.chars)
}
