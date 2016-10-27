package lexical

import scala.language.postfixOps

import scala.util.parsing.combinator.lexical.Lexical
import scala.util.parsing.input.CharSequenceReader._

/**
  * Created by Marin on 28/03/16.
  */
class MyLexical extends Lexical with MyTokens {

    def token: Parser[Token] = (
        keywords
      | variable ~ rep(identChar | digit)           ^^ { case first ~ second ~ rest => Variable(first :: second :: rest mkString "") }
      | identChar ~ rep(identChar | digit)        ^^ { case first ~ rest => processIdent(first :: rest mkString "") }
      | procDigit                                 ^^ { case first ~ second ~ rest => NumericLit((first mkString "") :: second.getOrElse("") :: rest mkString "") }
      | '\"' ~ rep(chrExcept('\"', ' ', EofCh))   ^^ { case '\"' ~ chars => StringLit(chars mkString "") }
      | EofCh                                     ^^^ EOF
      | delim
      | failure("Illegal character")
      )

    def keywords: Parser[Token] =
        toKeyword | endKeyword

    def toKeyword =
        ('t' ~ 'o') ^^ { case _ => Keyword("to") } |
        ('T' ~ 'O') ^^ { case _ => Keyword("to") }

    def endKeyword =
        ('e' ~ 'n' ~ 'd') ^^ { case _ => Keyword("end") } |
        ('E' ~ 'N' ~ 'D') ^^ { case _ => Keyword("end") }

    def processIdent(name: String) = {

        //if (reserved contains name) {
        //    Keyword(name)
        //} else {
            Identifier(name)
        //}
    }

    def procDigit = digit.+ ~ '.'.? ~ digit.*

    def identChar = letter | elem('_')

    def variable =  ':' ~ identChar

    override def whitespace: Parser[Any] = rep[Any] (
        whitespaceChar
        | ';' ~ comment
    )

    def comment: Parser[Any] = rep(chrExcept(EofCh, ';')) ^^ { case _ => ' ' }


    /****** Čisti copy-paste ******/

    val identifiers = new scala.collection.mutable.HashMap[String, Int]

    /** The set of reserved identifiers: these will be returned as `Keyword`s. */
    val reserved = new scala.collection.mutable.HashMap[String, Int]

    /** The set of delimiters (ordering does not matter). */
    val delimiters = new scala.collection.mutable.HashSet[String]

    private lazy val _delim: Parser[Token] = {
        // construct parser for delimiters by |'ing together the parsers for the individual delimiters,
        // starting with the longest one -- otherwise a delimiter D will never be matched if there is
        // another delimiter that is a prefix of D
        def parseDelim(s: String): Parser[Token] = accept(s.toList) ^^ { x => Keyword(s) }

        val d = new Array[String](delimiters.size)
        delimiters.copyToArray(d, 0)
        scala.util.Sorting.quickSort(d)
        (d.toList map parseDelim).foldRight(failure("no matching delimiter"): Parser[Token])((x, y) => y | x)
    }
    protected def delim: Parser[Token] = _delim
}
