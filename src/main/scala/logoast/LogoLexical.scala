package logoast

import scala.util.parsing.combinator.lexical.Lexical

/**
  * Created by Marin on 03/04/16.
  */
class LogoLexical extends Lexical with LogoTokens {

    def token: Parser[Token] = ???

    def name = ???

    def whitespace: Parser[Any] = ???
}
