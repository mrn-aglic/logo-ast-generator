package basicparser

/**
  * Created by Marin on 25/03/16.
  */
sealed abstract class Expr {

    def eval(): Double
}

case class Const(value: Double) extends Expr {

    def eval(): Double = value
}

case class Add(left: Expr, right: Expr) extends Expr {

    def eval(): Double = left.eval() + right.eval()
}