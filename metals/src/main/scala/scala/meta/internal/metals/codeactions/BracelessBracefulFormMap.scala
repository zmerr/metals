package scala.meta.internal.metals.codeactions

import scala.meta.internal.metals.codeactions.BraceHolder.BraceHolder
import scala.meta.internal.metals.codeactions.BracelessEnd.BracelessEnd
import scala.meta.internal.metals.codeactions.BracelessOpening.BracelessOpening

object BracelessBracefulFormMap {
  val map: Map[BraceHolder, BracelessOpeningAndEnd] = Map(
    BraceHolder.method -> BracelessOpeningAndEnd(
      BracelessOpening.empty,
      BracelessEnd.empty
    )
  )
}

object BraceHolder extends Enumeration {
  type BraceHolder = Value
  val ifToken: Value = Value("if")
  val elseToken: Value = Value("else")
  val method: Value = Value("method")
  val tryToken: Value = Value("try")
  val catchToken: Value = Value("catch")
  val finallyToken: Value = Value("finally")
}

object BracelessOpening extends Enumeration {
  type BracelessOpening = Value
  val colon, empty, emptyOrColon = Value
}
object BracelessEnd extends Enumeration {
  type BracelessEnd = Value
  val empty, endIf, endElse, endTry, endCatch, endFinally, endMethod = Value

}

case class BracelessOpeningAndEnd(opening: BracelessOpening, end: BracelessEnd)
