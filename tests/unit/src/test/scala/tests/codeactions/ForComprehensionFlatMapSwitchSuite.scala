package tests.codeactions

import scala.meta.internal.metals.codeactions.ForComprehensionFlatMapSwitchCodeAction
import scala.meta.internal.metals.codeactions.RewriteBracesParensCodeAction

class ForComprehensionFlatMapSwitchSuite
    extends BaseCodeActionLspSuite("forComprehension") {

  check(
    "two-flatMaps-to-forComprehension",
    """|object A {
       |  val a: List[Int] = List(1, 2)
       |
       |  val b: Option[String] = Some("great")
       |
       |  val c = a.fl<<>>atMap { aMember =>
       |                        b.map( bMember =>
       |                            s"$aMember $bMember"
       |                              )
       |                     }
       |
       |}
       |""".stripMargin,
    s"""|${RewriteBracesParensCodeAction.toParens}
        |${ForComprehensionFlatMapSwitchCodeAction.flatMapToForComprehension}
        |""".stripMargin,
    """|object A {
       |  val a: List[Int] = List(1, 2)
       |
       |  val b: Option[String] = Some("great")
       |
       |  val c = for {
       |               aMember <- a
       |               bMember <- b
       |             } yield {
       |               s"$aMember $bMember"
       |             }
       |
       |}
       |""".stripMargin,
    selectedActionIndex = 1
  )
}
