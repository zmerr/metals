package scala.meta.internal.metals.codeactions

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

import scala.meta.Term
import scala.meta.Tree
import scala.meta.inputs.Position
import scala.meta.internal.metals.Buffers
import scala.meta.internal.metals.CodeAction
import scala.meta.internal.metals.MetalsEnrichments._
import scala.meta.internal.parsing.Trees
import scala.meta.io.AbsolutePath
import scala.meta.pc.CancelToken

import org.eclipse.lsp4j.CodeActionParams
import org.eclipse.{lsp4j => l}

class ForComprehensionFlatMapSwitchCodeAction(
    trees: Trees,
    buffers: Buffers
) extends CodeAction {
  override def kind: String = l.CodeActionKind.RefactorRewrite

  override def contribute(params: CodeActionParams, token: CancelToken)(implicit
      ec: ExecutionContext
  ): Future[Seq[l.CodeAction]] = Future {
    val uri = params.getTextDocument().getUri()

    val path = uri.toAbsolutePath
    val range = params.getRange()
    val maybeTree =
      if (range.getStart == range.getEnd)
        trees
          .findLastEnclosingAt[Tree](
            path,
            range.getStart(),
            applyWithSingleFunction
          )
      else
        None

    val maybeCodeAction = for {
      document <- buffers.get(path)
      applyTree <- maybeTree
      indentation = getIndentationForPosition(applyTree.pos)
    } yield {
      pprint.log("inside yield")
      applyTree match {
        case forYeild: Term.ForYield =>
          Some(
            buildToFlatMapCodeAction(
              path,
              forYeild,
              document,
              uri,
              indentation
            )
          )
        case termApply: Term.Apply =>
          pprint.log("matched on Apply")
          maybeBuildToForYieldCodeActionWithApply(
            path,
            termApply,
            document,
            indentation
          )
        //      case termSelect: Term.Select
        //          if termSelect.name.value == "flatMap" || termSelect.name.value == "map" =>
        //        maybeBuildToForYieldCodeActionWithSelect(
        //          path,
        //          termSelect,
        //          document,
        //          indentation
        //        )
        case termName: Term.Name
            if termName.value == "flatMap" || termName.value == "map" =>
          pprint.log("matched on Name")
          maybeBuildToForYieldCodeActionWithName(
            path,
            termName,
            document,
            indentation
          )
        case _ => None
      }
    }

    maybeCodeAction.flatten.toSeq

  }

  private def buildToFlatMapCodeAction(
      path: AbsolutePath,
      forComprehension: Term.ForYield,
      document: String,
      uri: String,
      str: String
  ): l.CodeAction = ???

  private def maybeBuildToForYieldCodeActionWithApply(
      path: AbsolutePath,
      termApply: Term.Apply,
      document: String,
      indentation: String
  ): Option[l.CodeAction] = {
    val (nameQualsList, yieldString) =
      extractForYield(List.empty, termApply, document)
    if (nameQualsList.nonEmpty) {
      val forYieldString =
        s"""|for {
            |${nameQualsList.map(nameQual => s"${indentation}  ${nameQual._1} <- ${nameQual._2}").mkString("\n")}
            |${indentation}} yield {
            |${indentation}  ${yieldString}
            |${indentation}}""".stripMargin

      val codeAction = new l.CodeAction()
      val range =
        new l.Range(termApply.pos.toLSP.getStart, termApply.pos.toLSP.getEnd)
      codeAction.setTitle(
        ForComprehensionFlatMapSwitchCodeAction.flatMapToForComprehension
      )
      codeAction.setKind(this.kind)
      val forComprehensionTextEdit = new l.TextEdit(range, forYieldString)
      codeAction.setEdit(
        new l.WorkspaceEdit(
          Map(
            path.toURI.toString -> List(forComprehensionTextEdit).asJava
          ).asJava
        )
      )
      Some(codeAction)
    } else None

  }

  private def maybeBuildToForYieldCodeActionWithSelect(
      path: AbsolutePath,
      termSelect: Term.Select,
      document: String,
      indentation: String
  ): Option[l.CodeAction] = {
    pprint.log("term select parent is: \n" + termSelect.parent)
    termSelect.parent.collect { case termApply: Term.Apply =>
      pprint.log("inside termApply Parent")
      maybeBuildToForYieldCodeActionWithApply(
        path,
        termApply,
        document,
        indentation
      )
    }.flatten
  }

  private def maybeBuildToForYieldCodeActionWithName(
      path: AbsolutePath,
      termName: Term.Name,
      document: String,
      indentation: String
  ): Option[l.CodeAction] = {
    pprint.log("parent is \n" + termName.parent)
    termName.parent.collect { case termSelect: Term.Select =>
      pprint.log("inside term Select parent")
      maybeBuildToForYieldCodeActionWithSelect(
        path,
        termSelect,
        document,
        indentation
      )
    }.flatten

  }

  private def extractForYield(
      existingNameQuals: List[(String, String)],
      termApply: Term.Apply,
      document: String
  ): (List[(String, String)], String) = {
    termApply.fun match {
      case termSelect: Term.Select
          if termSelect.name.value == "flatMap" || termSelect.name.value == "map" =>
        pprint.log("inside termApply termSelect")
        val qual = termSelect.qual
        val qualString = document.substring(qual.pos.start, qual.pos.end)
        termApply.args.head match {
          case termFunction: Term.Function =>
            extractForYieldOutOfFunction(
              existingNameQuals,
              termFunction,
              qualString,
              document
            )

          case termBlock: Term.Block =>
            termBlock.stats.collect { case termFunction: Term.Function =>
              extractForYieldOutOfFunction(
                existingNameQuals,
                termFunction,
                qualString,
                document
              )
            }.head

          case _ =>
            (
              existingNameQuals,
              document.substring(termApply.pos.start, termApply.pos.end)
            )
        }
      case _ =>
        (
          existingNameQuals,
          document.substring(termApply.pos.start, termApply.pos.end)
        )
    }
  }

  /**
   * @param existingNameQuals the Name Qual tuples which are extracted from the higher levels of the hierarchy
   * @param termFunction the function which is passed as the main argument of `.map` or `.flatMap`
   * @param qualString the left hand side of each enumeration item in the for yield expression
   * @param document the text string of the whole file
   * @return
   */
  private def extractForYieldOutOfFunction(
      existingNameQuals: List[(String, String)],
      termFunction: Term.Function,
      qualString: String,
      document: String
  ): (List[(String, String)], String) = {
    val paramName =
      termFunction.params.headOption.map(_.name.value).getOrElse("")
    val body = termFunction.body
    body match {
      case bodyTermApply: Term.Apply =>
        extractForYield(
          existingNameQuals :+ (paramName, qualString),
          bodyTermApply,
          document
        )
      case otherBody =>
        (
          existingNameQuals :+ (paramName, qualString),
          document.substring(otherBody.pos.start, otherBody.pos.end)
        )
    }
  }

  private def getIndentationForPosition(
      treePos: Position
  ): String = {
    var result = ""
    for (i <- 0 to treePos.startColumn) {
      result = result + " "
    }
    result
    //   """              """
    // (0 to treePos.startColumn + 1 ).map(" ").mkString("")
  }

  private def applyWithSingleFunction: Tree => Boolean = {
    case _: Term.ForYield => true
    case _: Term.Apply => true
    case _: Term.Select => true
    case _: Term.Name => true
    case _ => false
  }
}

object ForComprehensionFlatMapSwitchCodeAction {
  val flatMapToForComprehension = "Turn into for comprehension"
}
