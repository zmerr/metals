package scala.meta.internal.metals.codeactions

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.util.Try

import scala.meta.Defn
import scala.meta.Pkg
import scala.meta.Template
import scala.meta.Term
import scala.meta.Tree
import scala.meta._
import scala.meta.inputs.Position
import scala.meta.internal.metals.Buffers
import scala.meta.internal.metals.CodeAction
import scala.meta.internal.metals.MetalsEnrichments._
import scala.meta.internal.parsing.Trees
import scala.meta.io.AbsolutePath
import scala.meta.pc.CancelToken

import org.eclipse.lsp4j.CodeActionParams
import org.eclipse.lsp4j.TextEdit
import org.eclipse.{lsp4j => l}

class AddingBracesCodeAction(
    trees: Trees,
    buffers: Buffers
) extends CodeAction {
  override def kind: String = l.CodeActionKind.RefactorRewrite

  override def contribute(params: CodeActionParams, token: CancelToken)(implicit
      ec: ExecutionContext
  ): Future[Seq[l.CodeAction]] = Future {
    val textDocumentIdentifier = params.getTextDocument
    val path = textDocumentIdentifier.getUri.toAbsolutePath
    val cursorRange = params.getRange
    val cursorPosition = cursorRange.getStart()
    val maybeTree =
      if (cursorRange.getStart == cursorRange.getEnd)
        trees
          .findLastEnclosingAt[Tree](
            path,
            cursorPosition,
            applyWithSingleFunction
          )
      else None

    val result =
      for {
        tree <- maybeTree
        document <- buffers.get(path)
      } yield {
        val maybeBlockHolderCodeActionIngredients
            : Option[(Tree, Term, String, String)] = tree match {
          case _: Pkg => None
          case valDefn: Defn.Val =>
            Some(valDefn, valDefn.rhs, "val definition", "")
          case varDefn: Defn.Var =>
            varDefn.rhs.map((varDefn, _, "var definition", ""))

          case defDefn: Defn.Def =>
            Some(defDefn, defDefn.body, "def definition", "")
          case termTry: Term.Try =>
            val cursorLine = cursorPosition.getLine
            val tryLine = termTry.expr.pos.toLSP.getStart.getLine
            val distanceToTry = (cursorLine - tryLine).abs
            val distanceToFinally = (cursorLine - termTry.finallyp
              .map(_.pos.toLSP.getStart.getLine)
              .getOrElse(Int.MaxValue)).abs
            Try(
              Seq(
                (
                  Some(termTry, termTry.expr, "try expression", ""),
                  distanceToTry
                ),
                (
                  termTry.finallyp.map(finallyp =>
                    (termTry, finallyp, "finally expression", "")
                  ),
                  distanceToFinally
                )
              ).minBy(_._2)
            )
              .map(_._1)
              .toOption
              .flatten

          case termIf: Term.If =>
            val cursorLine = cursorPosition.getLine
            val tryLine = termIf.thenp.pos.toLSP.getStart.getLine
            val distanceToThen = (cursorLine - tryLine).abs
            val distanceToElseP = (cursorLine - Try(
              termIf.elsep.pos.toLSP.getStart.getLine
            ).getOrElse(Int.MaxValue)).abs
            Try(
              Seq(
                (
                  (termIf, termIf.thenp, "then expression", "then"),
                  distanceToThen
                ),
                ((termIf, termIf.elsep, "else expression", ""), distanceToElseP)
              )
                .minBy(_._2)
            ).map(_._1).toOption

          case termFor: Term.For =>
            Some(termFor, termFor.body, "for expression", "")
          case termForYield: Term.ForYield =>
            Some(termForYield, termForYield.body, "yield expression", "")
          case termWhile: Term.While =>
            Some(termWhile, termWhile.body, "while expression", "do")
          case _: Defn.GivenAlias => None
          case termBlock: Term.Block if !termBlock.parent.exists(_ match {
                case _: Term.Apply => true
                case _ => false
              }) =>
            termBlock.parent.flatMap { blockHolder =>
              Some(blockHolder, termBlock, "block", "")
            }

          case _ => None
        }
        maybeBlockHolderCodeActionIngredients
          .flatMap { ingredients =>
            codeActionForBlockHolder(
              ingredients._1,
              path,
              document,
              ingredients._2,
              ingredients._3,
              ingredients._4
            )
          }
          .orElse(
            tree match {
              case template: Template =>
                val title = template.parent
                  .collectFirst {
                    case _: Defn.Enum => "enum definition"
                    case _: Defn.Trait => "trait definition"
                    case _: Defn.Object => "object definition"
                    case _: Defn.Class => "class definition"
                  }
                  .getOrElse("template")
                template.parent.flatMap {
                  codeActionForTemplateHolder(
                    _,
                    path,
                    document,
                    template,
                    title
                  )
                }
              case _ => None
            }
          )
      }
    result.toSeq.flatten
  }

  /**
   * stats holder trees are `Term.Block` and `Template`
   */
  private def isStatsHolderBraced(tree: Tree, stats: List[Stat]): Boolean = {
    util
      .Try(stats.maxBy(_.pos.end).pos.end)
      .getOrElse(-1) != tree.pos.end && util
      .Try(tree.tokens.maxBy(_.pos.end).text)
      .getOrElse("") == "}"
  }

  /**
   * @param blockEmbraceable the `rhs` of [[Defn.Var]] or [[Defn.Val]];
   *                         or the `body` of [[Defn.Def]]
   * @return whether the `rhs` or `body` passed as the
   *         argument has braces
   */
  private def isBlockEmbraceableBraced(blockEmbraceable: Term): Boolean = {
    blockEmbraceable match {
      case termBlock: Term.Block =>
        isStatsHolderBraced(termBlock, termBlock.stats)
      case _ => false
    }
  }

  private def addBracesToBlockHolderAction(
      blockHolder: Tree,
      path: AbsolutePath,
      document: String,
      blockEmbraceable: Term,
      codeActionSubjectTitle: String,
      indentation: String,
      bracelessStart: String
  ) = {

    for {

      bracelessStartToken <- util
        .Try(blockEmbraceable.tokens.minBy(_.pos.start))
        .toOption
        .map(_.pos.start)
        .flatMap(blockEmbraceableStartPos =>
          util
            .Try(
              blockHolder.tokens.tokens
                .filter { token =>
                  token.pos.start < blockEmbraceableStartPos && !token.isWhiteSpaceOrComment
                }
                .maxBy(_.pos.start)
            )
            .toOption
        )
      bracePose =
        if (
          bracelessStartToken.text == bracelessStart && bracelessStart.length > 0
        )
          bracelessStartToken.pos.toLSP.getStart
        else bracelessStartToken.pos.toLSP.getEnd

    } yield addBracesAction(
      path,
      expectedBraceStartPos = bracePose,
      expectedBraceEndPose = blockEmbraceable.pos.toLSP.getEnd,
      bracelessStart = bracelessStart,
      bracelessEnd = "",
      indentation = indentation,
      document = document,
      codeActionSubjectTitle = codeActionSubjectTitle,
      maybeEndMarkerPos = maybeGetEndMarkerPos(blockHolder)
    )
  }

  /**
   * @param blockHolder      the trees which can only be braced if the type of a branch of
   *                         them is `Term.Block` and that block is braced.
   * @param path             the path to the file containing the tree. It is used in
   *                         contstructing the TextEdit of the code action
   * @param document
   * @param blockEmbraceable the branch of the main tree which can potentially have
   *                         the type `Term.Block`; so can be braced.
   * @param codeActionSubjectTitle
   * @return
   */
  private def codeActionForBlockHolder(
      blockHolder: Tree,
      path: AbsolutePath,
      document: String,
      blockEmbraceable: Term,
      codeActionSubjectTitle: String,
      bracelessStart: String = ""
  ): Option[l.CodeAction] = {
    val indentation =
      getIndentationForPositionInDocument(blockHolder.pos, document)

    if (!isBlockEmbraceableBraced(blockEmbraceable)) {
      addBracesToBlockHolderAction(
        blockHolder,
        path,
        document,
        blockEmbraceable,
        codeActionSubjectTitle,
        indentation,
        bracelessStart
      )
    } else None
  }

  private def maybeGetEndMarkerPos(tree: Tree): Option[Position] =
    tree.parent
      .flatMap(parent =>
        parent.children.dropWhile(_ != tree).tail.headOption.collectFirst {
          case endMarker: Term.EndMarker => endMarker
        }
      )
      .map(_.pos)

  private def codeActionForTemplateHolder(
      templateHolder: Tree,
      path: AbsolutePath,
      document: String,
      templ: Template,
      codeActionSubjectTitle: String
  ): Option[l.CodeAction] = {
    val indentation =
      getIndentationForPositionInDocument(templateHolder.pos, document)
    val expectedBraceStartPos = util
      .Try {
        val lastInit = templ.inits.maxBy(init => init.pos.end)
        lastInit.pos.end
      }
      .getOrElse(templ.pos.start)
    if (!isStatsHolderBraced(templ, templ.stats)) {
      addBracesToTemplateHolderAction(
        templateHolder,
        path,
        document,
        templ,
        codeActionSubjectTitle,
        expectedBraceStartPos,
        indentation
      )
    } else None
  }

  private def addBracesToTemplateHolderAction(
      templateHolder: Tree,
      path: AbsolutePath,
      document: String,
      templ: Template,
      codeActionSubjectTitle: String,
      expectedBraceStartPos: Int,
      indentation: String
  ): Option[l.CodeAction] = {

    for {
      colonPose <- templ.tokens
        .find(token =>
          token.text == ":" && token.pos.start >= expectedBraceStartPos
        )
        .map(_.pos.toLSP.getStart)
    } yield addBracesAction(
      path,
      expectedBraceStartPos = colonPose,
      expectedBraceEndPose = templ.pos.toLSP.getEnd,
      bracelessStart = ":",
      bracelessEnd = "",
      indentation = indentation,
      document = document,
      codeActionSubjectTitle = codeActionSubjectTitle,
      maybeEndMarkerPos = maybeGetEndMarkerPos(templateHolder)
    )
  }

  private def addBracesAction(
      path: AbsolutePath,
      indentation: String,
      document: String,
      expectedBraceStartPos: l.Position,
      expectedBraceEndPose: l.Position,
      bracelessStart: String,
      bracelessEnd: String,
      codeActionSubjectTitle: String,
      maybeEndMarkerPos: Option[Position]
  ): l.CodeAction = {
    val braceableBranchStart = expectedBraceStartPos
    val braceableBranchStartEnd = new l.Position()
    braceableBranchStartEnd.setCharacter(
      expectedBraceStartPos.getCharacter + bracelessStart.length
    )
    braceableBranchStartEnd.setLine(expectedBraceStartPos.getLine)
    val startBraceTextEdit = new TextEdit(
      new l.Range(braceableBranchStart, braceableBranchStartEnd),
      "{"
    )

    val braceableBranchEndStart = expectedBraceEndPose
    val braceableBranchEnd = new l.Position()
    braceableBranchEnd.setCharacter(
      expectedBraceEndPose.getCharacter + bracelessEnd.length
    )
    braceableBranchEnd.setLine(expectedBraceEndPose.getLine)

    val endBraceTextEdit = new TextEdit(
      new l.Range(braceableBranchEndStart, braceableBranchEnd),
      s"""|
          |$indentation}""".stripMargin
    )
    val maybeEndMarkerEraserTextEdit = maybeEndMarkerPos.map { endMarkerPos =>
      new TextEdit(
        new l.Range(endMarkerPos.toLSP.getStart, endMarkerPos.toLSP.getEnd),
        ""
      )
    }.toList

    val codeAction = new l.CodeAction()
    codeAction.setTitle(
      AddingBracesCodeAction.goBraceFul(codeActionSubjectTitle)
    )
    codeAction.setKind(this.kind)
    codeAction.setEdit(
      new l.WorkspaceEdit(
        Map(
          path.toURI.toString -> (List(
            startBraceTextEdit,
            endBraceTextEdit
          ) ++ maybeEndMarkerEraserTextEdit).asJava
        ).asJava
      )
    )
    codeAction
  }

  private def getIndentationForPositionInDocument(
      treePos: Position,
      document: String
  ): String =
    document
      .substring(treePos.start - treePos.startColumn, treePos.start)
      .takeWhile(_.isWhitespace)

  def applyWithSingleFunction: Tree => Boolean = {
    case _: Pkg | _: Defn.Class | _: Defn.Enum | _: Defn.Trait |
        _: Defn.Object =>
      true
    case _: Defn.GivenAlias | _: Defn.Val | _: Defn.Var | _: Defn.Def => true
    case _: Term.Try | _: Term.If | _: Term.For | _: Term.Match |
        _: Term.While =>
      true
    case _: Template | _: Term.Block => true
    case _ => false
  }

}

object AddingBracesCodeAction {

  def goBraceFul(subject: String): String = s"Add braces to the $subject"

}
