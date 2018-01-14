package scalafix.internal.rule

import scala.meta.Term.Block
import scala.meta._
import scalafix.SemanticdbIndex
import scalafix.internal.util.{StatementOps, TypeSyntax}
import scalafix.lint.{LintCategory, LintMessage}
import scalafix.rule.{RuleCtx, RuleName, SemanticRule}
import scalafix.syntax._
import scala.meta.contrib.implicits.Equality._

case class DiscardedValues(index: SemanticdbIndex)
    extends SemanticRule(index, RuleName("DiscardedValues")) {

  override def description: String =
    "This rule ensures that statements should only return Unit"

  private lazy val errorCategory: LintCategory =
    LintCategory.error("Statements should only return Unit")

  override def check(ctx: RuleCtx): List[LintMessage] = {
    def statsToErrors(stats: List[Stat]): List[LintMessage] =
      for {
        stat <- stats
        fullType <- StatementOps.fullStatementType(stat, ctx)
        if !fullType.isEqual(t"_root_.scala.Unit")
      } yield {
        errorCategory
          .copy(id = fullType.toString)
          .at(s"Type $fullType is not Unit", stat.pos)
      }

    def actualStats(defName: Term.Name, stats: List[Stat]): List[Stat] = {
      val fullTypeOpt = defName.symbol
        .flatMap(t => t.resultType)
        .map(TypeSyntax.prettify(_, ctx, false))
      fullTypeOpt match {
        case Some((tpe, _)) if tpe.isEqual(t"_root_.scala.Unit") => stats
        case _ => stats.dropRight(1)
      }
    }

    ctx.tree.collect {
      case Template(_, _, _, stats) => statsToErrors(stats)
      case Defn.Val(_, Pat.Var(name) :: Nil, _, Block(stats)) =>
        statsToErrors(actualStats(name, stats))
      case Defn.Var(_, Pat.Var(name) :: Nil, _, Some(Block(stats))) =>
        statsToErrors(actualStats(name, stats))
      case Defn.Def(_, name, _, _, _, Block(stats)) =>
        statsToErrors(actualStats(name, stats))
    }.flatten
  }
}
