package scalafix.internal.rule

import metaconfig.{Conf, Configured}

import scala.meta._
import scalafix.{LintMessage, Rule}
import scalafix.internal.config.DisableSelectConfig
import scalafix.internal.util.{StatementOps, SymbolOps}
import scalafix.lint.LintCategory
import scalafix.rule.{RuleCtx, SemanticRule}
import scalafix.util.{SemanticdbIndex, SymbolMatcher}

final case class DisableSelect(
    index: SemanticdbIndex,
    config: DisableSelectConfig)
    extends SemanticRule(index, "DisableSelect") {

  private lazy val errorCategory: LintCategory =
    LintCategory.error(
      """Disable selects of specific methods/operators for objects of given type""".stripMargin
    )

  override def description: String =
    "Linter that reports an error on a configurable set of keywords and syntax."

  override def init(config: Conf): Configured[Rule] =
    config
      .getOrElse("disableSelect", "DisableSelect")(DisableSelectConfig.default)
      .map(DisableSelect(index, _))

  private lazy val disabledQualTypes: SymbolMatcher =
    SymbolMatcher.normalized(config.allQualTypes: _*)

  override def check(ctx: RuleCtx): Seq[LintMessage] = {

    def treeIsBlocked(
        tree: Tree,
        blockedSymbols: List[Symbol.Global]): Boolean =
      ctx.index.symbol(tree) match {
        case Some(s: Symbol.Global) =>
          blockedSymbols.exists(SymbolOps.isSameNormalized(_, s))
        case _ => false
      }

    def globalSymbol(index: SemanticdbIndex, t: Tree): Option[Symbol.Global] =
      index.symbol(t) match {
        case Some(s: Symbol.Global) => Some(s)
        case _ => None
      }

    ctx.tree.collect {
      case Term.Select(qual, name) =>
        for {
          fullQualType <- StatementOps.fullStatementType(qual, ctx)
          qualSymbol <- globalSymbol(ctx.index, qual)
          if disabledQualTypes.matches(fullQualType) && treeIsBlocked(
            name,
            config.namesForQual(qualSymbol))
        } yield
          errorCategory
            .copy(id = name.toString)
            .at(s"$name is blocked", name.pos)
    }.flatten
  }
}
