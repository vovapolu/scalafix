package scalafix.internal.rule

import metaconfig.{Conf, Configured}

import scala.meta._
import scalafix.{LintMessage, Rule}
import scalafix.internal.config.DisableSelectConfig
import scalafix.internal.util.{StatementOps, SymbolOps}
import scalafix.lint.LintCategory
import scalafix.rule.{RuleCtx, SemanticRule}
import scalafix.util.{SemanticdbIndex, SymbolMatcher}
import scalafix.syntax._

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

    // A[B] -> A
    def tryToMakeSimpler(tpe: Type): Type = {
      tpe match {
         case Type.Apply(t, _) => t
         case _ => tpe
      }
    }

    def globalSymbol(index: SemanticdbIndex, t: Tree): Option[Symbol.Global] =
      index.symbol(t) match {
        case Some(s: Symbol.Global) => Some(s)
        case _ => None
      }

    ctx.tree.collect {
      case Term.Select(qual, name) =>
        val f = StatementOps.fullStatementType(qual, ctx)
        val sf = f.map(tryToMakeSimpler)
        println(f)
        println(sf)
        println(qual.symbol.flatMap(_.resultType))
        println(sf.map(disabledQualTypes.matches))
        println("------")
        for {
          fullQualType <- StatementOps.fullStatementType(qual, ctx)
          simpleType = tryToMakeSimpler(fullQualType)
          qualSymbol <- globalSymbol(ctx.index, simpleType)
          if disabledQualTypes.matches(qualSymbol)
        } yield
          errorCategory
            .copy(id = name.toString)
            .at(s"$name is blocked", name.pos)
    }.flatten
  }
}
