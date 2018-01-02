package scalafix.internal.rule

import scala.meta._
import scala.meta.tokens.Token.Comment
import metaconfig.{Conf, Configured}
import scalafix.rule.{Rule, RuleCtx}
import scalafix.lint.LintMessage
import scalafix.lint.LintCategory
import scalafix.util.SymbolMatcher
import scalafix.internal.util.IntervalSet
import scalafix.internal.config.{DisableSyntaxConfig, Keyword}
import scalafix.syntax._

final case class DisableSyntax(
    config: DisableSyntaxConfig = DisableSyntaxConfig())
    extends Rule("DisableSyntax")
    with Product {

  override def description: String =
    "Linter that reports an error on a configurable set of keywords and syntax."

  override def init(config: Conf): Configured[Rule] =
    config
      .getOrElse("disableSyntax", "DisableSyntax")(DisableSyntaxConfig.default)
      .map(DisableSyntax(_))

  private def checkRegex(ctx: RuleCtx): Seq[LintMessage] = {
    def pos(offset: Int): Position =
      Position.Range(ctx.input, offset, offset)
    val regexLintMessages = Seq.newBuilder[LintMessage]
    config.regex.foreach { regex =>
      val matcher = regex.value.matcher(ctx.input.chars)
      val pattern = regex.value.pattern
      val message = regex.message.getOrElse(s"$pattern is disabled")
      while (matcher.find()) {
        regexLintMessages +=
          errorCategory
            .copy(id = pattern)
            .at(message, pos(matcher.start))
      }
    }

    regexLintMessages.result()
  }

  private def checkTokens(ctx: RuleCtx): Seq[LintMessage] = {
    ctx.tree.tokens.collect {
      case token @ Keyword(keyword) if config.isDisabled(keyword) =>
        errorCategory
          .copy(id = s"keywords.$keyword")
          .at(s"$keyword is disabled", token.pos)
      case token @ Token.Semicolon() if config.noSemicolons =>
        error("noSemicolons", token)
      case token @ Token.Tab() if config.noTabs =>
        error("noTabs", token)
      case token @ Token.Xml.Start() if config.noXml =>
        error("noXml", token)
    }
  }

  private def checkTree(ctx: RuleCtx): Seq[LintMessage] = {
    object AbstractWithVals {
      def unapply(t: Tree): Option[List[Defn.Val]] = {
        val stats = t match {
          case Defn.Class(mods, _, _, _, templ)
              if mods.exists(_.is[Mod.Abstract]) =>
            templ.stats
          case Defn.Trait(_, _, _, _, templ) => templ.stats
          case _ => List.empty
        }
        val vals = stats.flatMap {
          case v: Defn.Val => Some(v)
          case _ => None
        }
        if (vals.isEmpty) None else Some(vals)
      }
    }

    object WithMethods {
      def unapply(t: Tree): Option[List[Defn.Def]] = {
        val stats = t match {
          case Defn.Class(_, _, _, _, templ) => templ.stats
          case Defn.Trait(_, _, _, _, templ) => templ.stats
          case Term.NewAnonymous(templ) => templ.stats
          case _ => List.empty
        }
        val methods = stats.flatMap {
          case d: Defn.Def => Some(d)
          case _ => None
        }
        if (methods.isEmpty) None else Some(methods)
      }
    }

    def hasDefaultArgs(d: Defn.Def): Boolean =
      d.paramss.exists(_.exists(_.default.isDefined))

    ctx.tree.collect {
      case t @ mod"+" if config.noVariantTypes =>
        Seq(
          errorCategory
            .copy(id = "covariant")
            .at(
              "Covariant types could lead to error-prone situations.",
              t.pos
            )
        )
      case t @ mod"-" if config.noVariantTypes =>
        Seq(
          errorCategory
            .copy(id = "contravariant")
            .at(
              "Contravariant types could lead to error-prone situations.",
              t.pos
            )
        )
      case t @ WithMethods(methods)
          if methods.exists(hasDefaultArgs) && config.noDefaultArgs =>
        methods
          .filter(hasDefaultArgs)
          .map(
            m =>
              errorCategory
                .copy(id = "defaultArgs")
                .at(
                  "Default args makes it hard to use methods as functions.",
                  m.pos))
      case t @ AbstractWithVals(vals) if config.noValInAbstract =>
        vals.map(
          v =>
            errorCategory
              .copy(id = "valInAbstract")
              .at(
                "val definitions in traits/abstract classes may cause initialization bugs",
                v.pos))
      case t @ Defn.Object(mods, _, _)
          if mods.exists(_.is[Mod.Implicit]) && config.noImplicitObject =>
        Seq(
          errorCategory
            .copy(id = "implicitObject")
            .at("implicit objects may cause implicit resolution errors", t.pos)
        )
    }.flatten
  }

  override def check(ctx: RuleCtx): Seq[LintMessage] = {
    checkTree(ctx) ++ checkTokens(ctx) ++ checkRegex(ctx)
  }

  private val errorCategory: LintCategory =
    LintCategory.error(
      "Some constructs are unsafe to use and should be avoided")

  private def error(keyword: String, token: Token): LintMessage =
    errorCategory.copy(id = keyword).at(s"$keyword is disabled", token.pos)
}
