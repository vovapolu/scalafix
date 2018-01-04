package scalafix
package rule

import scalafix.internal.rule._
import scalafix.internal.config._

object ScalafixRules {
  val syntax: List[Rule] = List(
    ProcedureSyntax,
    DottyVolatileLazyVal,
    RemoveXmlLiterals,
    ExplicitUnit,
    NoValInForComprehension,
    NoFinalize,
    DottyKeywords,
    DottyVarArgPattern,
    DisableSyntax(),
    FixSyntax()
  )
  def semantic(index: SemanticdbIndex): List[Rule] = List(
    NoInfer(index, NoInferConfig.default),
    Sbt1(index),
    ExplicitResultTypes(index),
    RemoveUnusedImports(index),
    RemoveUnusedTerms(index),
    NoAutoTupling(index),
    Disable(index, DisableConfig.default),
    LeakingSealed(index)
  )
  def all(index: SemanticdbIndex): List[Rule] =
    syntax ++ semantic(index)
  def name2rule(index: SemanticdbIndex): Map[String, Rule] =
    all(index).flatMap(x => x.allNames.map(_ -> x)).toMap
  lazy val syntaxName2rule: Map[String, Rule] =
    syntax.flatMap(x => x.allNames.map(_ -> x)).toMap
  lazy val syntacticNames: List[String] = syntaxName2rule.keys.toList
  lazy val semanticNames: List[String] =
    semantic(SemanticdbIndex.empty).flatMap(_.allNames)
  def allNames: List[String] = syntacticNames ++ semanticNames

  lazy val syntacticNamesDescriptions: List[(String, String)] =
    syntaxName2rule.toList.map { case (name, rule) => (name, rule.description) }
  lazy val semanticNamesDescriptions: List[(String, String)] =
    semantic(SemanticdbIndex.empty).flatMap(rule =>
      rule.allNames.map(name => (name, rule.description)))
  def allNamesDescriptions: List[(String, String)] =
    syntacticNamesDescriptions ++ semanticNamesDescriptions

}
