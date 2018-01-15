package scalafix.internal.util

import scala.meta._
import scalafix.SemanticdbIndex
import scalafix.rule.RuleCtx
import scalafix.syntax._

object StatementOps {
  def litType(l: Lit): Option[Type] = l match {
    case Lit.Int(_) => Some(t"_root_.scala.Int")
    case Lit.Unit() => Some(t"_root_.scala.Unit")
    case Lit.Float(_) => Some(t"_root_.scala.Float")
    case Lit.Double(_) => Some(t"_root_.scala.Double")
    case Lit.Symbol(_) => Some(t"_root_.scala.Symbol")
    case Lit.Boolean(_) => Some(t"_root_.scala.Boolean")
    case Lit.Byte(_) => Some(t"_root_.scala.Byte")
    case Lit.Char(_) => Some(t"_root_.scala.Char")
    case Lit.Long(_) => Some(t"_root_.scala.Long")
    case Lit.Null() => Some(t"_root_.scala.Null")
    case Lit.String(_) => Some(t"_root_.scala.String")
    case Lit.Short(_) => Some(t"_root_.scala.Short")
    case _ => None
  }

  def statementType(stat: Stat)(implicit index: SemanticdbIndex): Option[Type] =
    stat match {
      case l: Lit => litType(l)
      case name: Term.Name => name.symbol.flatMap(_.resultType)
      case Term.Select(_, name) => statementType(name)
      case Term.Apply(fun, _) => statementType(fun)
      case Term.ApplyInfix(_, op, _, _) => statementType(op)
      case Term.ApplyUnary(op, _) => statementType(op)
      case _ => None
    }

  def fullStatementType(stat: Stat, ctx: RuleCtx)(
      implicit index: SemanticdbIndex): Option[Type] =
    for {
      tpe <- statementType(stat)
      (fullType, _) = TypeSyntax.prettify(tpe, ctx, false)
    } yield fullType
}
