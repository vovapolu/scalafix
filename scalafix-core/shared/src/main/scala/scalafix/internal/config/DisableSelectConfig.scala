package scalafix.internal.config

import scala.meta._
import metaconfig.{Conf, ConfDecoder, ConfError, Configured}

import scalafix.internal.config.MetaconfigPendingUpstream.XtensionConfScalafix
import scalafix.internal.util.SymbolOps

case class SelectConfig(
    qualType: Symbol.Global,
    name: Symbol.Global,
    message: Option[String])

object SelectConfig {
  implicit val decoder: ConfDecoder[SelectConfig] =
    ConfDecoder.instanceF[SelectConfig] {
      case c: Conf.Obj =>
        (c.get[Symbol.Global]("qualType") |@|
          c.get[Symbol.Global]("name") |@|
          c.getOption[String]("message")).map {
          case ((a, b), c) => SelectConfig(a, b, c)
        }
      case _ => Configured.NotOk(ConfError.msg("Wrong config format"))
    }
}

case class DisableSelectConfig(symbols: List[SelectConfig] = Nil) {
  def allQualTypes: List[Symbol.Global] = symbols.map(_.qualType)

  private val messageByQualAndName: Map[(String, String), String] =
    (for {
      u <- symbols
      message <- u.message
    } yield {
      (
        SymbolOps.normalize(u.qualType).syntax,
        SymbolOps.normalize(u.name).syntax
      ) -> message
    }).toMap

  private val namesByQual: Map[String, List[Symbol.Global]] =
    symbols
      .map(u => SymbolOps.normalize(u.qualType).syntax -> u.name)
      .groupBy(_._1)
      .mapValues(_.map(_._2))

  def namesForQual(qualType: Symbol.Global): List[Symbol.Global] =
    namesByQual.getOrElse(SymbolOps.normalize(qualType).syntax, List.empty)

  def customMessage(
      qualType: Symbol.Global,
      name: Symbol.Global): Option[String] =
    messageByQualAndName.get(
      (SymbolOps.normalize(qualType).syntax, SymbolOps.normalize(name).syntax)
    )

  implicit val reader: ConfDecoder[DisableSelectConfig] =
    ConfDecoder.instanceF[DisableSelectConfig](
      _.getField(symbols).map(DisableSelectConfig(_))
    )
}

object DisableSelectConfig {
  lazy val default = DisableSelectConfig()
  implicit val reader: ConfDecoder[DisableSelectConfig] = default.reader
}
