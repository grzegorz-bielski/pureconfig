package pureconfig
package generic
package scala3

import scala.compiletime._
import scala.deriving.Mirror

import derivation.Utils.TypeName

trait HintsAwareConfigReaderDerivation
    extends HintsAwareCoproductConfigReaderDerivation,
      HintsAwareProductConfigReaderDerivation {
  def defaultProductHint[A]: ProductHint[A] | Null = null
  inline def getProductHint[A]: ProductHint[A] = defaultProductHint[A] match {
    case null => summonInline[ProductHint[A]]
    case hint => hint
  }

  def defaultCoproductHint[A]: CoproductHint[A] | Null = null
  inline def getCoproductHint[A]: CoproductHint[A] = defaultCoproductHint[A] match {
    case null => summonInline[CoproductHint[A]]
    case hint => hint
  }

  inline def deriveReader[A <: AnyVal]: ConfigReader[A] = AnyValDerivationMacros.unsafeDeriveAnyValReader[A](this)

  inline def deriveReader[A](using m: Mirror.Of[A]): ConfigReader[A] =
    inline m match {
      case pm: Mirror.ProductOf[A] =>
        deriveProductReader[A](using pm, getProductHint[A])
      case sm: Mirror.SumOf[A] =>
        deriveSumReader[A](using sm, getCoproductHint[A])
    }

  private[pureconfig] inline def summonConfigReader[A]: ConfigReader[A] =
    summonFrom {
      case reader: ConfigReader[A] => reader
      case given Mirror.Of[A] => deriveReader[A]
      case _ =>
        inline if AnyValDerivationMacros.isAnyVal[A]
        then AnyValDerivationMacros.unsafeDeriveAnyValReader[A](this)
        else error("Cannot derive ConfigReader for " + TypeName[A])
    }

}

object HintsAwareConfigReaderDerivation extends HintsAwareConfigReaderDerivation
