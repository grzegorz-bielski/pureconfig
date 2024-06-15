package pureconfig
package generic
package scala3

import scala.compiletime._
import scala.deriving.Mirror

import derivation.Utils.TypeName

trait HintsAwareConfigWriterDerivation
    extends HintsAwareCoproductConfigWriterDerivation,
      HintsAwareProductConfigWriterDerivation {
  inline def getProductHint[A]: ProductHint[A] = summonInline[ProductHint[A]]
  inline def getCoproductHint[A]: CoproductHint[A] = summonInline[CoproductHint[A]]

  inline def deriveWriter[A <: AnyVal]: ConfigWriter[A] = AnyValDerivationMacros.unsafeDeriveAnyValWriter[A](this)

  inline def deriveWriter[A](using m: Mirror.Of[A]): ConfigWriter[A] =
    inline m match {
      case pm: Mirror.ProductOf[A] => deriveProductWriter[A](using pm, getProductHint[A])
      case sm: Mirror.SumOf[A] => deriveSumWriter[A](using sm, getCoproductHint[A])
    }

  private[pureconfig] inline def summonConfigWriter[A]: ConfigWriter[A] =
    summonFrom {
      case writer: ConfigWriter[A] => writer
      case given Mirror.Of[A] => deriveWriter[A]
      case _ =>
        inline if AnyValDerivationMacros.isAnyVal[A]
        then AnyValDerivationMacros.unsafeDeriveAnyValWriter[A](this)
        else error("Cannot derive ConfigWriter for: " + TypeName[A])
    }
}

object HintsAwareConfigWriterDerivation extends HintsAwareConfigWriterDerivation
