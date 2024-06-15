package pureconfig
package generic
package scala3

import scala.deriving.Mirror

object DefaultConfigReaderDerivation extends HintsAwareConfigReaderDerivation {
  override def defaultProductHint[A]: ProductHint[A] | Null = ProductHint.default[A]
  override def defaultCoproductHint[A]: CoproductHint[A] | Null = CoproductHint.default[A]

  object syntax {
    extension (reader: ConfigReader.type) {
      inline def derived[A: Mirror.Of]: ConfigReader[A] = deriveReader
    }
  }
}
