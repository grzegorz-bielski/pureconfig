package pureconfig

import scala.language.higherKinds

import com.typesafe.config.ConfigFactory

import pureconfig._
import pureconfig.error._
import pureconfig.generic._
import pureconfig.generic.derivation.default.derived

enum AnimalConfig derives ConfigReader {
  case DogConfig(age: Int)
  case CatConfig(age: Int)
  case BirdConfig(canFly: Boolean)
}

class CoproductReaderDerivationSuite extends BaseSuite {

  import AnimalConfig._

  behavior of "ConfigConvert"

  it should "read disambiguation information on sealed families by default" in {
    val conf = ConfigFactory.parseString("{ type = dog-config, age = 2 }")
    ConfigReader[AnimalConfig].from(conf.root()) shouldEqual Right(DogConfig(2))
  }

  it should "return a proper ConfigReaderFailure if the hint field in a coproduct is missing" in {
    val conf = ConfigFactory.parseString("{ can-fly = true }")
    ConfigReader[AnimalConfig].from(conf.root()) should failWithReason[KeyNotFound]
  }

  it should "return a proper ConfigReaderFailure if the hint field in a coproduct contains an invalid option" in {
    val conf = ConfigFactory.parseString("{ can-fly = true, type = car-config }")
    ConfigReader[AnimalConfig].from(conf.root()) should failWith(
      CannotConvert("car-config", "AnimalConfig", "The value is not a valid option.")
    )
  }

  it should "return a proper ConfigReaderFailure when a coproduct config is missing" in {
    case class AnimalCage(animal: AnimalConfig) derives ConfigReader
    ConfigReader[AnimalCage].from(ConfigFactory.empty().root()) should failWithReason[KeyNotFound]
  }

  it should "invoke defaults when a key is not in the configuration" in {
    enum ConfigDefaults derives ConfigReader {
      case A(age: Int, name: String = "Rex")
      case B(canFly: Boolean, name: String = "Tweety", c: List[Int] = List(1, 2, 3))
    }

    val confA = ConfigFactory.parseString("{ type = a, age = 2 }")
    ConfigReader[ConfigDefaults].from(confA.root()) shouldEqual Right(ConfigDefaults.A(age = 2, name = "Rex"))

    val confB = ConfigFactory.parseString("{ type = b, can-fly = true }")
    ConfigReader[ConfigDefaults].from(confB.root()) shouldEqual Right(
      ConfigDefaults.B(canFly = true, name = "Tweety", c = List(1, 2, 3))
    )
  }
}
