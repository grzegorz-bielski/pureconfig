# Enumeratum module for PureConfig

Adds support for enums generated by [Enumeratum](https://github.com/lloydmeta/enumeratum) library to PureConfig.

## Why

Automatically create a converters to read [Enumeratum](https://github.com/lloydmeta/enumeratum) enums from a configuration.

## Add pureconfig-enumeratum to your project

In addition to [core pureconfig](https://github.com/melrief/pureconfig), you'll need:

```scala
libraryDependencies += "com.github.melrief" %% "pureconfig-enumeratum" % "0.6.0"
```

## Example

Given a Greeting ADT which implements one of Enumeratum's `EnumEntry` types:

```scala
import pureconfig.loadConfig
import pureconfig.module.enumeratum._
import com.typesafe.config.ConfigFactory.parseString
import enumeratum._
import enumeratum.EnumEntry._

object example {
  sealed trait Greeting extends EnumEntry with Snakecase

  object Greeting extends Enum[Greeting] {
    val values = findValues
    case object Hello extends Greeting
    case object GoodBye extends Greeting
    case object ShoutGoodBye extends Greeting with Uppercase
  }
}
import example._
```

And a class to hold the configuration:
```scala
case class GreetingConf(start: Greeting, end: Greeting)
```

We can read a GreetingConf like:
```scala
val conf = parseString("""{
  start: hello
  end: SHOUT_GOOD_BYE
}""")
// conf: com.typesafe.config.Config = Config(SimpleConfigObject({"end":"SHOUT_GOOD_BYE","start":"hello"}))

loadConfig[GreetingConf](conf)
// res1: Either[pureconfig.error.ConfigReaderFailures,GreetingConf] = Right(GreetingConf(Hello,ShoutGoodBye))
```

Note that Enumeratum has a variety of [other ways to define enums](https://github.com/lloydmeta/enumeratum#more-examples) which are [also supported by `pureconfig-enumeratum`](src/test/scala/pureconfig/module/enumeratum/EnumeratumConvertTest.scala). If you need to read integers, another numeric type, or arbitrary strings to specify your enum values, Enumeratum and Pureconfig have you covered.