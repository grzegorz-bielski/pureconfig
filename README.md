# PureConfig

<img src="docs/src/main/resources/microsite/img/pureconfig-logo-1040x1200.png" width="130px" height="150px" align="right">

[![Build Status](https://github.com/pureconfig/pureconfig/workflows/CI/badge.svg?branch=master)](https://github.com/pureconfig/pureconfig/actions?query=workflow%3ACI+branch%3Amaster)
[![Coverage Status](https://coveralls.io/repos/github/pureconfig/pureconfig/badge.svg?branch=master)](https://coveralls.io/github/pureconfig/pureconfig?branch=master)
[![Maven Central](https://maven-badges.herokuapp.com/maven-central/com.github.pureconfig/pureconfig_2.12/badge.svg)](https://search.maven.org/artifact/com.github.pureconfig/pureconfig_2.12)
[![Scaladoc](https://javadoc.io/badge/com.github.pureconfig/pureconfig-core_2.12.svg)](https://javadoc.io/page/com.github.pureconfig/pureconfig-core_2.12/latest/pureconfig/index.html)
[![Join the chat at https://gitter.im/melrief/pureconfig](https://badges.gitter.im/melrief/pureconfig.svg)](https://gitter.im/melrief/pureconfig?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

PureConfig is a Scala library for loading configuration files. It reads [Typesafe Config](https://github.com/lightbend/config) configurations written in [HOCON](https://github.com/lightbend/config/blob/master/HOCON.md#hocon-human-optimized-config-object-notation), Java `.properties`, or JSON to native Scala classes in a boilerplate-free way. Sealed traits, case classes, collections, optional values, and many other [types are all supported out-of-the-box](https://pureconfig.github.io/docs/built-in-supported-types.html). Users also have many ways to [add support for custom types](https://pureconfig.github.io/docs/supporting-new-types.html) or [customize existing ones](https://pureconfig.github.io/docs/overriding-behavior-for-types.html).

<br clear="right"> <!-- Turn off the wrapping for the logo image. -->


## Why

Loading configurations has always been a tedious and error-prone procedure. A common way to do it
consists in writing code to deserialize each fields of the configuration. The more fields there are,
the more code must be written (and tested and maintained...) and this must be replicated for each project.

This kind of code is boilerplate because most of the times the code can be automatically generated by
the compiler based on what must be loaded. For instance, if you are going to load an `Int` for a field
named `foo`, then probably you want some code that gets the values associated with the key `foo` in
the configuration and assigns it to the proper field after converting it to `Int`.

The goal of this library is to create at compile-time the boilerplate necessary to load a configuration of a
certain type. In other words, you define **what** to load and PureConfig provides **how** to load it.


## Quick Start

To use PureConfig in an existing SBT project with Scala 2.12 or a later version, add the following dependency to your
`build.sbt`:

```scala
libraryDependencies += "com.github.pureconfig" %% "pureconfig" % "0.17.5"
```

For a full example of `build.sbt` you can have a look at this [build.sbt](https://github.com/pureconfig/pureconfig/blob/master/example/build.sbt).

Earlier versions of Scala had bugs which can cause subtle compile-time problems in PureConfig.
As a result we recommend only using the latest Scala versions within the minor series.

In your code, import `pureconfig.generic.auto` and define data types and a case class to hold the configuration:

```scala
import pureconfig._
import pureconfig.generic.auto._

case class Port(number: Int) extends AnyVal

sealed trait AuthMethod
case class Login(username: String, password: String) extends AuthMethod
case class Token(token: String) extends AuthMethod
case class PrivateKey(pkFile: java.io.File) extends AuthMethod

case class ServiceConf(
  host: String,
  port: Port,
  useHttps: Boolean,
  authMethods: List[AuthMethod]
)
```

Second, create an `application.conf` file and add it as a resource of your application (with SBT, they are usually
placed in `src/main/resources`):

```
// src/main/resources/application.conf
host = "example.com"
port = 8080
use-https = true
auth-methods = [
  { type = "private-key", pk-file = "/home/user/myauthkey" },
  { type = "login", username = "pureconfig", password = "12345678" }
]
```

Finally, load the configuration:

```scala
ConfigSource.default.load[ServiceConf]
// res4: ConfigReader.Result[ServiceConf] = Right(
//   ServiceConf(
//     "example.com",
//     Port(8080),
//     true,
//     List(PrivateKey(/home/user/myauthkey), Login("pureconfig", "12345678"))
//   )
// )
```

`ConfigReader.Result[ServiceConf]` is just an alias for `Either[ConfigReaderFailures, ServiceConf]`, so you can handle
it just like you would handle an `Either` value.

The various `loadConfig` methods defer to Typesafe Config's
[`ConfigFactory`](https://lightbend.github.io/config/latest/api/com/typesafe/config/ConfigFactory.html) to
select where to load the config files from. Typesafe Config has [well-documented rules for configuration
loading](https://github.com/lightbend/config#standard-behavior) which we'll not repeat. Please see Typesafe
Config's documentation for a full telling of the subtleties.

Alternatively, PureConfig also provides a `loadConfigFromFiles` method that builds a configuration from
an explicit list of files. Files earlier in the list have greater precedence than later ones. Each file can
include a partial configuration as long as the whole list produces a complete configuration. For an example,
see the test of `loadConfigFromFiles` in
[`ApiSuite.scala`](https://github.com/pureconfig/pureconfig/blob/master/tests/src/test/scala/pureconfig/ApiSuite.scala).

Because PureConfig uses Typesafe Config to load configurations, it supports reading files in [HOCON](https://github.com/lightbend/config/blob/master/HOCON.md#hocon-human-optimized-config-object-notation), JSON, and Java `.properties` formats. HOCON is a superset of both JSON and `.properties` that is highly recommended. As an added bonus it supports [advanced features](https://github.com/lightbend/config/blob/master/README.md#features-of-hocon) like variable substitution and file sourcing.


## Documentation

Please see the [full PureConfig documentation](https://pureconfig.github.io/docs) for more information.


## Contribute

PureConfig is a free library developed by several people around the world.
Contributions are welcomed and encouraged. If you want to contribute, we suggest to have a look at the
[available issues](https://github.com/pureconfig/pureconfig/issues) and to talk with
us on the [PureConfig Gitter channel](https://gitter.im/melrief/pureconfig?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge).

If you'd like to add support for types which are not part of the standard Java or Scala libraries, please consider submitting a pull request to create a [module](https://pureconfig.github.io/docs/library-integrations.html). [Pull Request #108](https://github.com/pureconfig/pureconfig/pull/108/files) created a very simple module. It should provide a good template for the pieces you'll need to add.

The steps to create a new module, called _`nexttopmod`_, are:

1. Define a new project in the root `build.sbt`. There are other examples near the top of the file;
2. Create a new  `modules/nexttopmod/` subdirectory;
3. Add a `modules/nexttopmod/build.sbt` defining the module's name and special dependencies;
4. Implement converters. Typically they're in a `package object` in `modules/nexttopmod/src/main/scala/pureconfig/module/nexttopmod/package.scala`;
5. Test the converters. Usually tests would be in `modules/nexttopmod/src/test/scala/pureconfig/module/nexttopmod/NextTopModSuite.scala`;
6. Optionally explain a little bit about how it works in `modules/nexttopmod/README.md`.

PureConfig supports the [Typelevel](http://typelevel.org) [code of conduct](http://typelevel.org/conduct.html) and wants all of its channels (Gitter, GitHub, etc.) to be
welcoming environments for everyone.


## License

[Mozilla Public License, version 2.0](LICENSE)


## Special Thanks

To the [Shapeless](https://github.com/milessabin/shapeless) and to the [Typesafe Config](https://github.com/lightbend/config)
developers.
