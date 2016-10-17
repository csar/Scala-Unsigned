# Scala Unsigned #

This library aims to add UByte, UShort, UInt and ULong datatypes that work more or less seamlessly in Scala.

I was a bit tired dealing with unsigned types read from interfaces like SBE

This is a rather quick hack that is intended to work together with most of the Scala functions based on numeric types.

One goal is high performance, not so much for the calculations, but for parsing from external data sources.

There are few implicit conversions so that we don't get into a conversion hell, but it requires a bit more experience using it.

Add it to the build.sbt with

libraryDependencies += "net.karana" % "scala-unsigned_2.11" % "0.1.2" 

## Contribution policy ##

Contributions via GitHub pull requests are gladly accepted from their original author. Along with any pull requests, please state that the contribution is your original work and that you license the work to the project under the project's open source license. Whether or not you state this explicitly, by submitting any copyrighted material via pull request, email, or other means you agree to license the material under the project's open source license and warrant that you have the legal authority to do so.

## License ##

This code is open source software licensed under the [MIT license](https://opensource.org/licenses/mit-license.php).
