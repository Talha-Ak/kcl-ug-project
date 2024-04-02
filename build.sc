import mill._, scalalib._
import $ivy.`com.lihaoyi::mill-contrib-scoverage:`
import mill.contrib.scoverage.ScoverageModule

object compiler extends RootModule with ScoverageModule {
    def scalaVersion = "3.4.0"
    def scoverageVersion = "2.1.0"

    def ivyDeps = Agg(
        ivy"com.lihaoyi::mainargs:0.6.2",
        ivy"com.lihaoyi::fastparse:3.0.2",
        ivy"com.lihaoyi::os-lib:0.9.2"
    )

    object test extends ScoverageTests with TestModule.ScalaTest {
        def ivyDeps = Agg(ivy"org.scalatest::scalatest:3.2.18")
        def testFramework = "org.scalatest.tools.Framework"
    }
}


