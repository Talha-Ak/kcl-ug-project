import mill._, scalalib._

object compiler extends RootModule with ScalaModule {
    def scalaVersion = "3.4.0"

    def ivyDeps = Agg(
        ivy"com.lihaoyi::mainargs:0.5.4",
        ivy"com.lihaoyi::fastparse:3.0.2",
        ivy"com.lihaoyi::os-lib:0.9.2"
    )

    object test extends ScalaTests {
        def ivyDeps = Agg(ivy"org.scalatest::scalatest:3.2.18")
        def testFramework = "org.scalatest.tools.Framework"
    }
}
