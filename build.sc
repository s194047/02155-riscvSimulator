import mill._
import scalalib._

object riscvSim extends ScalaModule {
    def scalaVersion = "2.13.4"
    def ivyDeps = Agg(
        ivy"com.github.scopt::scopt:3.7.1"
  )
}
