import mill._
import scalalib._

object riscvSim extends ScalaModule {
    def scalaVersion = "2.13.4"
    def ivyDeps = Agg(
      ivy"com.lihaoyi::mainargs:0.1.4"
    )
}
