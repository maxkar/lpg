import sbt._
import Keys._
import sbtunidoc.Plugin._

/* Junit interface docs: https://github.com/szeiger/junit-interface  */

object MySettings {
  val buildSettings = Defaults.defaultSettings ++ Seq(
    scalaSource in Compile := baseDirectory.value / "src",
    scalaSource in Test := baseDirectory.value / "test",
    libraryDependencies += "com.novocode" % "junit-interface" % "0.10" % "test",
    testOptions += Tests.Argument(TestFrameworks.JUnit, "-a", "-s"),
    scalacOptions += "-feature",
    scalacOptions in (Compile, doc) ++= Opts.doc.title("Language playground")
  )
}

object CustomBuild extends Build {
  import MySettings._

  private def prj(path : String, deps : ClasspathDep[ProjectReference]*) : Project = {
    val pname = path.replace('/', '_')
    Project(pname, file(path), settings = buildSettings).
      dependsOn(deps : _*)
  }

  lazy val front = prj("frontend/default")

  lazy val root = Project("root", file("."),
    settings = buildSettings ++ unidocSettings
  ).aggregate(
    front
  )
}
