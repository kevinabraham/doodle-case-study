name          := "doodle-case-study"
organization  := "underscoreio"
scalaVersion  := "2.11.8"
scalacOptions += "-feature"
licenses += ("Apache-2.0", url("http://apache.org/licenses/LICENSE-2.0"))
initialCommands in console := """
      |import doodle.core._
      |import doodle.syntax._
      |import doodle.example._
      |import doodle.jvm.Java2DCanvas
    """.trim.stripMargin
cleanupCommands in console := """
      |doodle.jvm.quit()
    """.trim.stripMargin
