import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.example",
      scalaVersion := "2.12.3",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "try-jackson",
    libraryDependencies ++= Seq(
     scalaTest % Test
     ,"org.scala-lang" % "scala-compiler" % "2.12.3"
     , "com.fasterxml.jackson.module" %% "jackson-module-scala" % "2.8.10"
     , "com.fasterxml.jackson.dataformat" % "jackson-dataformat-yaml" % "2.8.10"
     ,"com.fasterxml.jackson.dataformat" % "jackson-dataformat-xml" % "2.8.10"
     
    )

  )
    resolvers += "JBoss Repository" at "https://repository.jboss.org/nexus/content/groups/public-jboss"
