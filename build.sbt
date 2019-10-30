name := "Scala_HW2"

version := "0.1"

scalaVersion := "2.13.1"

libraryDependencies ++= Seq(
  "org.scalikejdbc" %% "scalikejdbc"       % "3.3.5",
  "com.h2database"  %  "h2"                % "1.4.199",
  "ch.qos.logback"  %  "logback-classic"   % "1.2.3"
)
libraryDependencies += "mysql" % "mysql-connector-java" % "8.0.18"
