name := "ScalaIO"

version := "1.0"

scalaVersion := "2.11.8"

libraryDependencies += "com.cra.figaro" % "figaro_2.11" % "5.0.0.0"


val breezeVersion = "0.13.1"


libraryDependencies  ++= Seq(
  // other dependencies here
  "org.scalanlp" %% "breeze" % breezeVersion,
  // native libraries are not included by default. add this if you want them (as of 0.7)
  // native libraries greatly improve performance, but increase jar sizes.
  // It also packages various blas implementations, which have licenses that may or may not
  // be compatible with the Apache License. No GPL code, as best I know.
  "org.scalanlp" %% "breeze-natives" % breezeVersion,
  // the visualization library is distributed separately as well.
  // It depends on LGPL code.
  "org.scalanlp" %% "breeze-viz" % breezeVersion
)

resolvers ++= Seq(
  // other resolvers here
  // if you want to use snapshot builds (currently 0.12-SNAPSHOT), use this.
  "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/",
  "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/"
)
