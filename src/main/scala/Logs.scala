object LogsJan01 {
  lazy val logs = io.Source.fromURL(getClass.getResource("/20150101.log")).getLines
}

object LogsDec31 {
  lazy val logs = io.Source.fromURL(getClass.getResource("/20141231.log")).getLines
}
