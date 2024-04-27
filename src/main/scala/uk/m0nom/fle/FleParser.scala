package uk.m0nom.fle

case class Date(day: Int, month: Int, year: Int)

case class CommentBody(comment: String)

case class MyCallsign(callsign: String)

case class Callsign(callsign: String)

case class Operator(callsign: String)

case class MyGridRef(gridRef: String)

case class GridRef(gridRef: String)

case class MySotaRef(ref: String)

case class MyWwffRef(ref: String)

case class MyPotaRef(ref: String)

case class QslMsg(msg: String)

case class Nickname(nick: String)

case class Frequency(freq: String)

case class Mode(mode: String)

case class Band(band: String)

case class DayInc(plus: String)

case class Time(time: String)

case class Sota(ref: String)

case class Wwff(ref: String)

case class Pota(ref: String)

case class TxRx(tx: String, rx: String)

case class Notes(notes: String)


object FleParser {

  import fastparse.*
  import fastparse.NoWhitespace.*

  // Convert tuple of ints into a date
  private def dmyTrans(t: (Int, Int, Int)): Date = {
    Date(t._3, t._2, t._1)
  }

  private def txrxTrans(t: (String, String)): TxRx = {
    TxRx(t._1, t._2)
  }

  /* PARSERS */
  private def ws[$: P]: P[Unit] = P(CharsWhileIn(" \t"))

  private def eol[$: P]: P[Unit] = P(ws.? ~ CharsWhileIn("\n"))

  private def textToEol[$: P]: P[Unit] = P(CharsWhile(_ != '\n'))

  def commentBody[$: P]: P[CommentBody] = P(CharsWhile(_ != '\n').!).map(CommentBody.apply)

  def comment[$: P]: P[CommentBody] = P("#" ~ ws.? ~ commentBody ~ eol)

  private def year[$: P]: P[Int] = P(CharIn("0-9").rep(min = 2, max = 4).!.map(_.toInt))

  private def month[$: P]: P[Int] = P(CharIn("0-9").rep(min = 1, max = 2).!.map(_.toInt))

  private def day[$: P]: P[Int] = P(CharIn("0-9").rep(min = 1, max = 2).!.map(_.toInt))

  private def dateSeparator[$: P]: P[Unit] = P(CharIn("/\\-"))

  private def dateValue[$: P]: P[Date] = P(year ~ dateSeparator ~ month ~ dateSeparator ~ day).map(dmyTrans)

  private def date[$: P]: P[Date] = P("date" ~ ws ~ dateValue ~ eol)

  private def callsignVal[$: P]: P[String] = P(CharsWhileIn("A-Za-z0-9/").!)

  def myCall[$: P]: P[MyCallsign] = P("mycall" ~ ws ~ callsignVal ~ eol).map(MyCallsign.apply)

  private def operator[$: P]: P[Operator] = P("operator" ~ ws ~ callsignVal ~ eol).map(Operator.apply)

  private def gridRefNumBlock[$: P]: P[Unit] = P(CharIn("0-9").rep(min = 2, max = 2))

  private def gridRefCharBlock[$: P]: P[Unit] = P(CharIn("a-zA-Z").rep(min = 2, max = 2))

  private def gridRef6Char[$: P]: P[Unit] = P(gridRefCharBlock ~ gridRefNumBlock ~ gridRefCharBlock)

  private def myGrid[$: P]: P[MyGridRef] = P("mygrid" ~ ws ~ gridRef6Char.! ~ eol).map(MyGridRef.apply)

  private def grid[$: P]: P[GridRef] = P("#" ~ gridRef6Char.!).map(GridRef.apply)

  private def sotaRef[$: P]: P[String] = P(CharIn("0-9a-zA-Z").rep(min = 1, max = 3) ~ "/" ~ CharIn("A-Za-z").rep(min = 2, max = 2)
    ~ "-" ~ CharIn("0-9").rep(min = 3, max = 3).!)

  private def wwffRef[$: P]: P[String] = P(CharIn("0-9a-zA-Z").rep(min = 3, max = 4) ~ "-" ~ CharIn("0-9").rep(min = 4, max = 4).!)

  private def potaRef[$: P]: P[String] = P(CharIn("0-9a-zA-Z").rep(min = 1, max = 2) ~ "-" ~ CharIn("0-9").rep(min = 4, max = 4).!)

  private def mySota[$: P]: P[MySotaRef] = P("mysota" ~ ws ~ sotaRef.! ~ eol).map(MySotaRef.apply)

  private def myWwff[$: P]: P[MyWwffRef] = P("mywwff" ~ ws ~ wwffRef.! ~ eol).map(MyWwffRef.apply)

  private def myPota[$: P]: P[MyPotaRef] = P("mypota" ~ ws ~ potaRef.! ~ eol).map(MyPotaRef.apply)

  private def qslMsg[$: P]: P[QslMsg] = P("qslmsg" ~ ws ~ textToEol.! ~ eol).map(QslMsg.apply)

  private def nickname[$: P]: P[Nickname] = P("nickname" ~ ws ~ textToEol.! ~ eol).map(Nickname.apply)

  private def band[$: P]: P[Band] = P(StringIn("2190m", "630m", "560m", "160m", "80m",
    "60m", "40m", "30m", "20m", "17m", "15m", "12m", "10m", "6m",
    "4m", "2m", "1.25m", "70cm", "33cm", "23cm", "13cm", "9cm",
    "6cm", "3cm", "1.25cm", "6mm", "4mm", "2.5mm", "2mm", "1mm",
    "2190M", "630M", "560M", "160M", "80M",
    "60M", "40M", "30M", "20M", "17M", "15M", "12M", "10M", "6M",
    "4M", "2M", "1.25M", "70CM", "33CM", "23CM", "13CM", "9CM",
    "6CM", "3CM", "1.25CM", "6MM", "4MM", "2.5MM", "2MM", "1MM"
  )).!.map(Band.apply)

  private def freqBody[$: P]: P[Unit] = P(CharIn("0-9").rep.? ~ "." ~ CharIn("0-9").rep)

  def freq[$: P]: P[Frequency] = P(freqBody.!).map(Frequency.apply)

  private def headerOptional[$: P]: P[Any] = P(comment | myGrid | operator | mySota | myWwff | myPota | qslMsg | nickname | eol)

  private def header[$: P]: P[(Seq[Any], MyCallsign, Seq[Any], Date)] = P(headerOptional.rep ~ myCall ~ headerOptional.rep ~ date)

  def mode[$: P]: P[Mode] = P(StringIn("CW", "SSB", "BAM", "FM", "RTTY", "FT8", "PSK", "JT65", "JT9", "FT4", "JS8", "ARDOP",
    "ATV", "C4FM", "CHIP", "CLO", "CONTESTI", "DIGITALVOICE", "DOMINO", "DSTAR", "FAX", "FSK441",
    "HELL", "ISCAT", "JT4", "JT6M", "JT44", "MFSK", "MSK144", "MT63", "OLIVIA", "OPERA", "PAC",
    "PAX", "PKT", "PSK2K", "Q15", "QRA64", "ROS", "RTTYM", "SSTV", "T10", "THOR", "THRB", "TOR", "V4",
    "VOI", "WINMOR", "WSPR",
    "cw", "ssb", "bam", "fm", "rtty", "ft8", "psk", "jt65", "jt9", "ft4", "js8", "ardop",
    "atv", "c4fm", "chip", "clo", "contesti", "digitalvoice", "domino", "dstar", "fax", "fsk441",
    "hell", "iscat", "jt4", "jt6m", "jt44", "mfsk", "msk144", "mt63", "olivia", "opera", "pac",
    "pax", "pkt", "psk2k", "q15", "qra64", "ros", "rttym", "sstv", "t10", "thor", "thrb", "tor", "v4",
    "voi", "winmor", "wspr"
  ).!).map(Mode.apply)

  private def bandFreqMode[$: P]: P[(Band, (Option[Product], Option[Product]))] = P(band ~ ws ~ ((mode.? ~ freq.?) | (freq.? ~ mode.?)) ~ eol)

  private def dayInc[$: P]: P[DayInc] = P("day" ~ ws.? ~ "+".rep(min = 1, max = 2).! ~ eol).map(DayInc.apply)

  private def time[$: P]: P[Time] = P(CharIn("0-9").rep(min = 1, max = 4).!).map(Time.apply)

  def callsign[$: P]: P[Callsign] = P(callsignVal).map(Callsign.apply)

  def sota[$: P]: P[Sota] = P(("sota" ~ ws).? ~ sotaRef.!).map(Sota.apply)

  private def wwff[$: P]: P[Wwff] = P(("wwff" ~ ws).? ~ wwffRef.!).map(Wwff.apply)

  private def pota[$: P]: P[Pota] = P(("pota" ~ ws).? ~ potaRef.!).map(Pota.apply)

  private def sigReport[$: P]: P[String] = P(CharIn("0-9").rep(min = 1, max = 3).!)

  private def tXrX[$: P]: P[TxRx] = P(sigReport ~ ws ~ sigReport).map(txrxTrans)

  private def notes[$: P]: P[Notes] = P("<" ~ CharPred(_ != '>').rep.! ~ ">").map(Notes.apply)

  private def qsoDetails[$: P]: P[Product] = P(ws ~ (freq | sota | wwff | pota | grid | tXrX | notes))

  def qso[$: P]: P[(Option[Time], Callsign, Seq[Product])] = P(time.? ~ ws ~ callsign ~ qsoDetails.rep ~ eol).log

  private def rowType[$: P]: P[Product] = P(date | bandFreqMode | qso | dayInc)

  private def bodyRow[$: P]: P[Any] = P(eol | rowType).log

  def fleParser[$: P]: P[(Seq[Any], MyCallsign, Seq[Any], Date, Seq[Any])] = P(header ~ bodyRow.rep)

}
