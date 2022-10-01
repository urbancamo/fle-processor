package uk.m0nom.fle

class FleParser {
  import fastparse.NoWhitespace._
  import fastparse._

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

  // Convert tuple of ints into a date
  def dmyTrans(t: (Int, Int, Int)):Date = {
    Date(t._3, t._2, t._1)
  }

  def txrxTrans(t: (String, String)): TxRx = {
    TxRx(t._1, t._2)
  }

  /* PARSERS */
  def ws[_: P] = P( CharsWhileIn(" \t") )
  def eol[_: P] = P( ws.? ~ CharsWhileIn("\n") )

  def textToEol[_: P] = P( CharsWhile(_!='\n') )
  def commentBody[_: P] = P( CharsWhile(_!='\n').! ).map(CommentBody)
  def comment[_: P] = P ( "#" ~ ws.? ~ commentBody ~ eol)

  def year[_: P]: P[Int] = P( CharIn("0-9").rep(min=2, max=4).!.map(_.toInt) )
  def month[_: P]: P[Int] = P( CharIn("0-9").rep(min = 1, max = 2).!.map(_.toInt) )
  def day[_: P]: P[Int] = P( CharIn("0-9").rep(min = 1, max = 2).!.map(_.toInt) )

  def dateSeparator[_: P] = P( CharIn("/\\-") )
  def dateValue[_: P] = P( year ~ dateSeparator ~ month ~ dateSeparator ~ day).map(dmyTrans)
  def date[_: P] = P( "date" ~ ws ~ dateValue ~ eol)

  def callsignVal[_: P] = P ( CharsWhileIn("A-Za-z0-9/").! )
  def myCall[_: P] = P ( "mycall" ~ ws ~ callsignVal ~ eol).map(MyCallsign)
  def operator[_: P] = P ( "operator" ~ ws ~ callsignVal ~ eol).map(Operator)

  def gridRefNumBlock[_ : P] = P( CharIn("0-9").rep(min = 2, max = 2))
  def gridRefCharBlock[_ : P] = P( CharIn("a-zA-Z").rep(min = 2, max = 2))
  def gridRef6Char[_: P] = P ( gridRefCharBlock ~ gridRefNumBlock ~ gridRefCharBlock)

  def myGrid[_: P] = P ( "mygrid" ~ ws ~ gridRef6Char.! ~ eol).map(MyGridRef)
  def grid[_: P] = P ( "#" ~ gridRef6Char.!).map(GridRef)

  def sotaRef[_: P] = P (CharIn("0-9a-zA-Z").rep(min=1, max=3) ~ "/" ~ CharIn("A-Za-z").rep(min=2,max=2)
    ~ "-" ~ CharIn("0-9").rep(min=3,max=3).!)

  def wwffRef[_: P] = P ( CharIn("0-9a-zA-Z").rep(min=3, max=4) ~ "-" ~ CharIn("0-9").rep(min=4,max=4).!)
  def potaRef[_: P] = P ( CharIn("0-9a-zA-Z").rep(min=1, max=2) ~ "-" ~ CharIn("0-9").rep(min=4,max=4).!)

  def mySota[_ : P] = P ( "mysota" ~ ws ~ sotaRef.! ~ eol).map(MySotaRef)
  def myWwff[_ : P] = P ( "mywwff" ~ ws ~ wwffRef.! ~ eol).map(MyWwffRef)
  def myPota[_ : P] = P ( "mypota" ~ ws ~ potaRef.! ~ eol).map(MyPotaRef)
  def qslMsg[_ : P] = P ( "qslmsg" ~ ws ~ textToEol.! ~ eol).map(QslMsg)
  def nickname[_: P] = P ( "nickname" ~ ws ~ textToEol.! ~ eol).map(Nickname)

  def band[_: P] = P (StringIn("2190m", "630m", "560m", "160m", "80m",
    "60m", "40m", "30m", "20m", "17m", "15m", "12m", "10m", "6m",
    "4m", "2m", "1.25m", "70cm", "33cm", "23cm", "13cm", "9cm",
    "6cm", "3cm", "1.25cm", "6mm", "4mm", "2.5mm", "2mm", "1mm",
    "2190M", "630M", "560M", "160M", "80M",
    "60M", "40M", "30M", "20M", "17M", "15M", "12M", "10M", "6M",
    "4M", "2M", "1.25M", "70CM", "33CM", "23CM", "13CM", "9CM",
    "6CM", "3CM", "1.25CM", "6MM", "4MM", "2.5MM", "2MM", "1MM"
  )).!.map(Band)

  def freqBody[_: P] = P( CharIn("0-9").rep.? ~ "." ~ CharIn("0-9").rep )
  def freq[_: P] = P( freqBody.! ).map(Frequency)

  def headerOptional[_: P] = P( comment | myGrid | operator | mySota | myWwff | myPota | qslMsg | nickname | eol )
  def header[_: P] = P( headerOptional.rep ~ myCall ~ headerOptional.rep ~ date )

  def mode[_: P] = P( StringIn("CW", "SSB", "BAM", "FM", "RTTY", "FT8", "PSK", "JT65", "JT9", "FT4", "JS8", "ARDOP",
    "ATV", "C4FM", "CHIP", "CLO", "CONTESTI", "DIGITALVOICE", "DOMINO", "DSTAR", "FAX", "FSK441",
    "HELL", "ISCAT", "JT4", "JT6M", "JT44", "MFSK", "MSK144", "MT63", "OLIVIA", "OPERA", "PAC",
    "PAX", "PKT", "PSK2K", "Q15", "QRA64", "ROS", "RTTYM", "SSTV", "T10", "THOR", "THRB", "TOR", "V4",
    "VOI", "WINMOR", "WSPR",
    "cw", "ssb", "bam", "fm", "rtty", "ft8", "psk", "jt65", "jt9", "ft4", "js8", "ardop",
    "atv", "c4fm", "chip", "clo", "contesti", "digitalvoice", "domino", "dstar", "fax", "fsk441",
    "hell", "iscat", "jt4", "jt6m", "jt44", "mfsk", "msk144", "mt63", "olivia", "opera", "pac",
    "pax", "pkt", "psk2k", "q15", "qra64", "ros", "rttym", "sstv", "t10", "thor", "thrb", "tor", "v4",
    "voi", "winmor", "wspr"
  ).!).map(Mode)

  def bandFreqMode[_: P] = P( band ~ ws ~ ((mode.? ~ freq.?) | (freq.? ~ mode.?)) ~ eol)

  def dayInc[_: P] = P( "day" ~ ws.? ~ "+".rep(min=1, max=2).! ~ eol).map(DayInc)

  def time[_: P] = P( CharIn("0-9").rep(min=1, max=4).!).map(Time)
  def callsign[_: P] = P( callsignVal ).map(Callsign)

  def sota[_: P] = P( ("sota" ~ ws).? ~ sotaRef.!).map(Sota)
  def wwff[_: P] = P( ("wwff" ~ ws).? ~ wwffRef.!).map(Wwff)
  def pota[_: P] = P( ("pota" ~ ws).? ~ potaRef.!).map(Pota)
  def sigReport[_: P] = P ( CharIn("0-9").rep(min=1, max=3).! )
  def tXrX[_: P] = P( sigReport ~ ws ~ sigReport ).map(txrxTrans)
  def notes[_: P] = P( "<" ~ CharPred(_!='>').rep.! ~ ">").map(Notes)

  def qsoDetails[_: P] = P( ws ~ ( freq | sota | wwff | pota | grid | tXrX | notes ) )
  def qso[_: P] = P( time ~ ws ~ callsign ~ qsoDetails.rep ~ eol).log

  def rowType[_: P] = P( date | bandFreqMode | qso | dayInc )
  def bodyRow[_: P] = P( (eol ) | rowType ).log
  def fleParser[_: P] = P( header ~ bodyRow.rep )

}
