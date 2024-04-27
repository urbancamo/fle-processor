package uk.m0nom.fle

import fastparse.Parsed.Success
import org.scalatest.funsuite.AnyFunSuite
import uk.m0nom.fle.FleParser.{commentBody, myCall, qso}

class FleParserTestSuite extends AnyFunSuite {

  test("FleParser.commentBody") {
    val result = fastparse.parse("foo\nbar", commentBody(using _)).get
    result match {
      case Success(value, _) =>
        assert("foo".equals(value.comment))
    }
  }

  test("FleParser.myCall") {
    val result = fastparse.parse("mycall m0nom/p\n", myCall(using _))
    result match {
      case Success(value, _) =>
        assert("m0nom/p".equals(value.callsign))
    }
  }

  test("FleParser.qso") {
    val result = fastparse.parse("1234 g8cpz/p 14.285 57 55 sota g/ld-050\n", qso(using _))
    result match {
      case Success((optionalTime, callsign, options), _) =>
        optionalTime match {
          case Some(t) => println(t)
          case None => println("No time provided")
        }
        println(callsign.callsign)
        for (option <- options)
          option match {
            case freq: Frequency =>
              println(s"freq: ${freq.freq}")
            case mode: Mode =>
              println(s"mode: ${mode.mode}")
            case txrx: TxRx =>
              println(s"txrx: ${txrx.tx} ${txrx.rx}")
            case sota: Sota =>
              println(s"sota: ${sota.ref}")
          }
    }
  }

  test("FleParser.qsoNoTime") {
    val result = fastparse.parse("   g8cpz/p 14.285 57 55 sota g/ld-050\n", qso(using _))
    result match {
      case Success((optionalTime, callsign, options), _) =>
        optionalTime match {
          case Some(t) => println(t)
          case None => println("No time provided")
        }
        println(callsign.callsign)
        for (option <- options)
          option match {
            case freq: Frequency =>
              println(s"freq: ${freq.freq}")
            case mode: Mode =>
              println(s"mode: ${mode.mode}")
            case txrx: TxRx =>
              println(s"txrx: ${txrx.tx} ${txrx.rx}")
            case sota: Sota =>
              println(s"sota: ${sota.ref}")
          }
    }
  }

//  test("FleParser.header") {
//    val result = fastparse.parse("mycall m0nom\nmygrid io84ni\noperator m0nom\ndate 2022-01-01", header(_))
//    result match {
//      case Success(myCallsign, date) =>
//
//    }
//  }
}