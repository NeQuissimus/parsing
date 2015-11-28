import org.parboiled2._
import scala.util.{ Success, Failure, Try }
import java.net.{ InetAddress, Inet4Address }
import java.time.{ LocalDateTime => LDT }
import java.time.format.{ DateTimeFormatter => DTF }

object Main extends App {
  import Logs._

  List(l1, l2, l3, l4, l5, l6, l7, l8, l9) foreach { l =>
    println(l)
    val p = LogParser(l)
    p.InputLine.run() match {
      case Success(v) => println(s"Expression output: $v")
      case Failure(e: ParseError) => println(s"Expression invalid: ${p.formatError(e)} || Stack: ${p.valueStack}")
      case Failure(e) => println(s"Error: $e")
    }
    println()
  }

  // val msgs: Iterator[Try[AnyRef]] = (LogsJan01.logs ++ LogsDec31.logs) map { s: String =>
  //   LogParser(s).InputLine.run()
  // } filter { e: Try[AnyRef] =>
  //   e match {
  //     case Success(x: os.linux.UnknownMessage) => true
  //     case Success(_) => false
  //     case Failure(_) => true
  //   }
  // }
  // msgs foreach println
}

object Logs {
  val l1 = "Jan  1 00:13:44 10.30.26.1 Got new client [18:B4:30:23:AA:67] associated from BAND24G-1.1 (2.4 Ghz)"
  val l2 = "Jan 11 00:13:53 10.30.26.2 DHCP: Server receive DISCOVER from 18:b4:30:23:aa:67."
  val l3 = "Jan  1 22:13:53 10.30.26.3 DHCP: Server sending OFFER of 10.30.26.115 for static DHCP client."
  val l4 = "Jan  3 18:13:53 10.30.26.4 DHCP: Server receive REQUEST from 18:b4:30:23:aa:67."
  val l5 = "Feb 22 12:13:53 10.30.26.5 DHCP: Server sending ACK to 10.30.26.115. (Lease time = -1)"
  val l6 = "Sep 12 14:22:12 10.30.26.254 DHCP: Client receive ACK from 192.0.240.1, IP=198.48.238.170, Lease time=3600."
  val l7 = "Oct 21 01:02:14 10.30.26.1 ATT:001[SYN-ACK][10.30.26.103][LAN-1]"
  val l8 = "Jan  1 00:51:43 10.30.26.1 DHCP: Server sending NAK to 40:b0:fa:68:1f:63."
  val l9 = "Jan  1 00:51:41 10.30.26.1 Got new client [88:C9:D0:D5:B1:06] associated from BAND24G-1.1 (2.4 Ghz)"
}

case class LogParser(val input: ParserInput) extends Parser {
  def InputLine = rule { DateMonth3NoYear ~ Whitespace ~ HHMMSS ~ Whitespace ~ IPv4 ~ Whitespace ~ (
    "DHCP: " ~ (
      "Server " ~ (
        "receive " ~ (
          "DISCOVER from " ~ MAC ~ "." ~ EOI ~> os.linux.dhcp.server.Discover
          | "REQUEST from " ~ MAC ~ "." ~ EOI ~> os.linux.dhcp.server.Request
          )
        | "sending " ~ (
          "OFFER of " ~ IPv4 ~ " for static DHCP client." ~ EOI ~> os.linux.dhcp.server.Offer
          | "ACK to " ~ IPv4 ~ ". (Lease time = " ~ Number ~ ")" ~ EOI ~> os.linux.dhcp.server.Ack
          | "NAK to " ~ MAC ~ "." ~ EOI ~> os.linux.dhcp.server.Nak
        ))
      | "Client receive ACK from " ~ IPv4 ~ ", IP=" ~ IPv4 ~ ", Lease time=" ~ Number ~ "." ~ EOI ~> os.linux.dhcp.client.Ack
    )
    | ("ATT:001[SYN-ACK][" ~ IPv4 ~ "][" ~ Anything ~ EOI ~> dlink.tcp.SynAck)
    | ("Got new client [" ~ MAC ~ "] associated from " ~ Anything ~ EOI ~> dlink.wifi.NewClient)
    | (Anything ~ EOI ~> os.linux.UnknownMessage)
  )}

  def DateMonth3NoYear = rule { capture(Month3 ~ Whitespace ~ DayOfMonth) }

  def Month3 = rule { "Jan" | "Feb" | "Mar" | "Apr" | "May" | "Jun" | "Jul" | "Aug" | "Sep" | "Oct" | "Nov" | "Dec" }
  def DayOfMonth = rule { Digits ~> (i => test(i >= 1 && i <= 31)) }
  def ZeroTo23 = rule { Digits ~> (i => test(i >= 0 && i <= 23)) }
  def ZeroTo59 = rule { Digits ~> (i => test(i >= 0 && i <= 59)) }
  def HHMMSS = rule { capture(ZeroTo23 ~ ':' ~ ZeroTo59 ~ ':' ~ ZeroTo59) }

  def IPv4 = rule { capture(4.times(IPv4Octet).separatedBy('.')) }
  def IPv4Octet = rule { Digits ~> (i => test(i >= 0 && i <= 255)) }

  def MAC = rule { capture(6.times(2.times(CharPredicate.HexDigit)).separatedBy(':')) }

  def Number = rule { capture(optional('-') ~ DigitsStr) }

  def Digit = rule { capture(CharPredicate.Digit) ~> (_.toInt) }
  def Digits = rule { capture(DigitsStr) ~> (_.toInt) }
  def DigitsStr = rule { oneOrMore(CharPredicate.Digit) }

  def Whitespace = rule { zeroOrMore(' ') }
  def Anything = rule { capture(zeroOrMore(CharPredicate.Printable)) }
}

trait ParsedDate { self: {val dateStr: String; val timeStr: String} =>
  lazy val date = LDT.parse("2014 " + dateStr + ' ' + timeStr, DTF.ofPattern("yyyy MMM [ ]d HH:mm:ss"))
}

trait ParsedIp { self: {val ipStr: String} =>
  lazy val ip: InetAddress = InetAddress.getByName(ipStr)
}

package dlink {
  package tcp {
    case class SynAck(dateStr: String, timeStr: String, ipStr: String, ackIpStr: String, rest: String) extends ParsedDate with ParsedIp {
      lazy val ackIp = InetAddress.getByName(ackIpStr)
      override def toString = s"dlink.tcp.SynAck[$date, $ip, $ackIp, $rest]"
    }
  }

  package wifi {
    case class NewClient(dateStr: String, timeStr: String, ipStr: String, clientMac: String, antenna: String) extends ParsedDate with ParsedIp {
      override def toString = s"dlink.wifi.NewClient[$date, $ip, $clientMac, $antenna]"
    }
  }
}

package os.linux {
  case class UnknownMessage(dateStr: String, timeStr: String, ipStr: String, rest: String) extends ParsedDate with ParsedIp {
    override def toString = s"UnknownMessage[$date, $ip, $rest]"
  }

  package dhcp {
    package client {
      case class Ack(dateStr: String, timeStr: String, ipStr: String, routerIpStr: String, assignedIpStr: String, leaseStr: String) extends ParsedDate with ParsedIp {
        lazy val assignedIp = InetAddress.getByName(assignedIpStr)
        lazy val routerIp = InetAddress.getByName(routerIpStr)
        lazy val lease = leaseStr.toLong
        override def toString = s"dhcp.client.Ack[$date, $ip, $routerIp, $assignedIp, $lease]"
      }
    }

    package server {
      case class Ack(dateStr: String, timeStr: String, ipStr: String, targetIpStr: String, leaseStr: String) extends ParsedDate with ParsedIp {
        lazy val targetIp = InetAddress.getByName(targetIpStr)
        lazy val lease = leaseStr.toLong
        override def toString = s"dhcp.server.Ack[$date, $ip, $targetIp, $lease]"
      }

      case class Discover(dateStr: String, timeStr: String, ipStr: String, clientMac: String) extends ParsedDate with ParsedIp {
        override def toString = s"dhcp.server.Discover[$date, $ip, $clientMac]"
      }

      case class Nak(dateStr: String, timeStr: String, ipStr: String, clientMac: String) extends ParsedDate with ParsedIp {
        override def toString = s"dhcp.server.Nak[$date, $ip, $clientMac]"
      }

      case class Offer(dateStr: String, timeStr: String, ipStr: String, offeredIpStr: String) extends ParsedDate with ParsedIp {
        lazy val offeredIp = InetAddress.getByName(offeredIpStr)
        override def toString = s"dhcp.server.Offer[$date, $ip, $offeredIp]"
      }

      case class Request(dateStr: String, timeStr: String, ipStr: String, clientMac: String) extends ParsedDate with ParsedIp {
        override def toString = s"dhcp.server.Request[$date, $ip, $clientMac]"
      }
    }
  }
}
