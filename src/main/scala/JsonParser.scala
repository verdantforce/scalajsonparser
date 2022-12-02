package org.json
import Parsers._
import JSON._
import scala.util.matching.Regex

object JsonParser {
  def jsonNull: Parser[JSON] = {
    stringP("null").map(_ => JNull)
  }

  def jsonNumber: Parser[JSON] = {
    lazy val pattern = "[+-]?[0-9]+\\.?[0-9]*([Ee][+-]?[0-9]+)?".r
    (input: String) => {
      pattern.findPrefixMatchOf(input).map(m => (JNumber(
        m.matched.toDouble
      ), m.after.toString))
    }
  }

  def jsonString: Parser[JSON] = {
    for {
      _ <- charP('"')
      s <- many(ifP(c => c != '"'))
      _ <- charP('"')
    } yield JString(s.mkString)
  }

  def jsonBool: Parser[JSON] = {
    (stringP("true") | stringP("false")).map{
      case "true" => JBool(true)
      case _ => JBool(false)
    }
  }

  /*
  case class JArray(get: IndexedSeq[JSON]) extends JSON
  case class JObject(get: Map[String, JSON]) extends JSON
  */
  def jsonArray: Parser[JSON] = {
    val sep = for {
      _ <- whitespaceP()
      v <- charP(',')
      _ <- whitespaceP()
    } yield v

    for {
      _ <- charP('[')
      _ <- whitespaceP()
      xs <- sepBy(sep, jsonValue) | unitP(List.empty)
      _ <- whitespaceP()
      _ <- charP(']')
    } yield JArray(xs)
  }

  def jsonObject: Parser[JSON] = {
    val kv_sep = for {
      _ <- whitespaceP()
      v <- charP(':')
      _ <- whitespaceP()
    } yield v

    val pair_sep = for {
      _ <- whitespaceP()
      v <- charP(',')
      _ <- whitespaceP()
    } yield v

    val pair = for {
      key <- jsonString
      _ <- kv_sep
      value <- jsonValue
    } yield {
      // no need for other branches as jsonString always return a JString
      (key: @unchecked) match {
        case JString(k) => (k, value)
      }
    }

    for {
      _ <- charP('{')
      _ <- whitespaceP()
      xs <- sepBy(pair_sep, pair) | unitP(List.empty)
      _ <- whitespaceP()
      _ <- charP('}')
    } yield JObject(xs.toMap)
  }

  def jsonValue: Parser[JSON] = {
    jsonNull | jsonBool | jsonNumber | jsonString | jsonArray | jsonObject
  }

}