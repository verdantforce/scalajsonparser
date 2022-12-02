package org.json

import org.scalatest.funsuite.AnyFunSuite
import Parsers._

class ParserSuite extends AnyFunSuite {

  test("charP") {
    val p = charP('h')
    assert(parse(p)("hello") == Some('h', "ello"))
    assert(parse(p)("world").isEmpty)
  }

  test("stringP") {
    val p = stringP("hello")
    assert(parse(p)("hello") == Some("hello", ""))
    assert(parse(p)("hell").isEmpty)
  }

  test("whitespaceP") {
    val p = whitespaceP()
    assert(parse(p)("hello") == Some("", "hello"))
    assert(parse(p)("  \n\thello") == Some("  \n\t", "hello"))
  }

  test("many") {
    val p = charP('a')
    val mp = many(p)
    assert(parse(mp)("aaabbbb") == Some(List('a', 'a', 'a'), "bbbb"))
  }
}