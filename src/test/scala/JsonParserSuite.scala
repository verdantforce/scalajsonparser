package org.json

import JSON.*
import JsonParser.*
import Parsers.*

import org.scalatest.funsuite.AnyFunSuite

class JsonParserSuite extends AnyFunSuite {

  test("jsonNull") {
    assert(parseAll(jsonNull)("null").contains(JNull))
  }

  test("jsonNumber") {
    assert(parse(jsonNumber)("123 hello").contains((JNumber(123.toDouble), " hello")))
    assert(parse(jsonNumber)("-123 hello").contains((JNumber(-123.toDouble), " hello")))
    assert(parse(jsonNumber)("-10e4 hello").contains((JNumber(-10e4), " hello")))
    assert(parse(jsonNumber)("-10e-4 hello").contains((JNumber(-10e-4), " hello")))
    assert(parse(jsonNumber)("-123.45 hello").contains((JNumber(-123.45), " hello")))
    assert(parse(jsonNumber)("-0.45 hello").contains((JNumber(-0.45), " hello")))
    assert(parse(jsonNumber)("hello").isEmpty)
  }

  test("jsonString") {
    assert(parseAll(jsonString)("\"hello\"").contains(JString("hello")))
  }

  test("jsonBool") {
    assert(parseAll(jsonBool)("true").contains(JBool(true)))
    assert(parseAll(jsonBool)("false").contains(JBool(false)))
    assert(parseAll(jsonBool)("foo").isEmpty)
  }

  test("jsonArray") {
    assert(parseAll(jsonArray)("[    ]").contains(JArray(List.empty)))
    assert {
      parseAll(jsonArray)("[123, null, true]").contains(
        JArray(
          List(
            JNumber(123.toDouble),
            JNull,
            JBool(true)
          )
        )
      )
    }

    assert {
      parseAll(jsonArray)("[123 , \"hello\", [null  ]]").contains(
        JArray(
          List(
            JNumber(123.toDouble),
            JString("hello"),
            JArray(
              List(JNull)
            )
          )
        )
      )
    }
  }

  test("jsonObject") {
    assert(parseAll(jsonObject)("{   }").contains(JObject(Map.empty)))
    assert {
      parseAll(jsonObject)(
        """
          |{
          |  "double": 123.1,
          |  "null": null,
          |  "string": "hello",
          |  "array": [{}],
          |  "object": {
          |     "a": 123,
          |     "b": true
          |  }
          |}
          |""".stripMargin.trim
      ).contains(
        JObject(
          Map(
            "double" -> JNumber(123.1),
            "null" -> JNull,
            "string" -> JString("hello"),
            "array" -> JArray(
              List(JObject(Map.empty))
            ),
            "object" -> JObject(
              Map(
                "a" -> JNumber(123.toDouble),
                "b" -> JBool(true)
              )
            )
          )
        )
      )
    }
  }

}