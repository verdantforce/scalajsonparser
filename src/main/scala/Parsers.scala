package org.json

import scala.language.implicitConversions

object Parsers { self =>
  type Parser[A] = String => Option[(A, String)]

  def parse[A](p: Parser[A])(s: String): Option[(A, String)] = p(s)

  def parseAll[A](p: Parser[A])(s: String): Option[A] = {
    parse(p)(s) match {
      case Some((a, "")) => Some(a)
      case _ => None
    }
  }

  /**
   *  Parsing Combinators
   * */

  def unitP[A](a: A): Parser[A] = {
    (input: String) => {
      Some((a, input))
    }
  }

  def flatmap[A, B](p: Parser[A], f: A => Parser[B]): Parser[B] = {
    (input: String) => {
      parse(p)(input).flatMap{case (a, s) => parse(f(a))(s)}
    }
  }

  def fmap[A, B](p: Parser[A], f: A => B): Parser[B] = {
    flatmap(p, a => unitP(f(a)))
  }

  def or[A](p1: Parser[A], p2: Parser[A]): Parser[A] = {
    (input: String) => {
      parse(p1)(input).orElse(parse(p2)(input))
    }
  }

  def many[A](p: Parser[A]): Parser[List[A]] = {
    or(
      flatmap(p, a => fmap(many(p), as => a :: as)),
      unitP(List.empty),
    )
  }

  def sequence[A](ps: List[Parser[A]]): Parser[List[A]] = {
    ps match {
      case Nil => unitP(List.empty)
      case x :: xs =>
        flatmap(x, a => fmap(sequence(xs), as => a :: as ))
    }
  }

  def sepBy[A, B](pa: Parser[A], pb: Parser[B]): Parser[List[B]] = {
    for {
      b <- pb
      bs <- many(pa.flatMap(_ => pb))
    } yield b :: bs
  }

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)

    def map[B](f: A => B): Parser[B] = self.fmap(p, f)

    def many: Parser[List[A]] = self.many(p)

//    def map2[B, C](p2: => Parser[B])(f: (A, B) => C): Parser[C] = self.map2(p, p2)(f)

//    def many1 = self.many1(p)

    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatmap(p, f)
  }

  implicit def parserToParserOps[A](p: Parser[A]): ParserOps[A] = ParserOps(p)

  /**
   * Concrete Parsers
   * */
  def ifP(f: Char => Boolean): Parser[Char] = (input: String) => {
    if input.nonEmpty && f(input.charAt(0)) then Some(
      input.charAt(0), input.drop(1)
    )
    else None
  }

  def spanP(f: Char => Boolean): Parser[String] = (input: String) => {
    Some(input.span(f))
  }

  def charP(c: Char): Parser[Char] = {
    ifP(ch => ch == c)
  }

  def stringP(s: String): Parser[String] = {
    fmap(sequence(s.map(c => charP(c)).toList), cs => cs.mkString)
  }

  def whitespaceP(): Parser[String] = {
    fmap(many(ifP(c => c.isWhitespace)), cs => cs.mkString)
  }
}
