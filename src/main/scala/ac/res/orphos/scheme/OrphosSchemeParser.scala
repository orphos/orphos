package ac.res.orphos.scheme

import javolution.text.TextFormat
import org.jscience.mathematics.number.{LargeInteger, Rational}
import org.parboiled2._

object OrphosSchemeParser {
  val LetterCategories: Seq[Int] = {
    import Character._
    Seq[Int](
      UPPERCASE_LETTER,
      LOWERCASE_LETTER,
      MODIFIER_LETTER,
      OTHER_LETTER,
      NON_SPACING_MARK,
      COMBINING_SPACING_MARK,
      ENCLOSING_MARK,
      DECIMAL_DIGIT_NUMBER,
      LETTER_NUMBER,
      OTHER_NUMBER,
      DASH_PUNCTUATION,
      CONNECTOR_PUNCTUATION,
      OTHER_PUNCTUATION,
      CURRENCY_SYMBOL,
      MATH_SYMBOL,
      MODIFIER_SYMBOL,
      OTHER_SYMBOL,
      PRIVATE_USE
    )
  }

}
import OrphosSchemeParser._

class OrphosSchemeParser(val input: ParserInput) extends Parser {
  def specialInitial: Rule0 = rule { anyOf("!$%&*/:<=>?^_~") }

  def bmpLetter: Rule0 = rule {
    CharPredicate.Alpha ++ CharPredicate.from { c =>
      LetterCategories.contains(Character.getType(c))
    }
  }

  def smp: Rule0 = rule { CharPredicate.from(Character.isHighSurrogate) ~ CharPredicate.from(Character.isLowSurrogate) }

  def smpLetter: Rule0 = rule {
    capture(smp) ~> { c: String =>
      test(LetterCategories.contains(Character.getType(c.codePointAt(0))))
    }
  }

  def letter: Rule0 = rule {
    bmpLetter | smpLetter
  }

  def initial: Rule0 = rule {
    specialInitial | letter
  }

  def digit: Rule0 = rule { CharPredicate.Digit }

  def explicitSign: Rule0 = rule { anyOf("+-") }

  def specialSubsequent: Rule0 = rule { anyOf("+-.@") }

  def subsequent: Rule0 = rule {
    initial | digit | specialSubsequent
  }

  def identifier: Rule1[String] = rule {
    capture(
      initial ~ zeroOrMore(subsequent)
    )
  }

  def peculiarIdentifier: Rule0 = rule {
    explicitSign ~ signSubsequent ~ zeroOrMore(subsequent) |
      explicitSign ~ '.' ~ dotSubSequent ~ zeroOrMore(subsequent) |
      '.' ~ dotSubSequent ~ zeroOrMore(subsequent) |
      explicitSign
  }
  def dotSubSequent: Rule0 = rule { signSubsequent | '.' }
  def signSubsequent: Rule0 = rule { initial | explicitSign | '@' }
  def t: Rule1[Boolean] = rule { ("#true" | "#t") ~> (() => true) }
  def f: Rule1[Boolean] = rule { ("#false" | "f") ~> (() => false) }
  def boolean: Rule1[Boolean] = rule { t | f }
  def mnemonicEscape: Rule1[String] = rule {
    valueMap(Map(
      "\\a" -> "\u0007",
      "\\b" -> "\b",
      "\\t" -> "\t",
      "\\n" -> "\n",
      "\\r" -> "\r"
    ))
  }
  def character: Rule1[String] = rule { "#\\x" ~ hexScalarValue | "#\\" ~ characterName | "#\\" ~ capture(smp) | "#\\" ~ capture(ANY) }
  def characterName: Rule1[String] = rule {
    valueMap(Map[String, String](
      "alarm" -> "\u0007",
      "backspace" -> "\b",
      "delete" -> "\u007f",
      "escape" -> "\u001b",
      "newline" -> "\n",
      "null" -> "\0",
      "return" -> "\r",
      "space" -> " ",
      "tab" -> "\t"
    ))
  }
  def hexScalarValue: Rule1[String] = rule {
    capture(oneOrMore(CharPredicate.HexDigit)) ~> { v: String =>
      val cs = Character.toChars(Integer.parseInt(v, 16))
      new String(cs, 0, cs.length)
    }
  }
  def partOfString: Rule1[String] = rule {
    capture(oneOrMore(noneOf("\"\\"))) |
      mnemonicEscape |
      valueMap(Map(
        "\\\"" -> "\"",
        "\\\\" -> "\\"
      ))
  }

  def byte: Rule1[Byte] = rule {
    capture(oneOrMore(CharPredicate.Digit)) ~> { c: String =>
      test {
        val i = Integer.parseInt(c)
        0 <= i && i <= 255
      } ~ push(java.lang.Byte.parseByte(c))
    }
  }
  def ureal2: Rule1[Rational] = rule {
    uinteger2 ~ '/' ~ uinteger2 ~>  { (dividend: LargeInteger, divisor: LargeInteger) => Rational.valueOf(dividend, divisor) } |
      uinteger2 ~>  { i: LargeInteger => Rational.valueOf(i, LargeInteger.ONE) }
  }
  def ureal8: Rule1[Rational] = rule {
    uinteger8 ~ '/' ~ uinteger8 ~> ((dividend, divisor) => Rational.valueOf(dividend, divisor)) |
      uinteger8 ~>  { i: LargeInteger => Rational.valueOf(i, LargeInteger.ONE) }
  }
  def ureal10: Rule1[Rational] = rule {
    uinteger10 ~ '/' ~ uinteger10 ~>  { (dividend: LargeInteger, divisor: LargeInteger) => Rational.valueOf(dividend, divisor) } |
      uinteger10 ~>  { i: LargeInteger => Rational.valueOf(i, LargeInteger.ONE) } |
      decimal10 ~>  { i: LargeInteger => Rational.valueOf(i, LargeInteger.ONE) }
  }
  def ureal16: Rule1[Rational] = rule {
    uinteger16 ~ '/' ~ uinteger16 ~>  { (dividend: LargeInteger, divisor: LargeInteger) => Rational.valueOf(dividend, divisor) } |
      uinteger16 ~>  { i: LargeInteger => Rational.valueOf(i, LargeInteger.ONE) }
  }
  def decimal10: Rule1[LargeInteger] = rule {
    uinteger10 ~ suffix ~>  { (i: LargeInteger, s: Option[LargeInteger]) => s match {
      case None => i
      case Some(s) => i.times(s.pow(10))
    } }
  }
  def uinteger2: Rule1[LargeInteger] = rule { capture(oneOrMore(digit2)) ~>   { s: String => LargeInteger.parse(s, 2, TextFormat.Cursor.newInstance(0, s.length)) }}
  def uinteger8: Rule1[LargeInteger] = rule { capture(oneOrMore(digit8)) ~>  { s: String => LargeInteger.parse(s, 8, TextFormat.Cursor.newInstance(0, s.length)) }}
  def uinteger10: Rule1[LargeInteger] = rule { capture(oneOrMore(digit10)) ~>  { s: String => LargeInteger.parse(s, 10, TextFormat.Cursor.newInstance(0, s.length)) }}
  def uinteger16: Rule1[LargeInteger] = rule { capture(oneOrMore(digit16)) ~>  { s: String => LargeInteger.parse(s, 16, TextFormat.Cursor.newInstance(0, s.length)) }}
  def prefix2: Rule1[Boolean] = rule { (exactness ~ radix2) | (radix2 ~ exactness)}
  def prefix8: Rule1[Boolean] = rule { (exactness ~ radix2) | (radix2 ~ exactness)}
  def prefix10: Rule1[Boolean] = rule { (exactness ~ radix2) | (radix2 ~ exactness)}
  def prefix16: Rule1[Boolean] = rule { (exactness ~ radix2) | (radix2 ~ exactness)}
  def infnan: Rule1[Double] = rule {
    valueMap[Double](Map(
      "+inf.0" -> Double.PositiveInfinity,
      "-inf.0" -> Double.NegativeInfinity,
      "+nan.0" -> Double.NaN,
      "-nan.0" -> Double.NaN
    ))
  }
  def suffix: Rule1[Option[LargeInteger]] = rule { optional('e' ~ sign ~ uinteger10 ~> { (s: Int, i: LargeInteger) =>
    if (s == -1) i.opposite() else i
  }) }
  def sign: Rule1[Int] = rule {
    capture(optional(ch('+') | '-')) ~> { s: String =>
      s match {
        case "-" => -1
        case _ => 1
      }
    }
  }
  def exactness: Rule1[Boolean] = rule { (str("#i") ~> (() => false)) | (str("#e").? ~> (() => true)) }
  def radix2: Rule0 = rule { "#b" }
  def radix8: Rule0 = rule { "#o" }
  def radix10: Rule0 = rule { "#d".? }
  def radix16: Rule0 = rule { "#x" }
  def digit2: Rule0 = rule { anyOf("01") }
  def digit8: Rule0 = rule { CharPredicate('0' to '7') }
  def digit10: Rule0 = rule { CharPredicate.Digit }
  def digit16: Rule0 = rule { CharPredicate.HexDigit }
}
