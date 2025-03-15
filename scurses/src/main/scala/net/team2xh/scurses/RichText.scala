package net.team2xh.scurses

import fastparse.{*, given}
import NoWhitespace.{*, given}

/** RichText string interpolator.
  *
  * Example usage:
  *   val color = "blue"
  *   val greet = "Hello"
  *   val richText = r"[b][fg:$color]$greet[/b], World!"
  *
  * Format:
  *   [b]        Start using bold text
  *   [u]        Start underlining text
  *   [bl]       Start blinking text
  *   [r]        Start reversing foreground and background
  *   [fg:color] Start coloring the foreground with a named color (e.g. "red")
  *   [fg:id]    Same with an xterm 256-color code (0 - 256)
  *   [fg:hex]   Same with an arbitrary RGB hexadecimal color code
  *   [bg:color] Start coloring the background with a named color (e.g. "red")
  *   [bg:id]    Same with an xterm 256-color code (0 - 256)
  *   [bg:hex]   Same with an arbitrary RGB hexadecimal color code
  *
  *   [/b]       Stop using bold text
  *   [/u]       Stop underlining text
  *   [/bl]      Stop blinking text
  *   [/r]       Stop reversing foreground and background
  *   [/fg]      Stop coloring the foreground
  *   [/bg]      Stop coloring the background
  *   [&#47*]    Stop all
  *
  * Tags don't have to be closed
  */
object RichText {

  implicit class RichTextHelper(val sc: StringContext) extends AnyVal {
    def r(args: Any*): RichText = {
      val input  = sc.s(args*)
      val result = parse(input, p => richText(using p))
      result match {
        case Parsed.Success(rt, _) => rt
        case _: Parsed.Failure     => RichText(Text(input))
      }
    }
  }

  final case class RichText(instructions: Instruction*)

  sealed trait Instruction
  final case class Text(text: String)                   extends Instruction
  final case class StartAttribute(attribute: Attribute) extends Instruction
  final case class StopAttribute(attribute: Attribute)  extends Instruction
  case object ResetAttributes                           extends Instruction

  sealed trait Attribute
  case object Bold                          extends Attribute
  case object Underline                     extends Attribute
  case object Blink                         extends Attribute
  case object Reverse                       extends Attribute
  case object Foreground                    extends Attribute
  case object Background                    extends Attribute
  final case class Foreground(color: Color) extends Attribute
  final case class Background(color: Color) extends Attribute

  sealed trait Color
  final case class NamedColor(name: String) extends Color
  final case class IndexedColor(code: Int)  extends Color
  final case class HexColor(hex: String)    extends Color

  private def letter[$: P]   = P(CharIn("a-z"))
  private def digit[$: P]    = P(CharIn("0-9"))
  private def hexDigit[$: P] = P(CharIn("0-9", "a-f", "A-F"))

  private def name[$: P]  = P(letter.rep(1).!)
  private def index[$: P] = P(digit.rep(1).!) map (_.toInt)
  private def hex[$: P]   = P(("#" ~/ hexDigit ~/ hexDigit ~/ hexDigit ~/ hexDigit ~/ hexDigit ~/ hexDigit).!)

  private def bold[$: P]       = P("b").map (_ => Bold)
  private def underline[$: P]  = P("u").map (_ => Underline)
  private def blink[$: P]      = P("bl").map (_ => Blink)
  private def reverse[$: P]    = P("r").map (_ => Reverse)
  private def foreground[$: P] = P("fg").map (_ => Foreground)
  private def background[$: P] = P("bg").map (_ => Background)

  private def attribute[$: P] = P(underline | blink | bold | reverse | foreground | background)

  private def namedColor[$: P]   = P(name).map(c => NamedColor(c))
  private def indexedColor[$: P] = P(index).map(c => IndexedColor(c))
  private def hexColor[$: P]     = P(hex).map(c => HexColor(c))

  private def color[$: P] = P(namedColor | indexedColor | hexColor)

  private def startAttribute[$: P] = P(attribute).map(StartAttribute.apply)
  private def beginColor[$: P] = P(("fg" | "bg").! ~ ":" ~/ color) map {
    case ("fg", aColor) => StartAttribute(Foreground(aColor))
    case (_, aColor)    => StartAttribute(Background(aColor))
  }
  private def stop[$: P] = P("/" ~/ ("*".! | attribute)).map {
    case "*"             => ResetAttributes
    case attr: Attribute => StopAttribute(attr)
  }

  private def block[$: P]  = P("[" ~/ (beginColor | startAttribute | stop) ~/ "]")
  private def escape[$: P]: P[Text] = P("[[".!).map (_ => Text("["))
  private def text[$: P]: P[Text]   = P(CharsWhile(c => c != '[').!).map(Text.apply)

  private def richText[$: P]: P[RichText] = P((text | escape | block).rep).map (x => RichText(x*))

}
