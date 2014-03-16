package scalatags.md

import org.scalatest.FlatSpec
import scalatags.Tags._

class MarkdownSpec extends FlatSpec {
  val x = div(span("Hey"))
  println(md"Hey, I'm *some* markdown with $x and stuff")
}
