package scalatags.md

import org.scalatest.FlatSpec
import scalatags.Tags._

class MarkdownSpec extends FlatSpec {
  val x = div(span("Hey"))
  val y = md"""
  Hey, I'm *some* markdown with $x and stuff

  Another paragraph
  """
  val z = div(x, y)
  println(z)
}
