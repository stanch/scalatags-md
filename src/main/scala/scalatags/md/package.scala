package scalatags

import scala.language.experimental.macros

package object md {
  case class GroupNode(children: List[HtmlTag]) extends Node {
    def writeTo(strb: StringBuilder) = children.reverse.foreach(_.writeTo(strb))
  }

  implicit class MarkdownStringContext(val sc: StringContext) extends AnyVal {
    def md(args: HtmlTag*) = macro Macros.mdImpl
  }
}
