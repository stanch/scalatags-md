package scalatags

import scala.language.experimental.macros

package object md {
  implicit class MarkdownStringContext(val sc: StringContext) extends AnyVal {
    def md(args: HtmlTag*) = macro Macros.mdImpl
  }
}
