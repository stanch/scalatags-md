package scalatags.md

import scala.language.experimental.macros
import scala.reflect.macros.Context
import scalatags.HtmlTag
import laika.parse.markdown.Markdown
import laika.io.Input

object Macros {
  def mdImpl(c: Context)(args: c.Expr[HtmlTag]*) = {
    import c.universe._
    val Apply(_, List(Apply(_, partTrees))) = c.prefix.tree
    val parts = partTrees map { case Literal(Constant(s: String)) ⇒ s }
    val placeholders = Stream.from(0) map { i ⇒ FragmentLink(i) }
    val joined = (parts.dropRight(1) zip placeholders).map { case (x, y) ⇒ x + y }.mkString + parts.last
    val parsed = Markdown.strict.newParser(Input.fromString(joined)).content
    val result = new TreeRenderer(c.universe)(args.map(_.tree)).render(parsed).asInstanceOf[Tree]
    c.info(c.enclosingPosition, result.toString(), force = false)
    c.Expr[GroupNode](result)
  }
}
