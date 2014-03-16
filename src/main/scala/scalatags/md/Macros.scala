package scalatags.md

import scala.language.experimental.macros
import scala.reflect.macros.Context
import scalatags.HtmlTag
import org.pegdown.PegDownProcessor

object Macros {
  def mdImpl(c: Context)(args: c.Expr[HtmlTag]*) = {
    import c.universe._
    val Apply(_, List(Apply(_, partTrees))) = c.prefix.tree
    val parts = partTrees map { case Literal(Constant(s: String)) ⇒ s }
    val placeholders = Stream.from(0) map { i ⇒ ScalatagsVisitor.Fragment(i) }
    val joined = (parts.dropRight(1) zip placeholders).map { case (x, y) ⇒ x + y }.mkString + parts.last
    val parsed = new PegDownProcessor().parseMarkdown(joined.toCharArray)
    val visitor = new ScalatagsVisitor(c.universe)(args.map(_.tree))
    visitor.visit(parsed)
    val result = visitor.result[c.universe.type]
    c.info(c.enclosingPosition, result.toString(), force = false)
    c.Expr[HtmlTag](result)
  }
}
