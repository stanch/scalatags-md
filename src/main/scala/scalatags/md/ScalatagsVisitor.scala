package scalatags.md

import org.pegdown.ast._
import scalatags.{ HtmlTag, StringNode, RawNode }
import scala.collection.JavaConversions._
import scala.collection.{mutable, SortedMap}
import scala.reflect.api.Universe
import org.pegdown.LinkRenderer

object ScalatagsVisitor {
  object Fragment {
    def apply(i: Int) = s"[dummy](%%fragment%%$i)"
    def unapply(s: ExpLinkNode): Option[Int] = if (s.url.startsWith("%%fragment%%")) Some(s.url.substring("%%fragment%%".length).toInt) else None
  }
}

class ScalatagsVisitor(val universe: Universe)(_parts: Seq[Universe#Tree]) extends Visitor {
  import universe._

  // This hack is due to inability to use universe.Type in class constructor
  val parts = _parts.asInstanceOf[Seq[Tree]]

  // meanwhile real liftables do not work (?)
  // https://groups.google.com/forum/#!topic/scala-language/BM1EPrZY3Hk
  object Liftable {
    def apply(list: List[Tree]) =
      q"scala.collection.immutable.List(..$list)"

    def apply(map: SortedMap[String, String]) =
      q"scala.collection.SortedMap(..${map.map { case (k, v) ⇒ q"($k, $v)"}.toList: List[Tree]})"
  }

  /** A continuation that produces a Tree, given its children */
  case class StackEntry(children: List[Tree], f: List[Tree] ⇒ Tree) {
    def makeTree = f(children)
  }

  object StackEntry {
    def apply(tag: HtmlTag) = new StackEntry(Nil, { ch ⇒
      q"scalatags.HtmlTag(${tag.tag}, ${Liftable(ch)}, ${Liftable(tag.attrs)})"
    })

    def apply(node: GroupNode) = new StackEntry(Nil, { ch ⇒
      q"scalatags.md.GroupNode(${Liftable(ch)})"
    })
  }

  /** Make a Tree from an arbitrary scalatags.Node */
  def makeTree(node: scalatags.Node): Tree = node match {
    case HtmlTag(tag, children, attrs, _) ⇒
      q"scalatags.HtmlTag($tag, ${Liftable(children.map(makeTree))}, ${Liftable(attrs)})"
    case RawNode(v) ⇒
      q"scalatags.RawNode($v)"
    case StringNode(v) ⇒
      q"scalatags.StringNode($v)"
  }

  /** Stack of tags being visited */
  val stack = mutable.Stack(StackEntry(GroupNode(Nil)))

  /** The result of "visiting" */
  // This is hacky to please the path-dependent types
  def result[U <: Universe] = stack.head.makeTree.asInstanceOf[U#Tree]

  /** Append a Tree to the top of the stack */
  def append(tree: Tree) = {
    val top = stack.pop()
    stack.push(top.copy(children = tree :: top.children))
  }

  // These methods are equivalent to print, printTag & co
  // at [https://github.com/sirthias/pegdown/blob/master/src/main/java/org/pegdown/ToHtmlSerializer.java]

  /** Add text */
  def addText(content: String) = {
    append(makeTree(StringNode(content)))
  }

  /** Add raw */
  def addRaw(content: String) = {
    append(makeTree(RawNode(content)))
  }

  /** Add a tree */
  def addTree(tree: Tree) = {
    append(tree)
  }

  /** Add text tag */
  def addTag(node: TextNode, tag: String) = {
    append(makeTree(HtmlTag(tag, StringNode(node.getText) :: Nil, SortedMap.empty)))
  }

  /** Add tag node */
  def addTag(node: SuperNode, tag: String) = {
    stack.push(StackEntry(HtmlTag(tag, Nil, SortedMap.empty)))
    visitChildren(node)
    val tree = stack.pop().makeTree
    append(tree)
  }

  def addImg(rendering: LinkRenderer.Rendering) = {
    append(makeTree(HtmlTag("img", Nil, SortedMap(
      ("src", rendering.href) +:
      ("alt", rendering.text) +:
      rendering.attributes.map(a ⇒ (a.name, a.value)).toSeq: _*
    ))))
  }

  protected def addLink(rendering: LinkRenderer.Rendering) = {
    append(makeTree(HtmlTag("a", StringNode(rendering.text) :: Nil, SortedMap(
      ("href", rendering.href) +:
      rendering.attributes.map(a ⇒ (a.name, a.value)).toSeq: _*
    ))))
  }

  val linkRenderer = new LinkRenderer

  // The rest is an approximation of what is found
  // at [https://github.com/sirthias/pegdown/blob/master/src/main/java/org/pegdown/ToHtmlSerializer.java]

  def visitChildren(node: Node) = node.getChildren.foreach(_.accept(this))

  def visit(node: Node): Unit = ???

  def visit(node: SuperNode) = visitChildren(node)

  def visit(node: TextNode) = addText(node.getText)

  def visit(node: WikiLinkNode) = addLink(linkRenderer.render(node))

  def visit(node: VerbatimNode): Unit = ???

  def visit(node: TableRowNode): Unit = ???

  def visit(node: TableNode): Unit = ???

  def visit(node: TableHeaderNode): Unit = ???

  def visit(node: TableColumnNode): Unit = ???

  def visit(node: TableCellNode): Unit = ???

  def visit(node: TableCaptionNode): Unit = ???

  def visit(node: TableBodyNode): Unit = ???

  def visit(node: StrongEmphSuperNode) = if(node.isClosed) {
    if(node.isStrong) {
      addTag(node, "strong")
    } else {
      addTag(node, "em")
    }
  } else {
    addText(node.getChars)
    visitChildren(node)
  }

  def visit(node: StrikeNode) = addTag(node, "del")

  def visit(node: SpecialTextNode) = addRaw(node.getText)

  def visit(node: SimpleNode) = node.getType match {
    case SimpleNode.Type.Apostrophe ⇒ addRaw("&rsquo;")
    case SimpleNode.Type.Ellipsis ⇒ addRaw("&hellip;")
    case SimpleNode.Type.Emdash ⇒ addRaw("&mdash;")
    case SimpleNode.Type.Endash ⇒ addRaw("&ndash;")
    case SimpleNode.Type.HRule ⇒ addRaw("<hr/>")
    case SimpleNode.Type.Linebreak ⇒ addRaw("<br/>")
    case SimpleNode.Type.Nbsp ⇒ addRaw("&nbsp;")
    case _ ⇒ ???
  }

  def visit(node: RootNode) = visitChildren(node)

  def visit(node: RefLinkNode): Unit = ???

  def visit(node: RefImageNode): Unit = ???

  def visit(node: ReferenceNode): Unit = ???

  def visit(node: QuotedNode) = node.getType match {
    case QuotedNode.Type.DoubleAngle ⇒
      addRaw("&laquo;")
      visitChildren(node)
      addRaw("&raquo;")
    case QuotedNode.Type.Double ⇒
      addRaw("&ldquo;")
      visitChildren(node)
      addRaw("&rdquo;")
    case QuotedNode.Type.Single ⇒
      addRaw("&lsquo;")
      visitChildren(node)
      addRaw("&rsquo;")
  }

  def visit(node: ParaNode) = addTag(node, "p")

  def visit(node: OrderedListNode) = addTag(node, "ol")

  def visit(node: MailLinkNode) = addLink(linkRenderer.render(node))

  def visit(node: ListItemNode) = addTag(node, "li")

  def visit(node: InlineHtmlNode) = addRaw(node.getText)

  def visit(node: HtmlBlockNode) = addRaw(node.getText)

  def visit(node: HeaderNode): Unit = ???

  def visit(node: ExpLinkNode) = node match {
    case ScalatagsVisitor.Fragment(index) ⇒ addTree(parts(index))
    case _ ⇒ ???
  }

  def visit(node: ExpImageNode): Unit = ???

  def visit(node: DefinitionTermNode): Unit = ???

  def visit(node: DefinitionNode): Unit = ???

  def visit(node: DefinitionListNode): Unit = ???

  def visit(node: CodeNode): Unit = ???

  def visit(node: BulletListNode): Unit = ???

  def visit(node: BlockQuoteNode): Unit = ???

  def visit(node: AutoLinkNode): Unit = ???

  def visit(node: AbbreviationNode): Unit = ???
}
