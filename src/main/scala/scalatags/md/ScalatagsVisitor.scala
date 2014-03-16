package scalatags.md

import org.pegdown.ast._
import scalatags.{ HtmlTag, StringNode, RawNode }
import scalatags.Tags._
import scala.collection.JavaConversions._
import scala.collection.{mutable, SortedMap}
import scala.reflect.api.Universe

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

  /** A continuation that produces a Tree, given its children */
  case class StackEntry(children: List[Tree], f: List[Tree] ⇒ Tree) {
    def makeTree = f(children)
  }

  object StackEntry {
    def apply(tag: HtmlTag) = new StackEntry(Nil, { ch ⇒
      q"scalatags.HtmlTag(${tag.tag}, scala.collection.immutable.List(..$ch), scala.collection.SortedMap.empty)"
    })
  }

  /** Make a Tree from an arbitrary scalatags.Node */
  def makeTree(node: scalatags.Node): Tree = node match {
    case HtmlTag(tag, children, _, _) ⇒
      q"scalatags.HtmlTag($tag, scala.collection.immutable.List(..${children.map(makeTree): List[Tree]}), scala.collection.SortedMap.empty, false)"
    case RawNode(v) ⇒
      q"scalatags.RawNode($v)"
    case StringNode(v) ⇒
      q"scalatags.StringNode($v)"
  }

  /** Stack of tags being visited */
  val stack = mutable.Stack(StackEntry(div()))

  /** The result of "visiting" */
  // This is hacky to please the path-dependent types
  def result[U <: Universe] = stack.head.children.head.asInstanceOf[U#Tree]

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

  // The rest is an approximation of what is found
  // at [https://github.com/sirthias/pegdown/blob/master/src/main/java/org/pegdown/ToHtmlSerializer.java]

  def visitChildren(node: Node) = node.getChildren.foreach(_.accept(this))

  def visit(node: Node): Unit = ???

  def visit(node: SuperNode) = visitChildren(node)

  def visit(node: TextNode) = addText(node.getText)

  def visit(node: WikiLinkNode): Unit = ???

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

  def visit(node: StrikeNode): Unit = ???

  def visit(node: SpecialTextNode) = addRaw(node.getText)

  def visit(node: SimpleNode): Unit = ???

  def visit(node: RootNode) = visitChildren(node)

  def visit(node: RefLinkNode): Unit = ???

  def visit(node: RefImageNode): Unit = ???

  def visit(node: ReferenceNode): Unit = ???

  def visit(node: QuotedNode): Unit = ???

  def visit(node: ParaNode) = addTag(node, "p")

  def visit(node: OrderedListNode): Unit = ???

  def visit(node: MailLinkNode): Unit = ???

  def visit(node: ListItemNode): Unit = ???

  def visit(node: InlineHtmlNode): Unit = ???

  def visit(node: HtmlBlockNode): Unit = ???

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
