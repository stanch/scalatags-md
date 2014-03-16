package scalatags.md

import org.pegdown.ast._
import scalatags.{ HtmlTag, StringNode, RawNode }
import scalatags.Tags._
import scala.collection.JavaConversions._
import scala.collection.SortedMap
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

  /** A continuation that produces a Tree from a scalatags.HtmlTag, given its children */
  case class TagTree(children: List[Tree], f: List[Tree] ⇒ Tree) {
    def make = f(children)
  }

  object TagTree {
    def apply(tag: HtmlTag) = new TagTree(Nil, { ch ⇒
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
  var stack: List[TagTree] = List(TagTree(div()))

  /** The result of "visiting" */
  // This is hacky to please the path-dependent types
  def result[U <: Universe] = stack.head.children.head.asInstanceOf[U#Tree]

  /** Update the top of the stack by adding a new Tree to its head */
  def update(add: Tree) = {
    val head :: tail = stack
    stack = head.copy(children = add :: head.children) :: tail
  }

  // These three methods are equivalent to print, printTag & co
  // at [https://github.com/sirthias/pegdown/blob/master/src/main/java/org/pegdown/ToHtmlSerializer.java]

  /** Add text */
  def addText(content: String) = {
    update(makeTree(StringNode(content)))
  }

  /** Add raw */
  def addRaw(content: String) = {
    update(makeTree(RawNode(content)))
  }

  /** Add a tree */
  def addTree(tree: Tree) = {
    update(tree)
  }

  /** Add text tag */
  def addTag(node: TextNode, tag: String) = {
    update(makeTree(HtmlTag(tag, StringNode(node.getText) :: Nil, SortedMap.empty)))
  }

  /** Add tag node */
  def addTag(node: SuperNode, tag: String) = {
    // push
    stack ::= TagTree(HtmlTag(tag, Nil, SortedMap.empty))
    // recurse
    visitChildren(node)
    // pop
    val head :: tail = stack
    stack = tail
    // insert into the top
    update(head.make)
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
