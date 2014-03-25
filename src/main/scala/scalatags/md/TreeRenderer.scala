package scalatags.md

import scala.reflect.api.Universe
import laika.tree.Elements
import laika.tree.Elements._
import laika.tree.Templates._
import scala.collection.SortedMap
import scala.quasiquotes.StandardLiftables

object FragmentLink {
  def apply(i: Int) = s"[dummy](%%fragment%%$i)"
  def unapply(s: String): Option[Int] = if (s.startsWith("%%fragment%%")) Some(s.substring("%%fragment%%".length).toInt) else None
}

class TreeRenderer(val universe: Universe)(_parts: Seq[Universe#Tree]) extends StandardLiftables {
  val u: universe.type = universe
  import universe.{ Block ⇒ _, _ }

  // need to use _root_ to be extra sure that names binds to the right definition
  val scalatags: Tree = q"_root_.scalatags"

  // This hack is due to inability to use universe.Type in class constructor
  val parts = _parts.asInstanceOf[Seq[Tree]]

  // meanwhile real liftables do not work (?)
  // https://groups.google.com/forum/#!topic/scala-language/BM1EPrZY3Hk

  implicit val `Tree is liftable` = Liftable[Tree](identity)
  implicit def `SortedMap is liftable`[A: Liftable, B: Liftable] = Liftable[SortedMap[A, B]] { map ⇒
    q"_root_.scala.collection.SortedMap(..${map.map { case (k, v) ⇒ q"($k, $v)" }.toList})"
  }

  object Symbols {
    // TODO: move all symbols here
  }

  def render(element: Element): Tree = element match {
    case e: SystemMessage ⇒ ???
    case e: Table ⇒ ???
    case e: TableElement ⇒ ???
    case e: Reference ⇒ ???
    case e: Invalid[_] ⇒ ???
    case e: BlockContainer[_] ⇒ renderBlockContainer(e)
    case e: SpanContainer[_] ⇒ renderSpanContainer(e)
    case e: ListContainer[_] ⇒ renderListContainer(e)
    case e: TextContainer ⇒ renderTextContainer(e)
    case e: Block ⇒ renderSimpleBlock(e)
    case e: Span ⇒ renderSimpleSpan(e)
    case unknown ⇒ ???
  }

  object WithFallback {
    def unapply(value: Element) = value match {
      case f: Fallback ⇒ Some(f.fallback)
      case _ ⇒ None
    }
  }

  def renderBlockContainer[T <: BlockContainer[T]](con: BlockContainer[T]): Tree = {
    def quotedBlockContent(content: Seq[Block], attr: Seq[Span]) =
      if (attr.isEmpty) content
      else content :+ Paragraph(attr, Styles("attribution"))

    def figureContent(img: Span, caption: Seq[Span], legend: Seq[Block]): List[Block] =
      List(SpanSequence(List(img)), Paragraph(caption, Styles("caption")), BlockSequence(legend, Styles("legend")))

    con match {
      case RootElement(content) ⇒
        q"$scalatags.md.GroupNode(${content.map(render).toList.reverse})"

      case EmbeddedRoot(content, indent, _) ⇒ ??? //out.indented(indent) { if (content.nonEmpty) out << content.head <<| content.tail }
      case Section(header, content, _) ⇒ ??? //out << header <<| content
      case TitledBlock(title, content, opt) ⇒ ??? //out <<@ ("div", opt) <<|> (Paragraph(title, Styles("title")) +: content) <<| "</div>"
      case QuotedBlock(content, attr, opt) ⇒
        ??? //out <<@ ("blockquote", opt); renderBlocks(quotedBlockContent(content, attr), "</blockquote>")
      case BulletListItem(content, _, opt) ⇒
        ??? //out <<@ ("li", opt); renderBlocks(content, "</li>")
      case EnumListItem(content, _, _, opt) ⇒
        ??? //out <<@ ("li", opt); renderBlocks(content, "</li>")
      case DefinitionListItem(term, defn, _) ⇒
        ??? //out << "<dt>" << term << "</dt>" <<| "<dd>"; renderBlocks(defn, "</dd>")
      case LineBlock(content, opt) ⇒ ??? //out <<@ ("div", opt + Styles("line-block")) <<|> content <<| "</div>"
      case Figure(img, caption, legend, opt) ⇒ ??? //out <<@ ("div", opt + Styles("figure")) <<|> figureContent(img, caption, legend) <<| "</div>"

      case Footnote(label, content, opt) ⇒ ??? //renderTable(toTable(label, content, opt + Styles("footnote")))
      case Citation(label, content, opt) ⇒ ??? //renderTable(toTable(label, content, opt + Styles("citation")))

      case WithFallback(fallback) ⇒ ??? //out << fallback
      case c: Customizable ⇒ c match {
        case BlockSequence(content, NoOpt) ⇒ ??? //if (content.nonEmpty) out << content.head <<| content.tail // this case could be standalone above, but triggers a compiler bug then
        case _ ⇒ ??? //out <<@ ("div", c.options) <<|> c.content <<| "</div>"
      }
      case unknown ⇒ ??? //out << "<div>" <<|> unknown.content <<| "</div>"
    }
  }

  def renderSpanContainer[T <: SpanContainer[T]](con: SpanContainer[T]): Tree = {
    def escapeTitle(s: String) = s.replace("&", "&amp;").replace("\"", "&quot;").replace("'", "$#39;")
    def codeStyles(language: String) = if (language.isEmpty) Styles("code") else Styles("code", language)
    def crossLinkRef(path: PathInfo, ref: String) = {
      val target = path.relative.name.lastIndexOf(".") match {
        case -1 ⇒ path.relative.toString
        case i ⇒ (path.relative.parent / (path.relative.name.take(i) + ".html")).toString
      }
      if (ref.isEmpty) target else s"$target#$ref"
    }

    con match {
      case Paragraph(content, opt) ⇒
        q"$scalatags.Tags.p(${content.map(render).toList})"
      case Emphasized(content, opt) ⇒
        q"$scalatags.Tags.em(${content.map(render).toList})"
      case Strong(content, opt) ⇒
        q"$scalatags.Tags.strong(${content.map(render).toList})"

      case ParsedLiteralBlock(content, opt) ⇒ ??? //out <<@ ("pre", opt) << "<code>" <<< content << "</code></pre>"
      case CodeBlock(lang, ontent, opt) ⇒ ??? //out <<@ ("pre", opt + codeStyles(lang)) << "<code>" <<< content << "</code></pre>"
      case Code(lang, content, opt) ⇒ ??? //out <<@ ("code", opt + codeStyles(lang)) << content << "</code>"
      case Line(content, opt) ⇒ ??? //out <<@ ("div", opt + Styles("line")) << content << "</div>"

      case Header(level, content, opt) ⇒
        val tag = newTermName(s"p$level")
        q"""$scalatags.Tags.$tag(${content.map(render).toList})"""

      case ExternalLink(content, FragmentLink(i), title, opt) ⇒ parts(i)

      case ExternalLink(content, url, title, opt) ⇒ ??? //out <<@ ("a", opt, "href" -> url, "title" -> title.map(escapeTitle)) << content << "</a>"
      case InternalLink(content, ref, title, opt) ⇒ ??? //out <<@ ("a", opt, "href" -> ("#" + ref), "title" -> title.map(escapeTitle)) << content << "</a>"
      case CrossLink(content, ref, path, title, opt) ⇒ ??? //out <<@ ("a", opt, "href" -> (crossLinkRef(path, ref)), "title" -> title.map(escapeTitle)) << content << "</a>"

      case WithFallback(fallback) ⇒ ??? //out << fallback
      case c: Customizable ⇒ c match {
        case SpanSequence(content, NoOpt) ⇒ ??? //out << content // this case could be standalone above, but triggers a compiler bug then
        case TemplateRoot(content, NoOpt) ⇒ ??? //out << content
        case TemplateSpanSequence(content, NoOpt) ⇒ ??? //out << content
        case _ ⇒ ??? //out <<@ ("span", c.options) << c.content << "</span>"
      }
      case unknown ⇒ ??? //out << "<span>" << unknown.content << "</span>"
    }
  }

  def renderListContainer[T <: ListContainer[T]](con: ListContainer[T]): Tree = con match {
    case EnumList(content, format, start, opt) ⇒
      ??? //out <<@ ("ol", opt, ("class", format.enumType.toString.toLowerCase), ("start", noneIfDefault(start, 1))) <<|> content <<| "</ol>"
    case BulletList(content, _, opt) ⇒ ??? //out <<@ ("ul", opt) <<|> content <<| "</ul>"
    case DefinitionList(content, opt) ⇒ ??? //out <<@ ("dl", opt) <<|> content <<| "</dl>"

    case WithFallback(fallback) ⇒ ??? //out << fallback
    case c: Customizable ⇒ ??? //out <<@ ("div", c.options) <<|> c.content <<| "</div>"
    case unknown ⇒ ??? //out << "<div>" <<|> unknown.content <<| "</div>"
  }

  def renderTextContainer(con: TextContainer): Tree = con match {
    case Text(content, opt) ⇒ opt match {
      case NoOpt ⇒ q"$scalatags.StringNode($content)" //out <<& content
      case _ ⇒ ??? //out <<@ ("span", opt) <<& content << "</span>"
    }
    case TemplateString(content, opt) ⇒ ???
    case RawContent(formats, content, opt) ⇒ ??? /*if (formats.contains("html")) {
      opt match {
        case NoOpt ⇒ out << content
        case _ ⇒ out <<@ ("span", opt) << content << "</span>"
      }
    }*/
    case Elements.Literal(content, opt) ⇒
      q"$scalatags.Tags.code($scalatags.RawNode($content))"

    case LiteralBlock(content, opt) ⇒ ??? //out <<@ ("pre", opt) << "<code>" <<<& content << "</code></pre>"
    case Comment(content, opt) ⇒ ??? //out << "<!-- " << content << " -->"

    case WithFallback(fallback) ⇒ ??? //out << fallback
    case c: Customizable ⇒ ??? //out <<@ ("span", c.options) << c.content << "</span>"
    case unknown ⇒ ??? //out <<& unknown.content
  }

  def renderSimpleBlock(block: Block) = block match {
    case Rule(opt) ⇒ ??? //out <<@ ("hr", opt)
    case InternalLinkTarget(opt) ⇒ ??? //out <<@ ("a", opt) << "</a>"

    case WithFallback(fallback) ⇒ ??? //out << fallback
    case unknown ⇒ ??? //()
  }

  def renderSimpleSpan(span: Span) = span match {
    case CitationLink(ref, label, opt) ⇒ ??? //out <<@ ("a", opt + Styles("citation"), "href" -> ("#" + ref)) << "[" << label << "]</a>"
    case FootnoteLink(ref, label, opt) ⇒ ??? //out <<@ ("a", opt + Styles("footnote"), "href" -> ("#" + ref)) << "[" << label << "]</a>"
    case Image(text, uri, title, opt) ⇒ ??? //out <<@ ("img", opt, "src" -> uri.uri, "alt" -> text, "title" -> title)
    case LineBreak(opt) ⇒ ??? //out << "<br>"
    case TemplateElement(elem, indent, _) ⇒ ??? //out.indented(indent) { out << elem }

    case WithFallback(fallback) ⇒ ??? //out << fallback
    case unknown ⇒ ??? //()
  }
}
