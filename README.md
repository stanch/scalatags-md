### scalatags-md

```scala
import scalatags.md._

val x = div(span("Hey"))
println(md"Hey, I'm *some* markdown with $x and stuff")
```

This code compiles into the following:

```scala
HtmlTag(
  "p",
  List(
    StringNode(" and stuff"),
    div(span("Hey")),
    StringNode(" markdown with "),
    HtmlTag(
      "em",
      List(
        StringNode("some")
      ),
      SortedMap.empty
    ),
    StringNode("Hey, I\'m ")
  ),
  SortedMap.empty
)
```

Which is basically the same as this:

```scala
p(
  "Hey, I\'m ",
  em("some"),
  " markdown with ",
  div(span("Hey")),
  " and stuff"
)
