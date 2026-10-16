#let apa7(doc) = {
  set page(
    paper: "us-letter",
    margin: 1in,
    header: context {
      set par(first-line-indent: 0pt)
      [SLEEP VARIABILITY AND MENTAL HEALTH #h(1fr) #counter(page).display("1")]
    },
    header-ascent: 0.5in,
    footer: none,
  )
  set text(
    size: 12pt,
    top-edge: 0.8em,
    bottom-edge: -0.2em,
    hyphenate: false,
  )
  set par(
    leading: 1em,
    spacing: 1em,
    first-line-indent: (amount: 0.5in, all: true),
    justify: false,
  )
  show math.equation: it => {
    show regex("[Α-Ωα-ωϑϕϖϱϵ]"): math.upright
    it
  }
  set heading(numbering: none)
  show heading: set text(size: 12pt, weight: "bold")
  show heading: set par(first-line-indent: 0pt)
  show heading: set block(above: 12pt, below: 12pt)
  show heading.where(level: 1): it => align(
    if it.body == [Conclusion] { left } else { center }, it
  )
  show heading.where(level: 2): set align(left)
  show heading.where(level: 3): set align(left)
  show heading.where(level: 3): set text(style: "italic")
  show heading.where(level: 4): it => {
    h(0.5in)
    strong(it.body + [.])
    h(0.25em)
  }
  show heading.where(level: 5): it => {
    h(0.5in)
    strong(emph(it.body + [.]))
    h(0.25em)
  }
  show figure.where(kind: table): set figure.caption(position: top)
  show figure.where(kind: image): set figure.caption(position: top)
  show figure.where(kind: image): set block(breakable: true)
  show figure.caption: it => {
    set align(left)
    set par(first-line-indent: 0pt)
    if it.supplement == [Figure] {
      strong(it.supplement + [ ] + it.counter.display(it.numbering) + [. ] + it.body)
    } else {
      strong(it.supplement + [ ] + it.counter.display(it.numbering))
      linebreak()
      emph(it.body)
    }
  }
  show quote.where(block: true): set block(inset: (left: 0.5in))
  show quote.where(block: true): set par(first-line-indent: 0pt)
  show <refs>: set par(first-line-indent: 0pt, hanging-indent: 0.5in)
  show <refs>: set block(above: 12pt, below: 12pt)
  doc
}
