#set document(
  title: [$title$],
  author: ($for(by-author)$"$it.name.literal$", $endfor$),
)
#show: apa7
#align(center)[
  #set par(first-line-indent: 0pt)
  #strong[$title$]

  #strong[$subtitle$]

  #emph[$for(by-author)$$it.name.literal$$sep$, $endfor$]
]
#v(24pt)
