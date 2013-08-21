#lang ragg

term: (node | list | STRING | number) [tags]
number : NUMBER
node: LABEL "(" terms ")"
list: "[" terms "]"
terms: [ term ("," term)* ]

tags: "{" "[" tag ("," tag)* "]" "}"
tag: "Head" "(" LABEL "," NUMBER "," term ")"
     | "Body" | "Alien"