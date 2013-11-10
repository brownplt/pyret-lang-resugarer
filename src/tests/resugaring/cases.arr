#lang pyret

cases(List) [2]:
  | empty => print("empty")
  | link(something, _) => print(something + 1)
end