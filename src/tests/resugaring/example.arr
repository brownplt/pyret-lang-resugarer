#lang pyret

fun len(x):
  cases(List) x:
    | empty => 0
    | link(_, tail) => len(tail) + 1
  end
end

len([1, 2])