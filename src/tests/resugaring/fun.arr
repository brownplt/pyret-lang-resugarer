#lang pyret

fun length(lst):
  if is-empty(lst): 0
  else: length(lst.rest) + 1
  end
end

length([1, 2, 3])