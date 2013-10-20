#lang pyret

check:
  fun four-args(a, b, c, d): a + b + c + d end
  four-args(_, _, 5, _)(_, _, 3)(_, 2)(1) is 11
  plus = _ + _
  x-plus-4 = (_ + 4)
  four-plus-x = (4 + _)
  plus(3,4) is 7
  x-plus-4(10) is 14
  four-plus-x(6) is 10
  (not _)(true) is false
  (_ == 4)(4) is true
  (_ == 4)(5) is false
  (_ <> 4)(5) is true
end
