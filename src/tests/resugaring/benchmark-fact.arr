#lang pyret

import profile as p

fun factorial(n):
  if (n <= 0): 1
  else: n * factorial(n - 1)
  end
end

for p.time():
  _ = factorial(10000)
  "done"
end
