#lang pyret

import profile as p

fun build-some-lists(n):
  if (n <= 0): []
  else if n > 0: list.link(list.range(0, 100), build-some-lists(n - 1))
  end
end

fun discard(x):
end

for p.time():
  discard(build-some-lists(2000))
end
"done"