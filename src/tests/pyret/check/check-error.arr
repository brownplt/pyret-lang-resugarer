#lang pyret

fun f():

where:
  checkers.check-equals("0=0",0, 0)
  checkers.check-equals("0 = 1", 0, 1)
  raise("Done checking")
  checkers.check-equals("1=1",1, 1)
end
