#lang pyret

data D:
	| foo(a, b :: Number)
	| bar(c)
end

cases(D) bar(2):
	| foo(a, b :: String) =>
          print("matched")
          5 + "asdf"
end
