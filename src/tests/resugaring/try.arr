#lang pyret

try:
  print(raise("oops" + "?"))
except(e):
  print("It's fine")
end