#lang pyret

try:
  print(raise("oops"))
except(e):
  print(e)
end