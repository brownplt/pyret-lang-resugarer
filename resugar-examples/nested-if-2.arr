#lang pyret

if false:
  print("one")
else if false:
  print("two")
else if true:
  if true:
    print("three")
  else if false:
    print("four")
  end
end