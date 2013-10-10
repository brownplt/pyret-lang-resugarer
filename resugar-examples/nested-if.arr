#lang pyret

if false:
  print("one")
else if false:
  print("two")
else if true:
  if false:
    print("three")
  else if false:
    print("four")
  end
end