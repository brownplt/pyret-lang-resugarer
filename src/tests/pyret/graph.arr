#lang pyret

check:
  graph:
  BOS = [PVD, WOR]
  WOR = [BOS]
  PVD = [BOS]
  end
  BOS.first is PVD
  BOS.rest.first is WOR
  WOR.first is BOS
  PVD.first is BOS

  tostring(BOS) is "[cyclic-field, cyclic-field]"
  print(torepr(BOS))
end

check:
data Loc deriving builtins.Eq:
     | loc(name :: String, latitude :: Number, longitude :: Number, 
       neighbors :: list.List, distances :: list.List)
end

  graph:
  tl1 = loc("a", 0, 0, [tl2, tl4, tl5], [10, 8, 21])
  tl2 = loc("b", 8, 6, [tl1, tl3, tl4, tl5], [10, 13, 6, 17])
  tl3 = loc("c", 13, 18, [tl2], [13])
  tl4 = loc("d", 8, 0, [tl1, tl2], [8, 6])
  tl5 = loc("e", 0, 21, [tl1, tl2], [21, 17])
  end
  tl1 is tl1
  tl1.neighbors.first is tl2
  torepr(tl1) is 'loc("a", 0, 0, [cyclic-field, cyclic-field, cyclic-field], [10, 8, 21])'
  String(tostring(tl1)) is true
  String(torepr(tl2)) is true
  String(tostring(tl2)) is true
end
