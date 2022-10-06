open Constants;;

open Types;;

(*
  Returns true if s1 is flanking s2.
*)
let is_soldier_flanked cover s1 s2 =
  if (s1.x < s2.x and s1.y = s2.y) then
    find_cover cover (s2.x - 1) s2.y
  else if (s1.x < s2.x and s1.y < s2.y) then
    ((find_cover cover (s2.x - 1) s2.y) or (find_cover cover s2.x (s2.y - 1)))
  else if (s1.x = s2.x and s1.y < s2.y) then

  else if (s1.x > s2.x and s1.y < s2.y) then

  else if (s1.x > s2.x and s1.y = s2.y) then

  else if (s1.x > s2.x and s1.y > s2.y) then

  else if (s1.x = s2.x and s1.y > s2.y) then

  else if (s1.x < s2.x and s1.y > s2.y) then

