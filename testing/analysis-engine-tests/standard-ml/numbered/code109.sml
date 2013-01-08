(* old test case name: code109.sml *)

datatype t = xs of int
exception lkupex
fun lkup _ [] = raise lkupex
  | lkup id ((x as (y, z)) :: xs) =
    if id = y
    then (z, xs)
    else let val (idz, xss) = lkup id xs in (idz, x :: xss) end;
