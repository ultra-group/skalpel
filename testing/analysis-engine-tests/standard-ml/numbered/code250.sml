(* old test case name: code250.sml *)

(* Type name arity clash (via structures and signatures).
 *
 * The type function u is declared in the signature as taking zero
 * type parameters, but an attempt is made to define u in the
 * structure by copying the type function t which takes 1 argument 'a.
 *
 * When the type error slicer is run, the space where the type
 * parameter of u is missing in the signature will be highlighted with
 * one of the endpoint colors, and the (conflicting) single type
 * parameter of the definition of t will have a box around it with the
 * other endpoint color.
 *)

signature S =
sig
    datatype 'a t = T1 of int | T2 of 'a
    type u
end;

structure st1 : S =
struct
datatype 'a t = T1 of int | T2 of 'a
datatype u = datatype t
end;

structure st2 : S = st1;;
