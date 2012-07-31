(* old test case name: code290.sml *)

structure S : sig
    exception Ex of int * bool
end =
struct exception Ex of bool * bool end
