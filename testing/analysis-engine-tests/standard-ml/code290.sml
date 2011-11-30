structure S : sig
    exception Ex of int * bool
end =
struct exception Ex of bool * bool end
