(** Type constructor clash between bool and int not detected. *)

signature S = sig
    type a
    type b
end

signature S2 = sig
    include S
    type c
end where type a = int

signature S3 = sig
    include S2
    type d
end

structure test : S2 =
struct
    type a = bool
    type b = int
    type c = int
    type d = int
end
