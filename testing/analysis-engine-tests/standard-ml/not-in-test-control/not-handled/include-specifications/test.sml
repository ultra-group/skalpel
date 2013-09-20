(* contains an error which is *not detected*
 * Signature S3 includes both S and S2, but S and S2 both declare types 'a' and 'b'
 * We should generate an error for this. *)


signature S = sig
    type a
    type b
end

signature S2 = sig
    include S
    type c
end

signature S3 = sig
    include S
    include S2
    type d
end

structure test : S3 =
struct
    type a = int
    type b = int
    type c = int
    type d = int
end
