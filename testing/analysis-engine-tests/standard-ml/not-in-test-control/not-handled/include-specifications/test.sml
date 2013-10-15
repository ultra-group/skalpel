(* contains an error which is *not detected*
 * Signature S3 includes both S and S2, but S and S2 both declare types 'a' and 'b'
 * We should generate an error for this. *)


signature S = sig
    type a = int
    type a = bool
    type b = string
    type c = bool and a = int
    eqtype z
end
