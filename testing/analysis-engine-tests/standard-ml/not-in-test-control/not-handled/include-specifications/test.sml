(* contains an error which is *not detected*
 * Signature S3 includes both S and S2, but S and S2 both declare types 'a' and 'b'
 * We should generate an error for this. *)


signature S = sig
    type a
end

signature S2 = sig
    type a
    include S
end

