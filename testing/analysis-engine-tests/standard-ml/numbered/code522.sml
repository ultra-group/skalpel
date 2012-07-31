(* old test case name: code522.sml *)

(* Type constructor clash involving a twisty chain of openings of structures.
 *)

structure S = struct
    structure Y = struct
        structure A = struct val x = [false] end
        structure X = struct val x = false end
	structure M = struct val x = true end
    end
    open Y
    val m = M.x
    val x = [if m then true else false]
end
structure T = struct
    structure X = struct val x = nil end
    open S
    open X
    (* 1 possible fix: replace the 2 lines above by the line below *)
    (* open S X *)
    val y = if m then [true] else x
end
