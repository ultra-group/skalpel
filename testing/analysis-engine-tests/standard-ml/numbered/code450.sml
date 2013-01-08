(* old test case name: code450.sml *)

(* Untypable - Chain of opens *)
structure S = struct
    structure Y = struct
        structure A = struct val x = false end
        structure X = struct val x = false end
	structure M = struct val x = true  end
    end
    open Y
    val m = M.x
    val x = if m then true else false
end
structure T = struct
    structure X = struct val x = 1 end
    open S
    open X
    (*open S X*)
    val y = if m then 1 else x
end;
