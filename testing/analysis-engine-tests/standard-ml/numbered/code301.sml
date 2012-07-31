(* old test case name: code301.sml *)

(* S1 is untypable, but S2 is typable. *)
structure S1 = struct
structure S = struct datatype 'a list2 = nil2 | cons2 of 'a * 'a list2 end;
val _ = (fn nil2 => (S.cons2 (1, nil2), S.cons2 (true, nil2)));
end;

structure S2 = struct
structure S = struct datatype 'a list2 = nil2 | cons2 of 'a * 'a list2 end;
datatype list2 = datatype S.list2;
val _ = (fn nil2 => (S.cons2 (1, nil2), S.cons2 (true, nil2)));
end;
