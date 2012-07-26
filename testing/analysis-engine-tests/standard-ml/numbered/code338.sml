(* untypable *)
fn x => fn c => fn u => fn v =>
let
    val _ = c u
    val _ = u + v
in c true
end
