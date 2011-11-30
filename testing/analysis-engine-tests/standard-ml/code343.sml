(* The three first lines are typable. *)
datatype t = C | D
signature s = sig datatype u = C end;
signature s2 = s where type u = t;
(* Next line does not seem to be typable w.r.t. the SML
 * definition.  It is not typable in HaMLet but it is
 * typable in SML/NJ. *)
structure S = struct datatype u = datatype t end : s2;
