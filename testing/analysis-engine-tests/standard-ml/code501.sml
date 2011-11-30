(* Typable - to use with the basis. *)
if Posix.ProcEnv.sysconf "OPEN_MAX" > 0w10
then print "yay!\n"
else print "oh no!\n";
