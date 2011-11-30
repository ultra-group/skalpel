datatype u = U
datatype 'a u = C of int u | D
val x = (C D, C)
