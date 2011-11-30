datatype t = D of int
type u = t * int * bool
datatype v = C of u * v | T
val x = C ((D 2, 1, true), T)
