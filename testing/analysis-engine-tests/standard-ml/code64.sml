datatype 'a t = C
datatype u = D
val ex1 = fn z => (z C, z D)
val ex2 = fn z => (z (x1 : u), z (x2 : 'a t))
