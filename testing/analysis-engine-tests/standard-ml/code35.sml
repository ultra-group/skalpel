fun f 0 = 0 | f x = f ((not : bool -> bool) x)
