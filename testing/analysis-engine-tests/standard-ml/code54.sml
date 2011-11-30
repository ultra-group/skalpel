datatype ('a, 'b) w = W of 'a w * 'b
and v = V of (int, bool) w
val x = V (W (0, 0))
