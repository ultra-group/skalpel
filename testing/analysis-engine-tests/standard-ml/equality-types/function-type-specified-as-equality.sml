let
    val id : (''a * ''a) -> ''b -> ''a * ''a =
     fn x => fn y => x
in
    id (5.0, 6.0) 7.0
end;
