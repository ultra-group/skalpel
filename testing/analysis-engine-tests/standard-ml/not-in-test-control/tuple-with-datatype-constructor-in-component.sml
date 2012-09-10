datatype 'a mydt = firstCons of 'a | secondCons of int;

val x = firstCons 2.0;

(1,(1,x)) = (3, (4,2.0));
