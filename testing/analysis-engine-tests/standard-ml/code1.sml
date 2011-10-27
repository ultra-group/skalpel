(* Type constructor clash.
 *)

fun average weight list =
    let fun iterator (x,(sum,length)) = (sum + weight x, length + 1)
	val (sum,length) = foldl iterator (0,0) list
    in sum div length
    end

fun find_best weight lists =
    let val average1 = average weight
	fun iterator (list,(best,max)) =
	    let val avg_list = average1 list
	    in if avg_list > max
	       then (list,avg_list)
	       else (best,max)
	    end
	val (best,_) = foldl iterator (nil,0) lists
    in best
    end

val find_best_simple = find_best 1
