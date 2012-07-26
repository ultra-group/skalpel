(* Typable
 *
 * This is the reason why we don't want
 *
 *  <..val x = <..>
 *   ..x : bool
 *   ..x + <..>..>
 *
 * as a minimal slice for
 *
 *  val x = true
 *  val y = x : bool
 *  val z = x + 1
 *)

functor F (val y : 'a) =
  struct val x = y
         val _ = x : bool
         val _ = x + 1
  end
