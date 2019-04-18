structure CH2Tests = 
struct
  exception WRONG
  structure IntSet = BasicUnbalancedSet(IntOrd)

  val empty = IntSet.empty
  val toAdd = [1, 234, ~21, ~324, 342, 35, 765, 23, 456, 876, 4, 1, 1, 1, 234, 4, 0]
  val notIn = [3, 6, 9, 11, 111, ~1]

  fun ensureIn (set, memberFn) = 
    let
      fun check(set, []) = ()
        | check(set, hd::tl) = if memberFn(hd, set) then check(set, tl) else raise WRONG
    in
      check(set, toAdd)
    end

  fun ensureNotIn (set, memberFn) = 
    let
      fun check(set, []) = ()
        | check(set, hd::tl) = if not (memberFn(hd, set)) then check(set, tl) else raise WRONG
    in
      check(set, notIn)
    end

  fun test (insertFn, memberFn) =
    let
      val set = foldl insertFn empty toAdd
      val _ = ensureIn(set, memberFn)
      val _ = ensureNotIn(set, memberFn)
    in
      ()
    end

  fun run () = 
    (
      test(IntSet.insert, IntSet.member);
      test(IntSet.insert2, IntSet.member);
      test(IntSet.insert3, IntSet.member);
      test(IntSet.insert, IntSet.member2)
    )
end