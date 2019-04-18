signature SET = 
sig
  type Elem
  type Set

  val empty : Set
  val insert : Elem * Set -> Set
  val insert2 : Elem * Set -> Set
  val insert3 : Elem * Set -> Set
  val member : Elem * Set -> bool
  val member2 : Elem * Set -> bool
  val complete : Elem * int -> Set
  val balanced : Elem * int -> Set
end

functor BasicUnbalancedSet (Element : ORDERED) : SET = 
struct
  
  exception EQSET
  type Elem = Element.T
  datatype Tree = EMPTY
    | TREE of Tree * Elem * Tree
  type Set = Tree

  val empty = EMPTY

  fun insert (elem, EMPTY) = TREE(EMPTY, elem, EMPTY)
    | insert (elem, s as TREE(l, v, r)) =
      if(Element.lt (elem,v)) then TREE(insert(elem, l), v, r)
      else if (Element.lt (v,elem)) then TREE(l, v, insert(elem, r))
      else s

  fun member (elem, EMPTY) = false
    | member (elem, TREE(l, v, r)) =
      if(Element.lt (elem,v)) then member (elem, l)
      else if (Element.lt (v,elem)) then member (elem, r)
      else true

  (*Ex 2.1*)
  fun suffixes [] = [[]]
    | suffixes items = items::suffixes(tl items)

  val testcase = [1,2,3,4]

  (*Ex 2.2*)
  fun member2 (elem, tree) =
    let
      fun memberTail (elem, EMPTY, tailVal) = 
          if isSome tailVal 
          then Element.eq (elem, (valOf tailVal)) 
          else false
        | memberTail (elem, s as TREE(l, v, r), tailVal) = 
          if Element.leq(elem, v) then memberTail (elem, l, SOME(v))
          else memberTail (elem, r, tailVal)
    in
      memberTail (elem, tree, NONE)
    end

  (*Ex 2.3*)
  fun insert2 (elem, EMPTY) = TREE(EMPTY, elem, EMPTY)
    | insert2 (elem, s) =
      let
        fun throwWhenEqual (elem, EMPTY) = TREE(EMPTY, elem, EMPTY)
          | throwWhenEqual (elem, s as TREE(l, v, r)) =
              if(Element.lt (elem,v)) then TREE(throwWhenEqual(elem, l), v, r)
              else if (Element.lt (v,elem)) then TREE(l, v, throwWhenEqual(elem, r))
              else raise EQSET
      in
        throwWhenEqual (elem, s) handle EQSET => s
      end

  (*Ex 2.4*)
  fun insert3 (elem, EMPTY) = TREE(EMPTY, elem, EMPTY)
    | insert3 (elem, s) =
      let
        fun throwWhenEqual (elem, EMPTY, tailVal) =
            if isSome tailVal 
            then 
              (if Element.eq (elem, (valOf tailVal)) then raise EQSET else TREE(EMPTY, elem, EMPTY))
            else TREE(EMPTY, elem, EMPTY)
          | throwWhenEqual (elem, s as TREE(l, v, r), tailVal) =
              if Element.leq(elem, v) then TREE(throwWhenEqual(elem, l, SOME(v)), v, r)
              else TREE(l, v, throwWhenEqual(elem, r, tailVal))
      in
        throwWhenEqual (elem, s, NONE) handle EQSET => s
      end

  (*Ex 2.5*)
  fun complete (x, 0) = EMPTY
    | complete (x, d) = 
      let
        val child = complete(x, d - 1)
      in
        TREE(child, x, child)
      end

  (*Ex 2.6*)
  (*
    Recurrence is: 
    T(n) = T(n/2) + 1
    which solves to log_2(n)

    O(log(n))
  *)
  fun balanced (x, n) = 
      let
        fun create2(0) = (EMPTY, TREE(EMPTY, x, EMPTY))
          | create2(m) = 
            let
              val (a, b) = create2((m - 1) div 2)
            in
              (case (m mod 2) of
                  0 => (TREE(b, x, a), TREE(b, x, b))
                | _ => (TREE(a, x, a), TREE(b, x, a)))
            end
        val (res, _) = create2(n)
      in
        res
      end
end