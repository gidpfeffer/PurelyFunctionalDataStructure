
(*use "ex2_1.sml";*)
fun suffixes [] = [[]]
  | suffixes items = items::suffixes(tl items)

val testcase = [1,2,3,4]

(*
Argument:
The way this data structure is represented is a list, with n elements,
and n pointers. One for every index in the list. Making these pointers
is O(1) per pointer, so O(n) total, and space is O(n) for all pointers 
by similar argument.
*)