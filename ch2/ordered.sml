signature ORDERED =
sig
	type T

	val eq : T * T -> bool
	val lt : T * T -> bool
	val leq : T * T -> bool
end

structure IntOrd : ORDERED = 
struct
	type T = int

	fun eq (x,y) = x = y
	fun lt (x,y) = x < y
	fun leq (x,y) = x <= y
end