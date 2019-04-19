signature MAP = 
sig
	type Key
	type 'a Map

	exception NotFound

	val empty : 'a Map
	val bind : Key * 'a * 'a Map -> 'a Map

	val lookup : Key * 'a Map -> 'a
end

(*Ex 2.7*)
functor BasicUnbalancedMap (Element : ORDERED) : MAP = 
struct
  
  exception NotFound
  exception EQSET
  type Key = Element.T
  datatype 'a Tree = EMPTY
    | TREE of 'a Tree * (Key * 'a) * 'a Tree

	type 'a Map = 'a Tree

	val empty = EMPTY

  fun bind (k, v, EMPTY) = TREE(EMPTY, (k,v), EMPTY)
    | bind (k, v, s as TREE(l, (tk, tv), r)) =
      if(Element.lt (k,tk)) then TREE(bind(k, v, l), (tk, tv), r)
      else if (Element.lt (tk,k)) then TREE(l, (tk, tv), bind(k, v, r))
      else TREE(l, (k, v), r)

  fun lookup (k, EMPTY) = raise NotFound
    | lookup (k, TREE(l, (tk, tv), r)) =
      if(Element.lt (k,tk)) then lookup (k, l)
      else if (Element.lt (tk,k)) then lookup (k, r)
      else tv

end