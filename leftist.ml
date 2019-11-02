(*---------------------TYPY DANYCH---------------------*)
(*  Typ złączalnej kolejki priorytetowej  *)
type 'a queue = 
	Node of 'a queue * 'a queue * 'a * int |
	Null

(* Wyjątek podnoszony przez [delete_min] gdy kolejka jest pusta  *)
exception Empty



(*---------------------FUNKCJE POMOCNICZE---------------------*)
(*  zwraca dlugosc najkrotszej sciezki od danego wierzcholka w drzewie lub 0, gdy wierzcholek to Null  *)
let length node = 
	match node with
		Node(_, _, _, res) -> res |
		Null -> 0

(*  dla dwoch wierzcholkow (a i b) w drzewie zwraca pare (a, b),
		gdzie dlugosc najkrotszej sciezki wychodzacej z a jest mniejsza od -||- b  *)
let get_smaller_length a b = if length a > length b then (b, a) else (a, b)

(*  zwraca wynik polaczenia dwoch drzew a i b  *)
let rec merge a b = 
	match (a, b) with
	(Null, Null) -> Null |
	(Null, Node(_, _, _, _)) -> b |
	(Node(_, _, _, _), Null) -> a |
	(Node(node_left_a, node_right_a, value_a, _), Node(node_left_b, node_right_b, value_b, _)) ->
		if value_a < value_b
			then
				let res = merge (node_right_a) b
				in  
					let (min, max) = get_smaller_length (node_left_a) res
					in Node(max, min, value_a, (length min) + 1)
			else 
				let res = merge a (node_right_b)
				in  
					let (min, max) = get_smaller_length res (node_left_b)
					in Node(max, min, value_b, (length min) + 1)
;;


(*---------------------FUNKCJE Z ZADANIA---------------------*)
(*  Pusta kolejka priorytetowa  *)
let empty = Null

(*  Zwraca [true] jeśli dana kolejka jest pusta. W przeciwnym razie [false]  *)
let is_empty queue = queue = Null

(*  [add e q] zwraca kolejkę powstałą z dołączenia elementu [e] do kolejki [q]  *)
let add value queue = 
	let new_node = Node(Null, Null, value, 1)
	in merge new_node queue

(*  Dla niepustej kolejki [q], [delete_min q] zwraca parę [(e,q')] gdzie [e]
    jest elementem minimalnym kolejki [q] a [q'] to [q] bez elementu [e].
    Jeśli [q] jest puste podnosi wyjątek [Empty]  *)
let delete_min queue = 
	match queue with
		Null -> raise Empty |
		Node(node_left, node_right, value, _) -> (value, merge (node_left) (node_right))

(*  [join q1 q2] zwraca złączenie kolejek [q1] i [q2]  *)
let join a b = merge a b



(*---------------------TESTY---------------------*)
;;