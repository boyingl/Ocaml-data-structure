
module type Queue = sig
  type 'a queue = 'a list * 'a list
 
  val empty : 'a queue
  val isEmpty : 'a queue -> bool
 
  val snoc : 'a -> 'a queue -> 'a queue
  val first : 'a queue -> 'a option
  val rest : 'a queue -> 'a queue option
end
 

module TLQueue : Queue = struct

type 'a queue = 'a list * 'a list

let empty = ([],[])

let isEmpty = function 
  | ([],[]) -> true
  | _ -> false


let reverse lst = 
  let rec rev accu  = function
    |[] -> accu
    |hd::tl -> rev (hd::accu) tl
  in 
  rev [] lst



let snoc a = function
  | ([], []) -> ([a], [])
  | (deque, inque) -> (deque, a::inque)


let first = function
  | (hd::_, _) -> Some hd
  | ([], _) -> None 


let rec rest = function
  | ([],[]) -> None
  | (_::tl, inque) -> 
    begin match tl with
      | [] -> Some (reverse inque, [])
      | x -> Some (tl, inque)
    end

end
