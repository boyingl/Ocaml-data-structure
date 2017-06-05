
module type StreamType = sig
  type 'a stream_cell = Nil | Cons of 'a * 'a stream
  and 'a stream = 'a stream_cell Lazy.t
 
  val (++) : 'a stream -> 'a stream -> 'a stream  (* stream append *)
  val rev : 'a stream -> 'a stream
end
 
module Stream : StreamType = struct

  type 'a stream_cell = Nil | Cons of 'a * 'a stream
  and 'a stream = 'a stream_cell Lazy.t

  (* ??? after force, only do one iteration of more ++ 
     one round will give the real head,  
     one only care about hd & chunck tail anyways???
     but rev, need to force all to get the real head
  *)

  let rec (++) st1 st2 = 
      match Lazy.force st1 with
        | Nil -> st2
        | Cons(hd,tl) -> lazy(Cons(hd, tl ++ st2))


  (* ??? after force, only do all iteration of revh *)
  let rev st = 
    lazy(
    let rec revh accu st =
        match Lazy.force st with
	  | Nil -> accu
	  | Cons(hd,tl) -> revh (Cons(hd,lazy(accu))) tl
     in revh Nil st )

end
 
module type Queue = sig
  type 'a queue = int * 'a Stream.stream * int * 'a Stream.stream
 
  val empty : 'a queue
  val is_empty : 'a queue -> bool
 
  val snoc : 'a -> 'a queue -> 'a queue
  val first : 'a queue -> 'a option
  val rest : 'a queue -> 'a queue option
end
 
module TSQueue : Queue = struct
  open Stream
(* invariant: 
  (f, front, b, back)
  f >= b, if f zero, empty

  rotate on breaking: 
  (f+b, f ++ rev b, 0, lazy(nil))  

  snoc at the back
  first of front, rest front tl + back *)
  
  type 'a queue = int * 'a Stream.stream * int * 'a Stream.stream

  let empty = (0,lazy(Nil),0,lazy(Nil))
  
  let is_empty = function
    | (0,lazy(Nil),0,lazy(Nil)) -> true
    | _ -> false


  let snoc a = function
    | (f, front, b, back) -> 
      if f = b then 
           (f+b+1, front ++ (rev (lazy(Cons(a,back)))), 0, lazy(Nil)) 
      else (f, front, b+1,lazy(Cons(a, back)))


(* (1,2,3,4) does not match (1,_) but match (_) *)
  let first = function
    | (_, front, _, _) -> 
	begin match Lazy.force front with
	  | Nil -> None
	  | Cons(hd,tl) -> Some hd
	end

  let rest = function
    | (f, front, b, back) ->
      if f = 0 then None
      else Some(
        begin match Lazy.force front with
 	  | Cons(hd,tl) -> 
            if f = b then (f-1+b, tl ++ (rev back), 0, lazy(Nil))
            else (f-1, tl, b, back) 
	end
      )  
end
