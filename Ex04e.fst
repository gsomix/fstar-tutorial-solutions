module Ex04e
//find

type option 'a =  
   | None : option 'a
   | Some : v:'a -> option 'a

val find: f:('a -> Tot bool) -> l:list 'a 
    -> Tot (option (x:'a {f x}))
let rec find f l = match l with
  | [] -> None
  | hd::tl -> if f hd then Some hd else find f tl

(*
Prove that if `find` returns `Some x` then `f x = true`. 
Is it better to do this intrinsically or extrinsically? Do it both ways.
*)

val find': #t:eqtype -> f:(t -> Tot bool) -> list t -> Tot (option t)
let rec find' #t f l = match l with
  | [] -> None
  | hd::tl -> if f hd then Some hd else find' f tl

val find_some: #t:eqtype 
    -> f:(t -> Tot bool) -> l:list t -> x:t
    -> Lemma (requires (find' f l == Some x))
             (ensures (f x = true))
let rec find_some #t f l x = 
    match l with
    | [y] -> ()
    | y::ys -> if f y then () else find_some f ys x 



