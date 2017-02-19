module Ex04h
//nth

val length: list 'a -> Tot nat
let rec length l = match l with
  | [] -> 0
  | _ :: tl -> 1 + length tl
  
val hd : l:list 'a{Cons? l} -> Tot 'a
let hd l =
  match l with
  | h :: t -> h

val tl : l:list 'a{Cons? l} -> Tot (list 'a)
let tl l =
  match l with
  | h :: t -> t

(* Write a function that returns the `n`th element of a list. Give a
   type that ensures it is total. *)
val nth: l: list 'a -> n:nat{n < length l} -> 'a
let rec nth l n =
    if n = 0 then hd l else nth (tl l) (n - 1)