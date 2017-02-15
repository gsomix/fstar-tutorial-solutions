module Ex04c
//mem

val append : list 'a -> list 'a -> Tot (list 'a)
let rec append l1 l2 = match l1 with
    | [] -> l2
    | hd :: tl -> hd :: append tl l2

val mem: #t:Type{hasEq t} -> t -> list t -> Tot bool
let rec mem #t x l =
    match l with
    | [] -> false
    | hd::tl ->
        if hd = x then true else mem x tl

val append_mem:  #t:Type{hasEq t} -> l1:list t -> l2:list t -> a:t
        -> Lemma (ensures (mem a (append l1 l2) <==>  mem a l1 || mem a l2))
let rec append_mem #t l1 l2 a = 
    match l1 with
    | [] -> ()
    | x :: xs -> append_mem xs l2 a 