(* 1. Write a function last : 'a list -> 'a list that returns the last element of a list *)

let rec last list = 
  match list with
  | [] -> []
  | [tl] -> [tl]
  | hd::tl -> last tl;;
  
 (* 2. Write a function last : 'a list -> 'a list that returns the last two elements of a list *) 

let rec last2 list = 
  match list with
  | [] -> []
  | [x;y] -> [x;y]
  | hd::tl -> last2 tl;;
  
(* 3. Write a function last : 'a list -> int that returns the length of a list *) 
 
let rec len list = 
  let rec len_helper list len = 
    match list with
    | [] -> 0
    | [tl] -> 1
    | hd::tl -> len_helper tl len+1 
  in len_helper list 0;;
  
 (* 4. Write a function last : 'a list -> 'a list that returns the reverse of a list *) 
 
  let rec rev list = 
  let rec rev_helper list = 
    match list with
    | [] -> [];
    | [tl] -> [tl]
    | hd::tl -> rev_helper tl @ [hd] 
  in rev_helper list;;
  
(* 5. Find out whether a list is a palindrome *)

let is_pal l =
  l = rev l
  
(* 6. Find the nth element of list *)

(* 7. Remove the duplicates in a list *)

let rec remove_dupli list = 
  match list with 
  | [] -> []
  | hd1::hd2::tl when hd1=hd2 -> remove_dupli (hd1::tl)
  | hd::tl -> hd::remove_dupli tl

  
  
