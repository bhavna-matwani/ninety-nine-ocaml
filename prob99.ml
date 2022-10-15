(* 1. Write a function last : 'a list -> 'a list that returns the last element of a list*)

let rec last list = 
  match list with
  | [] -> []
  | [tl] -> [tl]
  | hd::tl -> last tl;;
  
 (* 2. Write a function last : 'a list -> 'a list that returns the last two elements of a list*) 

let rec last2 list = 
  match list with
  | [] -> []
  | [x;y] -> [x;y]
  | hd::tl -> last2 tl;;
