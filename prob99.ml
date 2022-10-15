(* 1. Write a function last : 'a list -> 'a list that returns the last element of a list*)

let rec last list = 
  match list with
  | [] -> []
  | [tl] -> [tl]
  | hd::tl -> last tl;;
  
  
