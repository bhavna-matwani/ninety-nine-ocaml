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
let rec findnth list n = 
  match list with 
  | [] -> [] 
  | hd::tl -> 
      if n=1 then [hd]
      else findnth tl (n-1) ;; 
 
(* 7. Remove the duplicates in a list *)

let rec remove_dupli list = 
  match list with 
  | [] -> []
  | hd1::hd2::tl when hd1=hd2 -> remove_dupli (hd1::tl)
  | hd::tl -> hd::remove_dupli tl

 (* 8. Create a list containing all integers within a given range *)
 
 let rec range_rec l a b = 
  if a = b then l @ [b]
  else range_rec (l @ [a]) (a + 1) b;;

let range a b = range_rec [] a b;;

 (* 9. Insert an element at a given position into a list *)
 
 let rec insert ele n list = 
  match list with 
  | [] -> [ele] 
  | h :: t -> if n = 0 then ele :: h :: t else h :: insert ele (n - 1) t;; 
  
 (* 10. Remove the K'th element from a list *)
 
 let rec delete n list = 
  match list with 
  | [] -> []
  | [ele] -> [] 
  | h :: t -> if n = 1 then t else h :: delete (n - 1) t;; 
  
 (* 11. Split a list into two parts; the length of the first part is given *)
 
 let rec split n list = 
  let rec split_helper i acc = function
    | [] -> List.rev acc, []
    | head :: tail as l -> if i = 0
        then List.rev acc, l
        else split_helper (i-1) (head :: acc) tail in
  split_helper n [] list;;
  
 (* 12. Drop every N'th element from a list *)
 
 let drop (n: int) (xs: 'a list) : 'a list = 
  let rec dropInner xs num i = 
    match xs with
    | [] -> []
    | _::_ when n <=0 -> xs
    | h :: t when i < n -> h :: dropInner t n (i + 1)
    | _ :: t when i = n -> dropInner t n 1 in 
  dropInner xs n 1;;

 (* 13. Replicate the elements of a list a given number of times *)
 
 (* 14. Duplicate the elements of a list *)
 
 let rec dupli list = 
  match list with 
  | [] -> []
  | [tl] -> [tl;tl]
  | hd::tl -> hd::hd::dupli tl;; 
