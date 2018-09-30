(*
  Retourne une liste dont les éléments sont inversés.
  @param lst - La liste dont on veut une copie avec les éléments inversés.
*)

let rec invert lst =
  match lst with
    [] -> [] |
    head :: [] -> [head] |
    _ -> last lst :: invert (removeLast lst)
and last lst =
  match lst with
    [] -> 0 |
    head :: [] -> head |
    head :: tail -> last tail
and removeLast lst =
  match lst with
    [] -> [] |
    head :: [] -> [] |
    head :: tail -> head :: removeLast tail;;

let input = [1;2;3;4;5;6;7;8;9];;
let output = invert input;;


open Printf;;
let () = List.iter (printf "%d ") output;;
printf "\n"
