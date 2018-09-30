(*
  Retourne une liste dont les éléments sont inversés.
  @param lst - La liste dont on veut une copie avec les éléments inversés.
*)

let rec invert arg = invert2 ([], arg)
and invert2 arg =
  match arg with
  (invertedPart, []) -> invertedPart |
  (invertedPart, head::tail) -> invert2 (head::invertedPart, tail);;


let input = [1;2;3;4;5;6;7;8;9];;
let output = invert input;;


open Printf;;
let () = List.iter (printf "%d ") output;;
printf "\n"
