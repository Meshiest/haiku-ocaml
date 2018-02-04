open Str

let emptyDict = (
  ([]: string list),
  ([]: string list),
  ([]: string list),
  ([]: string list),
  ([]: string list),
  ([]: string list),
  ([]: string list)
);;

(* Reads `file` and stores lines in a string list *)
let readFile file =
  let rec readLine chan = match input_line chan with
      line -> line::readLine chan
    | exception End_of_file -> close_in chan; []
  in open_in file |> readLine;; 

(* Counts the times substring `sub` appears in string `str` *)
let count str sub =
  let search = search_forward (regexp sub) str in
  let rec next i = match search i with
      exception Not_found -> 0
    | n -> (next (n+1)) + 1
  in next 0;;

(* Concatenates a string to a tuple dict index based on number of asterisks *)
let dictAppend dict str =
  let newStr = global_replace (regexp "*") "" str
  in match (count str "*"), dict with
      0, (a, b, c, d, e, f, g) -> (newStr::a, b, c, d, e, f, g)
    | 1, (a, b, c, d, e, f, g) -> (a, newStr::b, c, d, e, f, g)
    | 2, (a, b, c, d, e, f, g) -> (a, b, newStr::c, d, e, f, g)
    | 3, (a, b, c, d, e, f, g) -> (a, b, c, newStr::d, e, f, g)
    | 4, (a, b, c, d, e, f, g) -> (a, b, c, d, newStr::e, f, g)
    | 5, (a, b, c, d, e, f, g) -> (a, b, c, d, e, newStr::f, g)
    | 6, (a, b, c, d, e, f, g) -> (a, b, c, d, e, f, newStr::g)
    | _, (a, b, c, d, e, f, g) -> (a, b, c, d, e, f, g);;

(* Gets a random item from the list in `dict` at tuple index `i` *)
let randomItem dict i =
  let sample l = List.nth l (Random.int (List.length l))
  in match i, dict with
      0, (a, b, c, d, e, f, g) -> sample a
    | 1, (a, b, c, d, e, f, g) -> sample b
    | 2, (a, b, c, d, e, f, g) -> sample c
    | 3, (a, b, c, d, e, f, g) -> sample d
    | 4, (a, b, c, d, e, f, g) -> sample e
    | 5, (a, b, c, d, e, f, g) -> sample f
    | 6, (a, b, c, d, e, f, g) -> sample g
    | _, (a, b, c, d, e, f, g) -> "";;

(* Append every element in a list to dict *)
let rec massAppend dict = function
    [] -> dict
  | x::xs -> massAppend (dictAppend dict x) xs;;

let dict = massAppend emptyDict (readFile "dict.txt")

(* Gets a random word from the dictionary with i syllables *)
let rand i = randomItem dict (i-1);;

(* Generates a list of words given a number of syllables *)
let rec sylLine = function
    0 -> []
  | num ->
    let syl = Random.int (min 7 num) + 1 in
    rand syl::sylLine(num - syl);;

(* Generates a haiku *)
let haiku () = (sylLine 5, sylLine 7, sylLine 5);;

let print_haiku (a, b, c) =
  String.concat " " a ^ "," |> print_endline;
  String.concat " " b ^ "," |> print_endline;
  String.concat " " c |> print_endline

let () = 
  Random.self_init();
  haiku() |> print_haiku
