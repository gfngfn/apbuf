
type t = {
  fragments : string list;
  original  : string;
}

let is_digit ch =
  let n = Char.code ch in
  Char.code '0' <= n && n <= Char.code '9'


let is_valid_fragment s =
  String.length s > 0 && String.equal s (String.lowercase_ascii s)


let is_valid = List.for_all is_valid_fragment


let make (original : string) : t option =
  let fragments = String.split_on_char '_' original in
  if is_valid fragments then Some({ fragments; original; }) else None


let make_exn (original : string) : t =
  let fragments = String.split_on_char '_' original in
  if is_valid fragments then { fragments; original; } else raise (Invalid_argument("Name.force"))


let original (name : t) : string =
  name.original


let snake_case (name : t) : string =
  name.fragments |> String.concat "_"


let upper_camel_case (name : t) : string =
  let rec aux acc = function
    | [] ->
        List.rev acc

    | x :: [] ->
        List.rev (String.capitalize_ascii x :: acc)

    | x :: ((y :: _) as rest) ->
        let xcap = String.capitalize_ascii x in
        if is_digit (String.get y 0) then
          aux ((xcap ^ "_") :: acc) rest
        else
          aux (xcap :: acc) rest
  in
  aux [] name.fragments |> String.concat ""


let pp ppf name =
  Format.fprintf ppf "<\"%s\">" (snake_case name)


let compare name1 name2 =
  String.compare name1.original name2.original


let int    = make_exn "int"
let bool   = make_exn "bool"
let string = make_exn "string"
let list   = make_exn "list"
let option = make_exn "option"
