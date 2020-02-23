
open Types
open ResultMonad

module I = Parser.MenhirInterpreter


let k_success (fin : in_channel) (x : top_level) : (top_level, error) result =
  close_in fin;
  return x


let k_fail (checkpoint : top_level I.checkpoint) : (top_level, error) result =
  match checkpoint with
  | I.HandlingError(parser_env) ->
      let rng = Range.from_positions (I.positions parser_env) in
      error (ParseErrorDetected{ range = rng; })

  | _ ->
      assert false


(** The argument [path_in] must be an absolute path of an existent input file. *)
let process (path_in : string) : (top_level, error) result =
  let fin = open_in path_in in
  let lexbuf =
    let lexbuf = Lexing.from_channel fin in
    let open Lexing in
    lexbuf.lex_start_p <- { lexbuf.lex_start_p with pos_fname = path_in; };
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = path_in; };
    lexbuf
  in
  let supplier = I.lexer_lexbuf_to_supplier Lexer.token lexbuf in
  try
    I.loop_handle (k_success fin) k_fail supplier (Parser.Incremental.toplevel lexbuf.Lexing.lex_curr_p)
  with
  | Lexer.LexingError(e) ->
      error e
