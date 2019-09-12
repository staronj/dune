open Stdune
open Dune_re

type t =
  { re : Re.re
  ; repr : string
  }

let test t = Re.execp t.re

let of_string repr =
  let result =
    Glob_lexer.parse_string repr
    |> Result.map ~f:(fun re -> { re = Re.compile re; repr })
  in
  match result with
  | Error (_, msg) -> invalid_arg (Printf.sprintf "invalid glob: :%s" msg)
  | Ok t -> t

let to_string (t : t) = t.repr
