module Protocol = Protocol
module Path = Path

type 'a t

(* TODO jstaron: Add documentation. *)

val return : 'a -> 'a t

val map : 'a t -> f:('a -> 'b) -> 'b t

val both : 'a t -> 'b t -> ('a * 'b) t

val stage : 'a t -> f:('a -> 'b t) -> 'b t

val read_file : path:Path.t -> (string, Unix.error) Stdune.Result.t t

val write_file :
  path:Path.t -> data:string -> (unit, Unix.error) Stdune.Result.t t

val read_directory : path:Path.t -> (string list, Unix.error) Stdune.Result.t t

val run : unit t -> unit
