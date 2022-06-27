module StrSet : Set.S with type elt = string

module StrMap : Map.S with type key = string

module E : sig
  type t =
    | True
    | False
    | Int of int
    | Record of t StrMap.t
end

module Ty : sig
  type t =
    | Bool
    | Int
    | Record of prop StrMap.t

  and prop =
    | OptProp of t
    | ReqProp of t
end

type type_error =
  | Mismatch of
      { expected : Ty.t
      ; got : Ty.t
      }
  | RecordError of record_error list

and record_error =
  | MissingProp of string
  | UnexpectedProp of string
  | SubtypeErrorProp of string * type_error
  | ExpectedReqButGotOptProp of string

val subtype : Ty.t -> Ty.t -> type_error option
val elab : E.t -> Ty.t
val string_of_ty : Ty.t -> string
val string_of_e : E.t -> string
val string_of_ty_error : type_error -> string