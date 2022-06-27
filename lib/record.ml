(* examples are at the bottom *)
module StrSet = Set.Make (String)
module StrMap = Map.Make (String)

module E = struct
  type t =
    | True
    | False
    | Int of int
    | Record of t StrMap.t
end

module Ty = struct
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

let all_keys props1 props2 =
  let keys props = StrMap.fold (fun k _v set -> StrSet.add k set) props StrSet.empty in
  StrSet.union (keys props1) (keys props2)
;;

let rec subtype t1 t2 =
  match t1, t2 with
  | Ty.Bool, Ty.Bool -> None
  | Ty.Bool, _ -> Some (Mismatch { expected = t1; got = t2 })
  | Ty.Int, Ty.Int -> None
  | Ty.Int, _ -> Some (Mismatch { expected = t1; got = t2 })
  | Ty.Record props1, Ty.Record props2 -> record_subtype props1 props2
  | Ty.Record _, _ -> Some (Mismatch { expected = t1; got = t2 })

and record_subtype props1 props2 =
  let find_mismatches key errs =
    let prop1_opt = StrMap.find_opt key props1 in
    let prop2_opt = StrMap.find_opt key props2 in
    let key_subtype t1 t2 =
      match subtype t1 t2 with
      | Some ty_err -> SubtypeErrorProp (key, ty_err) :: errs
      | None -> errs
    in
    match prop1_opt, prop2_opt with
    | Some _, None -> UnexpectedProp key :: errs
    | None, Some _ -> MissingProp key :: errs
    | Some prop1, Some prop2 ->
      (match prop1, prop2 with
      | ReqProp t1, ReqProp t2 -> key_subtype t1 t2
      | ReqProp t1, OptProp t2 -> key_subtype t1 t2
      | OptProp _, ReqProp _ -> ExpectedReqButGotOptProp key :: errs
      | OptProp t1, OptProp t2 -> key_subtype t1 t2)
    | None, None -> assert false
  in
  let mismatches = StrSet.fold find_mismatches (all_keys props1 props2) [] in
  match mismatches with
  | [] -> None
  | _ -> Some (RecordError mismatches)
;;

let rec elab = function
  | E.True -> Ty.Bool
  | E.False -> Ty.Bool
  | E.Int _ -> Ty.Int
  | E.Record e_props ->
    let t_props = StrMap.map (fun e -> Ty.ReqProp (elab e)) e_props in
    Ty.Record t_props
;;

(* display *)

let rec string_of_ty = function
  | Ty.Bool -> "bool"
  | Ty.Int -> "int"
  | Ty.Record props ->
    let folder k v str =
      match v with
      | Ty.ReqProp ty -> Printf.sprintf "%s %s <~ %s; " str k (string_of_ty ty)
      | Ty.OptProp ty -> Printf.sprintf "%s %s <~? %s; " str k (string_of_ty ty)
    in
    StrMap.fold folder props "[" ^ "]"
;;

let rec string_of_e = function
  | E.True -> "true"
  | E.False -> "false"
  | E.Int n -> Printf.sprintf "%d" n
  | E.Record props ->
    let folder k v str = Printf.sprintf "%s %s <~ %s; " str k (string_of_e v) in
    StrMap.fold folder props "[" ^ "]"
;;

let rec string_of_ty_error = function
  | Mismatch m ->
    "expected: " ^ string_of_ty m.expected ^ " but got: " ^ string_of_ty m.got
  | RecordError errs ->
    let folder str err =
      str
      ^
      match err with
      | MissingProp key -> Printf.sprintf "key %s is missing\n" key
      | UnexpectedProp key -> Printf.sprintf "key %s is unexpected\n" key
      | SubtypeErrorProp (key, err) ->
        Printf.sprintf "error at key %s: %s\n" key (string_of_ty_error err)
      | ExpectedReqButGotOptProp key ->
        Printf.sprintf "key %s: expected required prop but got optional prop\n" key
    in
    List.fold_left folder "" errs
;;
