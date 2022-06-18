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
  let keys props = StrMap.fold (fun k v set -> StrSet.add k set) props StrSet.empty in
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
      | OptProp t1, ReqProp t2 -> ExpectedReqButGotOptProp key :: errs
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
      match err with
      | MissingProp key -> Printf.sprintf "key %s is missing" key
      | UnexpectedProp key -> Printf.sprintf "key %s is unexpected" key
      | SubtypeErrorProp (key, err) ->
        Printf.sprintf "error at key %s: %s" key (string_of_ty_error err)
      | ExpectedReqButGotOptProp key ->
        Printf.sprintf "key %s: expected required prop but got optional prop" key
    in
    List.fold_left folder "" errs
;;

(* DSL *)

let record props =
  let props =
    List.map
      (fun prop ->
        let k, v, is_required = prop in
        if not is_required
        then
          failwith
          @@ Printf.sprintf
               "bad syntax in expression at key %s: only required properties are allowed"
               k;
        k, v)
      props
  in
  E.Record (StrMap.of_seq (List.to_seq props))
;;

let record_ty props =
  let props =
    List.map
      (fun prop ->
        let k, v, is_required = prop in
        if is_required then k, Ty.ReqProp v else k, Ty.OptProp v)
      props
  in
  Ty.Record (StrMap.of_seq (List.to_seq props))
;;

let assignability_example e_props ty_props =
  let e = record e_props in
  let ty = record_ty ty_props in
  let result =
    match subtype (elab e) ty with
    | Some ty_err -> string_of_ty_error ty_err
    | None -> "ok"
  in
  Printf.printf "\n%s @:? %s ----  %s\n" (string_of_e e) (string_of_ty ty) result
;;

let subtyping_example ty_props1 ty_props2 =
  let ty1 = record_ty ty_props1 in
  let ty2 = record_ty ty_props2 in
  let result =
    match subtype ty1 ty2 with
    | Some ty_err -> string_of_ty_error ty_err
    | None -> "ok"
  in
  Printf.printf "\n%s <:? %s ----  %s\n" (string_of_ty ty1) (string_of_ty ty2) result
;;

let required_prop k v = k, v, true
let optional_prop k v = k, v, false
let ( <~ ) = required_prop
let ( <~? ) = optional_prop
let ( @:? ) = assignability_example
let ( <:? ) = subtyping_example

let _assignability_examples =
  let open E in
  print_endline "\nassignability examples";
  [ "a" <~ Int 1; "b" <~ True ] @:? [ "a" <~ Ty.Int; "b" <~? Ty.Bool ];
  [] @:? [ "a" <~ Ty.Int; "b" <~? Ty.Bool ];
  [ "a" <~ Int 1; "b" <~ True ] @:? []
;;

let _subtype_examples =
  let open Ty in
  print_endline "\nsubtype examples";
  [ "a" <~ Int; "b" <~ Bool ] <:? [ "a" <~ Int; "b" <~? Bool ];
  [] <:? [ "a" <~ Int; "b" <~? Bool ];
  [ "a" <~ Int; "b" <~ Bool ] <:? [];
  [ "a" <~ Int ] <:? [ "a" <~ Bool ];
  [ "a" <~ Int ] <:? [ "a" <~ Int; "b" <~ Bool ];
  [ "a" <~ Int ] <:? [ "a" <~? Int ];
  [ "a" <~? Int ] <:? [ "a" <~ Int ];
  (* Note the syntax for nested records *)
  [ "a" <~ record_ty [ "b" <~ Int ] ] <:? [ "a" <~ record_ty [ "b" <~ Int; "c" <~ Bool ] ]
;;
