open Record

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