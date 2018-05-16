open Lambda.Fuzzer
open Crowbar

let pp ppf _ = assert false

let () =
  add_test ~name:"idempotent" [ unsafe_expr_gen ] @@ fun unsafe_expr ->
  match Lambda.typ unsafe_expr with
  | Error _ -> bad_test ()
  | Ok (Lambda.Expr.V (expr, ty)) ->
    let unsafe_expr' = Lambda.Expr.untype expr in
    match Lambda.typ unsafe_expr' with
    | Error err -> Crowbar.fail (Fmt.strf "type(untype(type(expr))): %a" (Fmt.hvbox Lambda.pp_error) err)
    | Ok (Lambda.Expr.V (expr', ty')) ->
      check_eq ~pp:Lambda.Parsetree.pp ~eq:Lambda.Parsetree.equal
        (Lambda.Expr.untype expr) (Lambda.Expr.untype expr')
