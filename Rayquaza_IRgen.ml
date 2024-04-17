(* IR generation: translate takes a AST and
   produces LLVM IR*)


module L = Llvm
module A = Ast
(* open Sast once we create *)
(*open Sast*)

module StringMap = Map.Make(String)


open Llvm_bitreader
open Llvm_linker




let translate (globals, functions) =
  let context = L.global_context () in
  let the_module = L.create_module context "Rayquaza"
    and rq_int_t = L.i64_type context
    and rq_float_t = L.double_type context
    and rq_bool_t = L.i1_type context
    and rq_void_t = L.void_type context in
  let rq_str_t = L.pointer_type i8_t in

  let ltype_of_typ = function
      A.Int -> rq_int_t
    | A.Float -> rq_float_t
    | A.Bool -> rq_bool_t
    | A.Str -> rq_str_t
    | A.None -> rq_none_t
  in

  let global_vars : L.llvalue StringMap.t =  (*check if L.llvalue StringMap.t is needed*)
    let global_var m (t, n) =
      let init = L.const_null (ltype_of_typ t) 0 (* Check if 0 is needed *)
      in StringMap.add n (L.define_global n init the_module) m in
    List.fold_left global_var StringMap.empty globals
  in


  (* Define each function (arguments and return type) so we can
     call it even before we've created its body *)

  (* let printf_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let calloc_t = L.function_type str_t [|i32_t;i32_t|] in
  let sleep_t = L.function_type i32_t [| i32_t |] in
  let clock_t = L.function_type i64_t [||] in

  let printf_func = L.declare_function "printf" printf_t the_module in
  let calloc_func = L.declare_function "calloc" calloc_t the_module in
  let sleep_func = L.declare_function "sleep" sleep_t the_module in
  let clock_func = L.declare_function "clock" clock_t the_module in *)
