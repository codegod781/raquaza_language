
 
 module L = Llvm
 module A = Rayquaza_ast
(* ADD CALL TO SAST *)
 open Rayquaza_sast

 module StringMap = Map.Make(String)
 (* open Llvm_bitreader
 open Llvm_linker *)
 
 type symbol_table_entry = {
  llvalue: L.llvalue;
  typ: A.typ;
} 

 (* translate : Sast.program -> Llvm.module *)
 let translate (globals, functions) =
    let context    = L.global_context () in  
    let the_module = L.create_module context "Rayquaza" in 
 (*GET RIGHT TYPES  *)
    let i32_t     = L.i32_type context 
    and i8_t      = (L.i8_type context)
    and i1_t      = L.i1_type context
  (* ADDED TYPES: *)
    and float_t    = L.double_type context
    and f32_t     = L.float_type context
    and void_t    = L.void_type context in
  (* let voidptr = L.pointer_type i8_t in*)
    let voidptr = L.pointer_type context in
    let str_t = L.pointer_type context in 
    
  let ltype_of_typ = function
     A.Int    -> i32_t
   | A.Float  -> f32_t
   | A.Bool   -> i1_t
   | A.String -> str_t
   | A.Char   -> i8_t
   | A.Null   -> void_t 
   (*| A.Str_p -> L.pointer_type str_t *)
   (* | A.None  -> void_t *)
   (* | A.Class(class_name)   -> L.pointer_type (StringMap.find class_name class_name_to_named_struct) *)
(* | A.Array(typ)          -> 
       Follow array type with definition
 *)
 in 



   (* Create a map of global variables after creating each *)
   let global_vars : L.llvalue StringMap.t =
     let global_var m (t, n) =
       let init = L.const_int (ltype_of_typ t) 0
       in StringMap.add n (L.define_global n init the_module) m 
      in
      List.fold_left global_var StringMap.empty globals 
    in
 
   let printf_t =  L.var_arg_function_type i32_t [| L.pointer_type context |] in
   let printf_func =  L.declare_function "printf" printf_t the_module in
 
   let function_decls =
    let function_decl m sstmt =
     match sstmt with 
     |(SFunc(name, args, body)) ->
      let formal_types = Array.make (List.length args) voidptr (* Use voidptr for all parameters *)
      in let ftype = L.function_type voidptr formal_types (* Assuming functions return generic pointer *)
      in let llvm_func = L.define_function name ftype the_module
      in StringMap.add name (llvm_func, args, body) m
     | _ -> m
    in
    List.fold_left function_decl StringMap.empty functions
  in

  let build_function_body (name, args, body) =
    let (the_function, _, _) = StringMap.find name function_decls in
    let builder = L.builder_at_end context (L.entry_block the_function) in

    let add_formal n p =
      L.set_value_name n p;
      let local = L.build_alloca voidptr n builder in
      ignore (L.build_store p local builder);
      local
    in
    let params = Array.to_list (L.params the_function) in
    let paramList = List.map2 add_formal args (Array.to_list (L.                                       params the_function)) in
    (*(the_function, builder, paramList)*)

   (* Define each function (arguments and return type) so we can
      call it even before we've created its body *)

 (* let translate (globals, functions) =
  let context = L.global_context () in
  let the_module = L.create_module context "Rayquaza" in
  let void_t = L.void_type context in
  let ptr_t = L.pointer_type (L.i8_type context) in  (* Default type for all parameters *)*)

  

  (* let function_decls : (L.llvalue * sfunc_def) StringMap.t =
     let function_decl m fdecl =
       let name = fdecl.sfname
       and formal_types =
         Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) fdecl.sparams)
       in let ftype = L.function_type (ltype_of_typ fdecl.srtyp) formal_types in
       StringMap.add name (L.define_function name ftype the_module, fdecl) m in
     List.fold_left function_decl StringMap.empty functions in*)




  (* let printf_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let calloc_t = L.function_type str_t [|i32_t;i32_t|] in
  let sleep_t = L.function_type i32_t [| i32_t |] in
  let clock_t = L.function_type i64_t [||] in

  let printf_func = L.declare_function "printf" printf_t the_module in
  let calloc_func = L.declare_function "calloc" calloc_t the_module in
  let sleep_func = L.declare_function "sleep" sleep_t the_module in
  let clock_func = L.declare_function "clock" clock_t the_module in *)



  
 
   (* Fill in the body of the given function *)
   (*let build_function_body fdecl =
     let (the_function, _) = StringMap.find fdecl.sfname function_decls in
     let builder = L.builder_at_end context (L.entry_block the_function) in
 
     let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder in
 
     (* Construct the function's "locals": formal arguments and locally
        declared variables.  Allocate each on the stack, initialize their
        value, if appropriate, and remember their values in the "locals" map *)
     let local_vars =
       let add_formal m (t, n) p =
         L.set_value_name n p;
         let local = L.build_alloca (ltype_of_typ t) n builder in
         ignore (L.build_store p local builder);
         StringMap.add n local m
 
       (* Allocate space for any locally declared variables and add the
        * resulting registers to our map *)
       and add_local m (t, n) =
         let local_var = L.build_alloca (ltype_of_typ t) n builder
         in StringMap.add n local_var m
       in
 
       let formals = List.fold_left2 add_formal StringMap.empty fdecl.sformals
           (Array.to_list (L.params the_function)) in
       List.fold_left add_local formals fdecl.slocals
     in*)
 
     (* Return the value for a variable or formal argument.
        Check local names first, then global names *)
     let lookup n = try StringMap.find n local
       with Not_found -> StringMap.find n global_vars
     in
 
     (* Construct code for an expression; return its value *)
 let rec build_expr builder (e : sexpr) = match e with
         SLiteral i  -> L.const_int i32_t i
        | SBoolLit b  -> L.const_int i1_t (if b then 1 else 0)
(* add FLOAT *)
        | SFloatLit f -> L.const_float float_t f
        | SId s       -> L.build_load (lookup s) s builder
        | SAssign (s, e) -> let e' = build_expr builder e in
         ignore(L.build_store e' (lookup s) builder); e'
        | SBinop (e1, op, e2) ->
         let e1' = build_expr builder e1
         and e2' = build_expr builder e2 in
         (match op with
            A.Add     -> L.build_add
          | A.Sub     -> L.build_sub
(* add MUL and DIV *)
          | A.Mul     -> L.build_fmul
          | A.Div     -> L.build_fdiv
          | A.And     -> L.build_and
          | A.Or      -> L.build_or
          | A.Equal   -> L.build_icmp L.Icmp.Eq
          | A.Neq     -> L.build_icmp L.Icmp.Ne
          | A.Less    -> L.build_icmp L.Icmp.Slt
         ) e1' e2' "tmp" builder
       | SCall ("print", [e]) ->
         L.build_call printf_func [| int_format_str ; (build_expr builder e) |]
           "printf" builder
       | SCall (f, args) ->
         let (fdef, fdecl) = StringMap.find f function_decls in
         let llargs = List.rev (List.map (build_expr builder) (List.rev args)) in
         let result = f ^ "_result" in
         L.build_call fdef (Array.of_list llargs) result builder
     in
 
     (* LLVM insists each basic block end with exactly one "terminator"
        instruction that transfers control.  This function runs "instr builder"
        if the current block does not already have a terminator.  Used,
        e.g., to handle the "fall off the end of the function" case. *)
     let add_terminal builder instr =
       match L.block_terminator (L.insertion_block builder) with
         Some _ -> ()
       | None -> ignore (instr builder) in
 
     (* Build the code for the given statement; return the builder for
        the statement's successor (i.e., the next instruction will be built
        after the one generated by this call) *)
     let rec build_stmt builder = function
         SBlock sl -> List.fold_left build_stmt builder sl
       | SExpr e -> ignore(build_expr builder e); builder
       | SReturn e -> ignore(L.build_ret (build_expr builder e) builder); builder
       | SIf (predicate, then_stmt, else_stmt) ->
         let bool_val = build_expr builder predicate in
 
         let then_bb = L.append_block context "then" the_function in
         ignore (build_stmt (L.builder_at_end context then_bb) then_stmt);
         let else_bb = L.append_block context "else" the_function in
         ignore (build_stmt (L.builder_at_end context else_bb) else_stmt);
 
         let end_bb = L.append_block context "if_end" the_function in
         let build_br_end = L.build_br end_bb in (* partial function *)
         add_terminal (L.builder_at_end context then_bb) build_br_end;
         add_terminal (L.builder_at_end context else_bb) build_br_end;
 
         ignore(L.build_cond_br bool_val then_bb else_bb builder);
         L.builder_at_end context end_bb
 
       | SWhile (predicate, body) ->
         let while_bb = L.append_block context "while" the_function in
         let build_br_while = L.build_br while_bb in (* partial function *)
         ignore (build_br_while builder);
         let while_builder = L.builder_at_end context while_bb in
         let bool_val = build_expr while_builder predicate in
 
         let body_bb = L.append_block context "while_body" the_function in
         add_terminal (build_stmt (L.builder_at_end context body_bb) body) build_br_while;
 
         let end_bb = L.append_block context "while_end" the_function in
 
         ignore(L.build_cond_br bool_val body_bb end_bb while_builder);
         L.builder_at_end context end_bb
 
     in
     (* Build the code for each statement in the function *)
     let func_builder = build_stmt builder (SBlock fdecl.sbody) in
 
     (* Add a return if the last block falls off the end *)
     add_terminal func_builder (L.build_ret (L.const_int i32_t 0))
 
   in
 
   List.iter build_function_body functions;

   the_module 
