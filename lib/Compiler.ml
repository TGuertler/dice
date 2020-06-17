open Core
open Cudd
open Wmc
open VarState
open CoreGrammar

let flip_id = ref 1

(** Result of compiling an expression *)
type compiled_expr = {
  state: varstate btree;
  z: Bdd.dt;
  flips: Bdd.dt List.t}

type compiled_func = {
  args: (varstate btree) List.t;
  body: compiled_expr;
}

type compile_context = {
  man: Man.dt;
  name_map: (int, String.t) Hashtbl.Poly.t; (* map from variable identifiers to names, for debugging *)
  weights: weight; (* map from variables to weights *)
  lazy_eval: bool; (* true if lazy let evaluation *)
  funcs: (String.t, compiled_func) Hashtbl.Poly.t;
}

type compiled_program = {
  ctx: compile_context;
  body: compiled_expr;
}

type env = (String.t, varstate btree) Map.Poly.t (* map from variable identifiers to BDDs*)


let new_context ~lazy_eval () =
  let man = Man.make_d () in
  Man.disable_autodyn man;
  let weights = Hashtbl.Poly.create () in
  let names = Hashtbl.Poly.create () in
  {man = man;
   name_map = names;
   weights = weights;
   funcs = Hashtbl.Poly.create ();
   lazy_eval = lazy_eval}


(** generates a symbolic representation for a variable of the given type *)
let rec gen_sym_type ctx (t:typ) : varstate btree =
  match t with
  | TBool ->
    let bdd = Bdd.newvar ctx.man in Leaf(BddLeaf(bdd))
  | TTuple(t1, t2) ->
    let s1 = gen_sym_type ctx t1 and s2 = gen_sym_type ctx t2 in
    Node(s1, s2)

let rec compile_expr (ctx: compile_context) (tenv: tenv) (env: env) e : compiled_expr =
  let r = match e with
  | And(e1, e2) ->
    let c2 = compile_expr ctx tenv env e2 in
    let c1 = compile_expr ctx tenv env e1 in
    let v = Leaf(BddLeaf(Bdd.dand (extract_bdd c1.state) (extract_bdd c2.state))) in
    let z = Bdd.dand c1.z c2.z in
    {state=v; z=z; flips=List.append c1.flips c2.flips}

  | Or(e1, e2) ->
    let c2 = compile_expr ctx tenv env e2 in
    let c1 = compile_expr ctx tenv env e1 in
    let v = Leaf(BddLeaf(Bdd.dor (extract_bdd c1.state) (extract_bdd c2.state))) in
    let z = Bdd.dand c1.z c2.z in
    {state=v; z=z; flips=List.append c1.flips c2.flips}

  | Eq(e1, e2) ->
    let c2 = compile_expr ctx tenv env e2 in
    let c1 = compile_expr ctx tenv env e1 in
    let v = Leaf(BddLeaf(Bdd.eq (extract_bdd c1.state) (extract_bdd c2.state))) in
    let z = Bdd.dand c1.z c2.z in
    {state=v; z=z; flips=List.append c1.flips c2.flips}


  | Not(e) ->
    let c = compile_expr ctx tenv env e in
    let v = Bdd.dnot (extract_bdd c.state) in
    {state=Leaf(BddLeaf(v)); z=c.z; flips=c.flips}

  | True -> {state=Leaf(BddLeaf(Bdd.dtrue ctx.man)); z=Bdd.dtrue ctx.man; flips=[]}

  | False -> {state=Leaf(BddLeaf(Bdd.dfalse ctx.man)); z=Bdd.dtrue ctx.man; flips=[]}

  | Ident(s) ->
    (match Map.Poly.find env s with
     | Some(r) -> {state=r; z=Bdd.dtrue ctx.man; flips=[]}
     | _ -> failwith (sprintf "Could not find variable '%s'" s))

  | Tup(e1, e2) ->
    let c1 = compile_expr ctx tenv env e1 in
    let c2 = compile_expr ctx tenv env e2 in
    {state=Node(c1.state, c2.state); z=Bdd.dand c1.z c2.z; flips=List.append c1.flips c2.flips}

  | Ite(g, thn, els) ->
    let cg = compile_expr ctx tenv env g in
    let cthn = compile_expr ctx tenv env thn in
    let cels = compile_expr ctx tenv env els in
    let gbdd = extract_bdd cg.state in
    let zipped = zip_tree cthn.state cels.state in
    let v' = map_tree zipped (fun (thn_state, els_state) ->
        match (thn_state, els_state) with
        | (BddLeaf(thn_bdd), BddLeaf(els_bdd)) ->
          BddLeaf(Bdd.dor (Bdd.dand gbdd thn_bdd) (Bdd.dand (Bdd.dnot gbdd) els_bdd))
        | (IntLeaf(l1), IntLeaf(l2)) ->
          let zipped_l = try List.zip_exn l1 l2
            with _ -> failwith (Format.sprintf "Type error: length mismatch between %s and %s"
                                  (string_of_expr thn) (string_of_expr els)) in
          let l = List.map zipped_l ~f:(fun (thn_bdd, els_bdd) ->
              Bdd.dor (Bdd.dand gbdd thn_bdd) (Bdd.dand (Bdd.dnot gbdd) els_bdd)
            ) in
          IntLeaf(l)
        | _ -> failwith (sprintf "Type error")
      ) in
    let z' = Bdd.dand cg.z (Bdd.dor (Bdd.dand cthn.z gbdd)
                              (Bdd.dand cels.z (Bdd.dnot gbdd))) in
    {state=v'; z=z'; flips = List.append cg.flips (List.append cthn.flips cels.flips)}

  | Fst(e) ->
    let c = compile_expr ctx tenv env e in
    let v' = (match c.state with
     | Node(l, _) -> l
     | _ -> failwith (Format.sprintf "Internal Failure: calling `fst` on non-tuple at %s" (string_of_expr e))) in
    {state=v'; z=c.z; flips=c.flips}

  | Snd(e) ->
    let c = compile_expr ctx tenv env e in
    let v' = (match c.state with
     | Node(_, r) -> r
     | _ -> failwith (Format.sprintf "Internal Failure: calling `snd` on non-tuple at %s" (string_of_expr e))) in
    {state=v'; z=c.z; flips=c.flips}

  | Flip(f) ->
    let new_f = Bdd.newvar ctx.man in
    let var_lbl = Bdd.topvar new_f in
    let var_name = (Format.sprintf "f%d" !flip_id) in
    flip_id := !flip_id + 1;
    Hashtbl.Poly.add_exn ctx.weights ~key:var_lbl ~data:(1.0-.f, f);
    Hashtbl.add_exn ctx.name_map ~key:var_lbl ~data:var_name;
    {state=Leaf(BddLeaf(new_f)); z=Bdd.dtrue ctx.man; flips=[new_f]}

  | Observe(g) ->
    let c = compile_expr ctx tenv env g in
    {state=Leaf(BddLeaf(Bdd.dtrue ctx.man)); z=Bdd.dand (extract_bdd c.state) c.z; flips=c.flips}

  | Let(x, e1, e2) ->
    (* 2 different evaluation strategies here: lazy and eager  *)
    (match ctx.lazy_eval with
     | true ->
       let c1 = compile_expr ctx tenv env e1 in
       (* create a temp variable *)
       let t = (type_of tenv e1) in
       let tmp = gen_sym_type ctx t in
       let env_tmp = map_tree tmp (function
           | BddLeaf(bdd) -> BddLeaf(bdd)
           | IntLeaf(l) ->
             IntLeaf(List.init (List.length l) ~f:(fun i ->
                 List.foldi l ~init:(Bdd.dtrue ctx.man) ~f:(fun idx acc cur ->
                     let bdd = if idx = i then cur else Bdd.dnot cur in
                     Bdd.dand bdd acc
                   )))) in
       let env' = Map.Poly.set env ~key:x ~data:env_tmp in
       let tenv' = Map.Poly.set tenv ~key:x ~data:t in
       let c2 = compile_expr ctx tenv' env' e2 in
       (* do substitution *)
       let argcube = fold_bddtree tmp (Bdd.dtrue ctx.man) (fun acc i -> Bdd.dand acc i) in
       let iff =
         let tree = map_tree (zip_tree c1.state tmp) (function
             | (BddLeaf(a), BddLeaf(p)) ->
               BddLeaf(Bdd.eq a p)
             | (IntLeaf(al), IntLeaf(pl)) when (List.length al) = (List.length pl) ->
               BddLeaf(List.fold ~init:(Bdd.dtrue ctx.man) (List.zip_exn al pl) ~f:(fun acc (a, p) ->
                   Bdd.dand acc (Bdd.eq a p)))
             | _ -> failwith (Format.sprintf "Type mismatch in arguments for '%s'\n" (string_of_expr e))) in
         fold_bddtree tree (Bdd.dtrue ctx.man) (fun acc i -> Bdd.dand acc i) in
       (* apply the arguments*)
       let final_state = map_bddtree c2.state (fun bdd ->
           Bdd.existand argcube iff bdd) in
       let final_z = Bdd.existand argcube iff c2.z in
       {state=final_state; z=Bdd.dand c1.z final_z; flips=List.append c1.flips c2.flips}

     | false ->
       let c1 = compile_expr ctx tenv env e1 in
       let env' = Map.Poly.set env ~key:x ~data:c1.state in
       let c2 = compile_expr ctx tenv env' e2 in
       {state=c2.state; z=Bdd.dand c1.z c2.z; flips=List.append c1.flips c2.flips})

  | FuncCall(name, args) ->
    let func = try Hashtbl.Poly.find_exn ctx.funcs name
      with _ -> failwith (Format.sprintf "Could not find function '%s'." name) in
    let cargs = List.map args ~f:(compile_expr ctx tenv env) in
    let zippedargs = try List.zip_exn cargs func.args
      with _ -> failwith (Format.sprintf "Incorrect number of arguments for function call '%s'\n" (string_of_expr e)) in
    (* set up the flip refreshing permutation *)
    let new_flips = List.map func.body.flips ~f:(fun f ->
        let newv = Bdd.newvar ctx.man in
        let lvl = Bdd.topvar newv in
        (match Hashtbl.Poly.find ctx.weights (Bdd.topvar f) with
         | Some(v) -> Hashtbl.Poly.add_exn ctx.weights ~key:lvl ~data:v
         | None -> ());
        newv) in
    let swapA = List.to_array (List.map new_flips ~f:(fun cur -> Bdd.topvar cur)) in
    let swapB = List.to_array (List.map func.body.flips ~f:(fun cur -> Bdd.topvar cur)) in
    let refreshed_state = map_bddtree func.body.state (fun bdd -> Bdd.swapvariables bdd swapA swapB) in
    let refreshed_z = Bdd.swapvariables func.body.z swapA swapB in
    let argcube = List.fold func.args ~init:(Bdd.dtrue ctx.man) ~f:(fun acc argstate ->
        fold_bddtree argstate acc (fun acc i -> Bdd.dand acc i)) in
    let argiff = List.fold ~init:(Bdd.dtrue ctx.man) zippedargs ~f:(fun acc (carg, placeholder) ->
        let tree = map_tree (zip_tree carg.state placeholder) (function
            | (BddLeaf(a), BddLeaf(p)) ->
                 BddLeaf(Bdd.dand acc (Bdd.eq a p))
            | (IntLeaf(al), IntLeaf(pl)) when (List.length al) = (List.length pl) ->
              BddLeaf(List.fold ~init:acc (List.zip_exn al pl) ~f:(fun acc (a, p) ->
                 Bdd.dand acc (Bdd.eq a p)))
            | _ -> failwith (Format.sprintf "Type mismatch in arguments for '%s'\n" (string_of_expr e))) in
        fold_bddtree tree acc (fun acc i -> Bdd.dand acc i)) in
    (* apply the arguments*)
    let final_state = map_bddtree refreshed_state (fun bdd ->
        Bdd.existand argcube argiff bdd) in
    let final_z = List.fold ~init:(Bdd.existand argcube argiff refreshed_z) cargs
        ~f:(fun acc arg -> Bdd.dand arg.z acc ) in
    {state=final_state; z=final_z; flips=new_flips} in
  r




let compile_func ctx tenv (f: func) : compiled_func =
  (* set up the context; need both a list and a map, so build both together *)
  let new_tenv = List.fold ~init:tenv f.args ~f:(fun acc (name, typ) ->
      Map.Poly.add_exn acc ~key:name ~data:typ
    ) in
  let (args, env) = List.fold f.args ~init:([], Map.Poly.empty)
      ~f:(fun (lst, map) (name, typ) ->
          let placeholder_arg = gen_sym_type ctx typ in
          (* the environment must know about the mutual exclusivity of integer arguments
             in order to avoid exponential explosions *)
          let env_arg = map_tree placeholder_arg (function
              | BddLeaf(bdd) -> BddLeaf(bdd)
              | IntLeaf(l) ->
                IntLeaf(List.init (List.length l) ~f:(fun i ->
                    List.foldi l ~init:(Bdd.dtrue ctx.man) ~f:(fun idx acc cur ->
                        let bdd = if idx = i then cur else Bdd.dnot cur in
                        Bdd.dand bdd acc
                      )))) in
          let map' = try Map.Poly.add_exn map ~key:name ~data:env_arg
            with _ -> failwith (Format.sprintf "Argument names must be unique: %s found twice" name) in
          (List.append lst [placeholder_arg], map')
        ) in
  (* now compile the function body with these arguments *)
  let body = compile_expr ctx new_tenv env f.body in
  {args = args; body = body}

let compile_program (p:program) : compiled_program =
  (* first compile the functions in topological order *)
  let ctx = new_context ~lazy_eval:true () in
  let tenv = ref Map.Poly.empty in
  List.iter p.functions ~f:(fun func ->
      let c = compile_func ctx !tenv func in
      tenv := Map.Poly.add_exn !tenv ~key:func.name ~data:(type_of_fun !tenv func);
      try Hashtbl.Poly.add_exn ctx.funcs ~key:func.name ~data:c
      with _ -> failwith (Format.sprintf "Function names must be unique: %s found twice" func.name)
    );
  (* now compile the main body, which is the result of the program *)
  let env = Map.Poly.empty in
  {ctx = ctx; body = compile_expr ctx !tenv env p.body}


let get_prob p =
  let c = compile_program p in
  let z = Wmc.wmc c.body.z c.ctx.weights in
  let prob = Wmc.wmc (Bdd.dand (extract_bdd c.body.state) c.body.z) c.ctx.weights in
  prob /. z


module I = Parser.MenhirInterpreter
open Lexing
open Lexer


exception Syntax_error of string


let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  fprintf outx "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)



(** [parse_and_prob] parses and computes the probability for string [txt] *)
let parse_and_prob ?debug txt =
  let buf = Lexing.from_string txt in
  let parsed = try Parser.program Lexer.token buf with
  | SyntaxError msg ->
    fprintf stderr "%a: %s\n" print_position buf msg;
    failwith (Format.sprintf "Error parsing %s" txt)
  | Parser.Error ->
    fprintf stderr "%a: syntax error\n" print_position buf;
    failwith (Format.sprintf "Error parsing %s" txt) in
  let transformed = Passes.from_external_prog parsed in
  (match debug with
   | Some(true)->
     Format.printf "Program: %s\n" (ExternalGrammar.string_of_prog parsed);
     Format.printf "After passes: %s\n" (CoreGrammar.string_of_prog transformed);
   | _ -> ());
  get_prob transformed


let get_lexing_position lexbuf =
  let p = Lexing.lexeme_start_p lexbuf in
  let line_number = p.Lexing.pos_lnum in
  let column = p.Lexing.pos_cnum - p.Lexing.pos_bol + 1 in
  (line_number, column)


let get_parse_error env =
    match I.stack env with
    | lazy Nil -> "Invalid syntax"
    | lazy (Cons (I.Element (state, _, _, _), _)) ->
        try (Parser_messages.message (I.number state)) with
        | _ -> "invalid syntax (no specific message for this eror)"


(** [parse_with_error] parses [lexbuf] as a program or fails with a syntax error *)
let parse_with_error lexbuf =
  let rec helper lexbuf checkpoint =
    match checkpoint with
    | I.InputNeeded _env ->
      (* The parser needs a token. Request one from the lexer,
         and offer it to the parser, which will produce a new
         checkpoint. Then, repeat. *)
      let token = Lexer.token lexbuf in
      let startp = lexbuf.lex_start_p
      and endp = lexbuf.lex_curr_p in
      let checkpoint = I.offer checkpoint (token, startp, endp) in
      helper lexbuf checkpoint
    | I.Shifting _
    | I.AboutToReduce _ ->
      let checkpoint = I.resume checkpoint in
      helper lexbuf checkpoint
    | I.HandlingError _env ->
      (* The parser has suspended itself because of a syntax error. Stop. *)
      let line, pos = get_lexing_position lexbuf in
      let err = get_parse_error _env in
      raise (Syntax_error (Format.sprintf "Error at line %d, position %d: %s\n%!" line pos err))
    | I.Accepted v -> v
    | I.Rejected ->
      (* The parser rejects this input. This cannot happen, here, because
         we stop as soon as the parser reports [HandlingError]. *)
      assert false in
  helper lexbuf (Parser.Incremental.program lexbuf.lex_curr_p)

