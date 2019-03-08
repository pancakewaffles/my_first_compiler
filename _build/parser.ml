(* binop_precedence - This holds the precedence for each binary operator that is defined *)
let binop_precedence:(char, int) Hashtbl.t = Hashtbl.create 10
(* precedence - a function that gets the precedence of the pending binary operator token *)
let precedence c = try Hashtbl.find binop_precedence c with Not_found -> -1


let rec parse_primary = parser
    | [<'Token.Number n >] -> Ast.Number n
    (* parenexpr ::= '(' expression ')' piggybacks on Ocaml's Stream.Error exception. We use the camlp4 shortcut syntax ?? "parse error" *)
    | [< 'Token.Kwd '('; e=parse_expr; 'Token.Kwd ')' ?? "expected ')'" >] -> e
    | [<'Token.Ident id; stream >] ->
      let rec parse_args accumulator = parser
        | [< e=parse_expr; stream >] ->
          begin parser
            | [< 'Token.Kwd ','; e=parse_args (e :: accumulator) >] -> e
            | [< >] -> e :: accumulator
          end stream
        | [< >] -> accumulator
      in
      let rec parse_ident id = parser
        | [< 'Token.Kwd '('; args = parse_args []; 'Token.Kwd ')' ?? "expected ')'">] ->
          Ast.Call(id, Array.of_list(List.rev args)) (* Uses a lookahead to see if it should initialise Ast.Call or Ast.Variable *)
        | [< >] -> Ast.Variable id
      in
      parse_ident id stream
    | [< >] -> raise (Stream.Error "unknown token when expecting an expression.")

(* expression = primary binoprhs *)
and parse_expr = parser
    | [< lhs = parse_primary ; stream >] -> parse_bin_rhs 0 lhs stream

(* binoprhs = ('+' primary)* *)
and parse_bin_rhs expr_prec lhs stream =
    match Stream.peek stream with
    (* If this is a binop, find its precedence. *)
    | Some (Token.Kwd c) when Hashtbl.mem binop_precedence c -> let token_prec = precedence c in
    if token_prec < expr_prec then lhs else begin
      (* Eat the binop *)
      Stream.junk stream;
      (* Parse the primary expression after the binary operator *)
      let rhs = parse_primary stream in
      (* Okay, we know this is a binop *)
      let rhs =
        match Stream.peek stream with
          | Some (Token.Kwd c2) ->
          (* If BinOp binds less tightly with rhs than the operator after rhs, let the pending operator take rhs as its lhs. So, we could have (a+b) binop unparsed or a+(b binop unparsed). If we look ahead at binop and determine its precedence and compare it to BinOp's precedence (which is '+' in this example).
      Hey, it's of lower or equal precedence, so (a+b) binop ... *)
            let next_prec = precedence c2 in
            if token_prec < next_prec then parse_bin_rhs (token_prec + 1) rhs stream
            else rhs
          | _ -> rhs
      in
      (* Merge lhs/rhs *)
      let lhs = Ast.Binary (c,lhs, rhs) in
      parse_bin_rhs expr_prec lhs stream
    end
   | _ -> lhs


(* Have you survived expressions? Good. Now we dive into function prototypes. *)
(* prototype = id '(' id* ')' *)
let parse_prototype =
        let rec parse_args accumulator = parser
                | [< 'Token.Ident id; e = parse_args (id::accumulator) >] -> e
                | [< >] -> accumulator
        in
        parser
        | [< 'Token.Ident id; 'Token.Kwd '(' ?? "expected '(' in prototype"; args = parse_args []; 'Token.Kwd ')' ?? "expected ')' in prototype" >] -> Ast.Prototype(id, Array.of_list(List.rev args))
        | [< >] -> raise (Stream.Error "expected function name in prototype")

(* definition = 'def' prototype expression *)
let parse_definition = parser
        | [< 'Token.Def; p=parse_prototype; e=parse_expr >] -> Ast.Function(p,e)

(* external = 'extern' prototype *)
let parse_extern = parser
        | [< 'Token.Extern; e = parse_prototype >] -> e

(* toplevelexpr = expression i.e. anonymous functions *)
let parse_toplevel = parser
        | [< e=parse_expr >] -> Ast.Function (Ast.Prototype ("",[||]) , e)
