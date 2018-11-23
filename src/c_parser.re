open Cabs;

let out = ref(stdout);
let width = ref(80);
let tab = ref(8);
let max_indent = ref(60);

let line = ref("");
let line_len = ref(0);
let current = ref("");
let current_len = ref(0);
let spaces = ref(0);
let follow = ref(0);
let roll = ref(0);

let print_tab = size => {
  output_string(out^, String.make(size / 8, '\t'));
  output_string(out^, String.make(size mod 8, ' '));
};

let flush = _ =>
  if (line^ != "") {
    print_tab(spaces^ + follow^);
    output_string(out^, line^);
    line := "";
    line_len := 0;
  };

let commit = _ =>
  if (current^ != "") {
    if (line^ == "") {
      line := current^;
      line_len := current_len^;
    } else {
      line := line^ ++ " " ++ current^;
      line_len := line_len^ + 1 + current_len^;
    };
    current := "";
    current_len := 0;
  };

let new_line = _ => {
  commit();
  if (line^ != "") {
    flush();
    output_char(out^, '\n');
  };
  follow := 0;
};

let force_new_line = _ => {
  commit();
  flush();
  output_char(out^, '\n');
  follow := 0;
};

let indent = _ => {
  new_line();
  spaces := spaces^ + tab^;
  if (spaces^ >= max_indent^) {
    spaces := tab^;
    roll := roll^ + 1;
  };
};

let unindent = _ => {
  new_line();
  spaces := spaces^ - tab^;
  if (spaces^ <= 0 && roll^ > 0) {
    spaces := (max_indent^ - 1) / tab^ * tab^;
    roll := roll^ - 1;
  };
};

let space = _ => commit();

let print = str => {
  current := current^ ++ str;
  current_len := current_len^ + String.length(str);
  if (spaces^ + follow^ + line_len^ + 1 + current_len^ > width^) {
    if (line_len^ == 0) {
      commit();
    };
    flush();
    output_char(out^, '\n');
    if (follow^ == 0) {
      follow := tab^;
    };
  };
};



let get_sign = si =>
  switch (si) {
  | NO_SIGN => ""
  | SIGNED => "signed "
  | UNSIGNED => "unsigned "
  };

let get_size = siz =>
  switch (siz) {
  | NO_SIZE => ""
  | SHORT => "short "
  | LONG => "long "
  | LONG_LONG => "long long "
  };

/*let print = print_endline;
let space = () => print_endline(" ");
let new_line = () => print_endline("");*/

let print_commas = (nl, fct, lst) => {
  let _ =
    List.fold_left(
      (com, elt) => {
        if (com) {
          print(",");
          if (nl) {
            new_line();
          } else {
            space();
          };
        } else {
          ();
        };
        fct(elt);
        true;
      },
      false,
      lst,
    );
  ();
};

let escape_string = str => {
  let lng = String.length(str);
  let conv = value =>
    String.make(
      1,
      Char.chr(
        value
        + (
          if (value < 10) {
            Char.code('0');
          } else {
            Char.code('a') - 10;
          }
        ),
      ),
    );
  let rec build = idx =>
    if (idx >= lng) {
      "";
    } else {
      let sub = String.sub(str, idx, 1);
      let res =
        switch (sub) {
        | "\n" => "\\n"
        | "\"" => "\\\""
        | "'" => "\\'"
        | "\r" => "\\r"
        | "\t" => "\\t"
        | "\b" => "\\b"
        | "\000" => "\\0"
        | _ =>
          if (sub == Char.escaped(sub.[0])) {
            sub;
          } else {
            let code = Char.code(sub.[0]);
            "\\"
            ++ conv(code / 64)
            ++ conv(code mod 64 / 8)
            ++ conv(code mod 8);
          }
        };
      res ++ build(idx + 1);
    };
  build(0);
};



let rec print_type = (fct: unit => unit, typ: base_type) => {
  let base = get_base_type(typ);
  switch (base) {
  | [@implicit_arity] BITFIELD(_, exp) =>
    fct();
    print(" : ");
    print_expression(exp, 1);
  | [@implicit_arity] PROTO(typ', pars, ell) =>
    print_type(
      _ => {
        if (base != typ) {
          print("(");
        };
        print_pointer(typ);
        fct();
        print_array(typ);
        if (base != typ) {
          print(")");
        };
        print("(");
        print_params(pars, ell);
        print(")");
      },
      typ',
    )
  | [@implicit_arity] OLD_PROTO(typ', pars, ell) =>
    ()
  | _ =>
    print_pointer(typ);
    fct();
    print_array(typ);
  };
}
and print_params = (pars: list(single_name), ell: bool) => {
  print_commas(false, print_single_name, pars);
  if (ell) {
    print(
      if (pars == []) {
        "...";
      } else {
        ", ...";
      },
    );
  } else {
    ();
  };
}
and print_pointer = typ =>
  switch (typ) {
  | PTR(typ) =>
    print_pointer(typ);
    print("*");
  | RESTRICT_PTR(typ) =>
    print_pointer(typ);
    print("* __restrict");
    space();
  | CONST(typ) =>
    print_pointer(typ);
    print(" const ");
  | VOLATILE(typ) =>
    print_pointer(typ);
    print(" volatile ");
  | [@implicit_arity] ARRAY(typ, _) => print_pointer(typ)
  | _ => /*print_base_type typ*/ ()
  }

and print_array = typ =>
  switch (typ) {
  | [@implicit_arity] ARRAY(typ, dim) =>
    print_array(typ);
    print("[");
    print_expression(dim, 0);
    print("]");
  | _ => ()
  }


and get_base_type = typ =>
  switch (typ) {
  | PTR(typ) => get_base_type(typ)
  | RESTRICT_PTR(typ) => get_base_type(typ)
  | CONST(typ) => get_base_type(typ)
  | VOLATILE(typ) => get_base_type(typ)
  | [@implicit_arity] ARRAY(typ, _) => get_base_type(typ)
  | _ => typ
 }
 
 and print_onlytype = typ => {
  print_base_type(typ);
  print_type(_ => (), typ);
}



and print_attributes = attrs =>
  switch (attrs) {
  | [] => ()
  | [GNU_EXTENSION] => ()
  | _ =>
    if (attrs != []) {
      print(" __attribute__ ((");
      print_commas(false, print_attribute, attrs);
      print(")) ");
    }
  }
  
  and print_attribute = attr =>
  switch (attr) {
  | GNU_NONE => ()
  | GNU_ID(id) => print(id)
  | [@implicit_arity] GNU_CALL(id, args) =>
    print(id);
    print("(");
    print_commas(false, print_attribute, args);
    print(")");
  | GNU_CST(cst) => print_constant(cst)
  | GNU_EXTENSION => print("__extension__")
  | GNU_INLINE => print("__inline__")
  }




and print_name = ((id, typ, attr, exp): name) => {
  print_type(_ => print(id), typ);
  print_attributes(attr);
  /*if (exp != NOTHING) {
    space();
    print("= ");
    print_expression(exp, 1);
  } else {
    ();
  };*/
}
and get_operator = exp =>
  switch (exp) {
  | NOTHING => ("", 16)
  | [@implicit_arity] UNARY(op, _) =>
    switch (op) {
    | MINUS => ("-", 13)
    | PLUS => ("+", 13)
    | NOT => ("!", 13)
    | BNOT => ("~", 13)
    | MEMOF => ("*", 13)
    | ADDROF => ("&", 13)
    | PREINCR => ("++", 13)
    | PREDECR => ("--", 13)
    | POSINCR => ("++", 14)
    | POSDECR => ("--", 14)
    }
  | [@implicit_arity] BINARY(op, _, _) =>
    switch (op) {
    | MUL => ("*", 12)
    | DIV => ("/", 12)
    | MOD => ("%", 12)
    | ADD => ("+", 11)
    | SUB => ("-", 11)
    | SHL => ("<<", 10)
    | SHR => (">>", 10)
    | LT => ("<", 9)
    | LE => ("<=", 9)
    | GT => (">", 9)
    | GE => (">=", 9)
    | EQ => ("==", 8)
    | NE => ("!=", 8)
    | BAND => ("&", 7)
    | XOR => ("^", 6)
    | BOR => ("|", 5)
    | AND => ("&&", 4)
    | OR => ("||", 3)
    | ASSIGN => ("=", 1)
    | ADD_ASSIGN => ("+=", 1)
    | SUB_ASSIGN => ("-=", 1)
    | MUL_ASSIGN => ("*=", 1)
    | DIV_ASSIGN => ("/=", 1)
    | MOD_ASSIGN => ("%=", 1)
    | BAND_ASSIGN => ("&=", 1)
    | BOR_ASSIGN => ("|=", 1)
    | XOR_ASSIGN => ("^=", 1)
    | SHL_ASSIGN => ("<<=", 1)
    | SHR_ASSIGN => (">>=", 1)
    }
  | QUESTION(_) => ("", 2)
  | CAST(_) => ("", 13)
  | CALL(_) => ("", 15)
  | COMMA(_) => ("", 0)
  | CONSTANT(_) => ("", 16)
  | VARIABLE(_) => ("", 16)
  | EXPR_SIZEOF(_) => ("", 16)
  | TYPE_SIZEOF(_) => ("", 16)
  | INDEX(_) => ("", 15)
  | MEMBEROF(_) => ("", 15)
  | MEMBEROFPTR(_) => ("", 15)
  | GNU_BODY(_) => ("", 17)
  | [@implicit_arity] EXPR_LINE(expr, _, _) => get_operator(expr)
  }
and print_constant = cst =>
  switch (cst) {
  | CONST_INT(i) => print(i)
  | CONST_FLOAT(r) => print(r)
  | CONST_CHAR(c) => print("'" ++ escape_string(c) ++ "'")
  | CONST_STRING(s) => print("\"" ++ escape_string(s) ++ "\"")
  | CONST_COMPOUND(exps) =>
    print("{");
    print_comma_exps(exps);
    print("}");
  }

and print_comma_exps = exps =>
  print_commas(false, exp => print_expression(exp, 1), exps)
and print_expression = (exp: expression, lvl: int) => {
  let (txt, lvl') = get_operator(exp);
  let _ =
    if (lvl > lvl') {
      print("(");
    } else {
      ();
    };
  let _ =
    switch (exp) {
    | NOTHING => ()
    | [@implicit_arity] UNARY(op, exp') =>
      switch (op) {
      | POSINCR
      | POSDECR =>
        print_expression(exp', lvl');
        print(txt);
      | _ =>
        print(txt);
        print_expression(exp', lvl');
      }
    | [@implicit_arity] BINARY(_, exp1, exp2) =>
      /*if (op = SUB) && (lvl <= lvl') then print "(";*/
      print_expression(exp1, lvl');
      space();
      print(txt);
      space();
      /*print_expression exp2 (if op = SUB then (lvl' + 1) else lvl');*/
      print_expression(exp2, lvl' + 1);
    /*if (op = SUB) && (lvl <= lvl') then print ")"*/
    | [@implicit_arity] QUESTION(exp1, exp2, exp3) =>
      print_expression(exp1, 2);
      space();
      print("? ");
      print_expression(exp2, 2);
      space();
      print(": ");
      print_expression(exp3, 2);
    | [@implicit_arity] CAST(typ, exp) =>
      print("(");
      print_onlytype(typ);
      print(")");
      print_expression(exp, 15);
    | [@implicit_arity] CALL(exp, args) =>
      print_expression(exp, 16);
      print("(");
      print_comma_exps(args);
      print(")");
    | COMMA(exps) => print_comma_exps(exps)
    | CONSTANT(cst) => print_constant(cst)
    | VARIABLE(name) => print(name)
    | EXPR_SIZEOF(exp) =>
      print("sizeof(");
      print_expression(exp, 0);
      print(")");
    | TYPE_SIZEOF(typ) =>
      print("sizeof(");
      print_onlytype(typ);
      print(")");
    | [@implicit_arity] INDEX(exp, idx) =>
      print_expression(exp, 16);
      print("[");
      print_expression(idx, 0);
      print("]");
    | [@implicit_arity] MEMBEROF(exp, fld) =>
      print_expression(exp, 16);
      print("." ++ fld);
    | [@implicit_arity] MEMBEROFPTR(exp, fld) =>
      print_expression(exp, 16);
      print("->" ++ fld);
    | [@implicit_arity] GNU_BODY(decs, stat) => ()
      /*print("(");
      print_statement([@implicit_arity] BLOCK(decs, stat));
      print(")");*/
    | [@implicit_arity] EXPR_LINE(expr, _, _) => print_expression(expr, lvl)
    };
  if (lvl > lvl') {
    print(")");
  } else {
    ();
  };
}

and print_base_type = typ =>
  switch (typ) {
  | NO_TYPE => ()
  | VOID => print("void")
  | CHAR(sign) => print(get_sign(sign) ++ "char")
  | [@implicit_arity] INT(size, sign) =>
    print(get_sign(sign) ++ get_size(size) ++ "int")
  | [@implicit_arity] BITFIELD(sign, _) => print(get_sign(sign) ++ "int")
  | FLOAT(size) => print((if (size) {"long "} else {""}) ++ "float")
  | DOUBLE(size) => print((if (size) {"long "} else {""}) ++ "double")
  | NAMED_TYPE(id) => print(id)
  | [@implicit_arity] ENUM(id, items) => ()
  /*print_enum(id, items)*/
  | [@implicit_arity] STRUCT(id, flds) => ()
  /*print_fields("struct " ++ id, flds)*/
  | [@implicit_arity] UNION(id, flds) => () 
  /*print_fields("union " ++ id, flds)*/
  | [@implicit_arity] PROTO(typ, _, _) => print_base_type(typ)
  | [@implicit_arity] OLD_PROTO(typ, _, _) => print_base_type(typ)
  | PTR(typ) => print_base_type(typ)
  | RESTRICT_PTR(typ) => print_base_type(typ)
  | [@implicit_arity] ARRAY(typ, _) => print_base_type(typ)
  | CONST(typ) => print_base_type(typ)
  | VOLATILE(typ) => print_base_type(typ)
  | [@implicit_arity] GNU_TYPE(attrs, typ) =>
    /*print_attributes(attrs);*/
    print_base_type(typ);
  | [@implicit_arity] TYPE_LINE(_, _, _type) => print_base_type(_type)
  }
  and get_storage = sto =>
  switch (sto) {
  | NO_STORAGE => ""
  | AUTO => "auto"
  | STATIC => "static"
  | EXTERN => "extern"
  | REGISTER => "register"
  }


  and print_single_name = ((typ, sto, name)) => {
  if (sto != NO_STORAGE) {
    print(get_storage(sto));
    space();
  };
  print_base_type(typ);
  space();
  print_name(name);
}
and print_defs = defs => {
  let prev = ref(false);
  List.iter(
    def => {
      switch (def) {
      | DECDEF(_) => prev := false
      | _ =>
        if (! prev^) {
          force_new_line();
        };
        prev := true;
      };
      print_def(def);
    },
    defs,
  );
}
and print_def = def =>
  switch (def) {
  | [@implicit_arity] FUNDEF(proto, body) =>
    print_single_name(proto);
    /*let (decs, stat) = body;
    print_statement([@implicit_arity] BLOCK(decs, stat));*/
    force_new_line();

  | [@implicit_arity] OLDFUNDEF(proto, decs, body) => ()
    /*print_single_name(proto);
    force_new_line();
    List.iter(
      dec => {
        print_name_group(dec);
        print(";");
        new_line();
      },
      decs,
    );
    let (decs, stat) = body;
    print_statement([@implicit_arity] BLOCK(decs, stat));
    force_new_line();*/

  | DECDEF(names) => ()
    /*print_name_group(names);
    print(";");
    new_line();*/

  | [@implicit_arity] TYPEDEF(names, attrs) => ()
    /*if (has_extension(attrs)) {
      print("__extension__");
      space();
    };
    print("typedef ");
    print_name_group(names);
    print(";");
    new_line();
    force_new_line();*/

  | ONLYTYPEDEF(names) => ()
    /*print_name_group(names);
    print(";");
    new_line();
    force_new_line();*/
  }

/*let printToplevel = (o, defs) => {
  out := o;
  List.iter(def => switch(def) {
  | FUNDEF(proto, body) =>
    print_single_name(proto);
    /*let (decs, stat) = body;*/
    /*print_statement(BLOCK(decs, stat));*/
  }, defs)
};*/

  switch (Frontc.parse_file("/Users/benjamin/Desktop/quantum/libquantum/classic.c", stderr)) {
  | PARSING_ERROR => print_endline("parse error!")
  | PARSING_OK(defs) =>
    out := stdout;
    /*printToplevel(stdout, file)*/
    print_defs(defs)
  };

/*open Common;

try (
  {
    /*let ic = open_in("../libquantum/classic.c");*/
    let ic = open_in("test.c");
    let lexbuf = Lexing.from_channel(ic);
    let tree = My_parser.main(My_lexer.token, lexbuf);
    print_endline(Printf.sprintf("Found %d functions", List.length(tree)));
    List.iter(
      result =>
        switch (result) {
        | FunctionDecl(result) =>
          switch (result.typedName._type) {
          | Primitive(_type) =>
            let getValueArgs = args =>
              List.fold_left(
                (acc, arg) => acc ++ ", value " ++ arg.name,
                "value " ++ List.hd(args).name,
                List.tl(args),
              );
            let getNameArgs = args =>
              List.fold_left(
                (acc, arg) => acc ++ ", " ++ arg.name,
                List.hd(args).name,
                List.tl(args),
              );
            let getReturnTypeFuncName = typedName =>
              switch (typedName._type) {
              | Primitive(_type) =>
                if (_type == "int") {
                  "Val_int";
                } else {
                  failwith("Return type not support " ++ _type);
                }
              | StructName(_type) =>
                failwith("Return type not support " ++ _type)
              };
            let getNameCArgs = args => {
              let printType = (acc, arg) =>
                switch (arg._type) {
                | Primitive(_type) =>
                  if (_type == "int") {
                    Printf.sprintf("%sInt_val(%s)", acc, arg.name);
                  } else if (_type == "int *") {
                    Printf.sprintf("%sInt_val(Field(%s, 0))", acc, arg.name);
                  } else {
                    failwith("Param type not supported " ++ _type);
                  }
                | StructName(_type) =>
                  failwith("Param type not supported " ++ _type)
                };

              List.fold_left(
                (acc, arg) => printType(acc ++ ", ", arg),
                printType("", List.hd(args)),
                List.tl(args),
              );
            };
            let code =
              if (_type == "void") {
                Printf.sprintf(
                  {|
                void %s(%s) {
                  %s(%s);
                }
              |},
                  "my_" ++ result.typedName.name,
                  getValueArgs(result.args),
                  result.typedName.name,
                  getNameCArgs(result.args),
                );
              } else {
                Printf.sprintf(
                  {|
                CAMLprim value %s(%s) {
                  CAMLparam%d(%s);
                  CAMLreturn(%s(%s(%s)));
                }
              |},
                  "my_" ++ result.typedName.name,
                  getValueArgs(result.args),
                  List.length(result.args),
                  getNameArgs(result.args),
                  getReturnTypeFuncName(result.typedName),
                  result.typedName.name,
                  getNameCArgs(result.args),
                );
              };
            print_endline(code);
          | StructName(_type) =>
            failwith("Function return type not supported " ++ _type)
          }

        /*let getType = x =>
            switch (x._type) {
            | Primitive(_type) => _type
            | StructName(_type) => _type
            };

          let _type = getType(result.typedName);
          print_endline(
            Printf.sprintf(
              ">> %s %s(%s)",
              _type,
              result.typedName.name,
              List.fold_left(
                (acc, arg) => acc ++ getType(arg) ++ " " ++ arg.name ++ ", ",
                "",
                result.args,
              ),
            ),
          );*/

        | Skipped => print_endline("skipped ;(")
        },
      tree,
    );
  }
) {
| My_lexer.Eof => exit(0)
};
*/
