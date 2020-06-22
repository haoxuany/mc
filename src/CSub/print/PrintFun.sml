
functor PrintFun(
  structure Abt : ABT
) : PRINT = struct
  open Abt

  type stream = TextIO.outstream

  fun printIndent stream i =
    case i of
      0 => ()
    | _ => TextIO.output (stream,
      String.implode (List.tabulate (i, (fn _ => #" "))))

  fun printMacro stream macro = let
    fun print s = TextIO.output (stream, s)
  in case macro of
      Macro_include s => (
        print "#include \"";
        print s;
        print "\"";
        ()
      )
  end

  fun printCType stream ctype = let
    fun print s = TextIO.output (stream, s)
  in case ctype of
       CType_sym sym => print (Symbols.name sym)
     | CType_void => print "void"
     | CType_ptr ty => (
         printCType stream ty;
         print "*";
         ()
       )
     | CType_fn (ret, args) => (
         printCType stream ret;
         print " (*)(";
         let
           fun pargs args =
             case args of
               nil => ()
             | [one] => printCType stream one
             | h :: rest => (
                 printCType stream h;
                 print ", ";
                 pargs rest
               )
         in pargs args end;
         print ")"
       )
  end

  fun printState stream indent state = let
    fun pIndent () = printIndent stream indent
    fun print s = TextIO.output (stream, s)
  in case state of
       State_exp e => (
         printExp stream indent e;
         print ";"
       )
     | State_decl (ty, s) => (
         pIndent ();
         printCType stream ty;
         print " ";
         print (Symbols.name s);
         print ";"
       )
     | State_return e => (
         pIndent ();
         (case e of
            NONE => print "return;"
          | SOME e => (
              print "return ";
              printExp stream 0 e;
              print ";"
            ))
        )
     | State_switch (e, cases, default) => let
         fun pstates i s =
           case s of
             nil => ()
           | [one] => printState stream i one
           | h :: rest => (
               printState stream i h;
               print "\n";
               pstates i rest
             )

       in pIndent ();
         print "switch (";
         printExp stream 0 e;
         print ") {\n";
         let
           fun psingle (e, s) = (
             pIndent ();
             print "case ";
             printExp stream 0 e;
             print ":\n";
             pstates (indent + 2) s
           )

           fun pcase cases =
             case cases of
               nil => ()
             | [one] => psingle one
             | h :: rest => (
                 psingle h;
                 print "\n";
                 pcase rest
               )
         in pcase cases end;
         (case default of
            NONE => ()
          | SOME s => (
              pIndent ();
              print "default:\n";
              pstates (indent + 2) s
            )
         )
       end
  end

  and printExp stream indent exp = let
    fun pIndent () = printIndent stream indent
    fun print s = TextIO.output (stream, s)
  in case exp of
       Exp_sym sym => (
         pIndent ();
         print (Symbols.name sym)
       )
     | Exp_int i => (
         pIndent ();
         print (Int.toString i)
       )
     | Exp_string s => (
         pIndent ();
         print "\"";
         print s;
         print "\""
       )
     | Exp_assign (e, e') => (
        pIndent ();
        printExp stream 0 e;
        print " = ";
        printExp stream 0 e'
      )
    | Exp_cast (e, ty) => (
        pIndent ();
        print "((";
        printCType stream ty;
        print ")";
        printExp stream 0 e;
        print ")"
      )
    | Exp_deref e => (
        pIndent ();
        print "(*";
        printExp stream 0 e;
        print ")"
      )
    | Exp_addr e => (
        pIndent ();
        print "(&";
        printExp stream 0 e;
        print ")"
      )
    | Exp_index (e, e') => (
        pIndent ();
        printExp stream 0 e;
        print "[";
        printExp stream 0 e';
        print "]"
      )
    | Exp_call (e, es) => (
        pIndent ();
        printExp stream 0 e;
        print "(";
        let
          fun pargs args =
            case args of
              nil => ()
            | [one] => printExp stream 0 one
            | h :: rest => (
                printExp stream 0 h;
                print ", ";
                pargs rest
              )
        in pargs es end;
        print ")"
      )
  end

  fun printDecl stream decl = let
    fun print s = TextIO.output (stream, s)
  in case decl of
      Decl_fn (retty, name, args, states) => (
        printCType stream retty;
        print " ";
        print (Symbols.name name);
        print "(";
        let
          fun psingle (ty, sym) = (
            printCType stream ty;
            print " ";
            print (Symbols.name sym)
          )
          fun pargs args =
            case args of
              nil => ()
            | [one] => psingle one
            | h :: rest => (
                psingle h;
                print ", ";
                pargs rest
              )
        in pargs args end;
        print ") {\n";
        List.map
          (fn s => (printState stream 2 s; print "\n"))
          states;
        print "}"
      )
    | Decl_fnty (retty, name, args) => (
        printCType stream retty;
        print " ";
        print (Symbols.name name);
        print "(";
        let
          fun pargs args =
            case args of
              nil => ()
            | [one] => printCType stream one
            | h :: rest => (
                printCType stream h;
                print ", ";
                pargs rest
              )
        in pargs args end;
        print ");"
      )
  end

  fun printCFile stream cfile = let
    fun print s = TextIO.output (stream, s)
  in case cfile of
      CFile {macros, decls} => (
        let
          fun pmacro macros = case macros of
            nil => ()
          | [one] => printMacro stream one
          | h :: rest => (
              printMacro stream h;
              print "\n";
              pmacro rest
            )
        in pmacro macros end;
        print "\n\n";
        let
          fun pdecls decls = case decls of
            nil => ()
          | [one] => printDecl stream one
          | h :: rest => (
              printDecl stream h;
              print "\n";
              pdecls rest
            )
        in pdecls decls end;
        print "\n"
      )
  end


end
