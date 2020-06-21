
signature LOW = sig
  (* Abstract Binding Tree of a Type Erased low level language *)
  structure Abt : ABT
  (* Free variables of expressions and values *)
  structure FreeVars : FREEVARS
  (* Representation of contexts (using debruijn indicies at the kind and con binding level) *)
  structure Subst : SUBST
  (* Printing *)
  structure Print : PRINT
end
