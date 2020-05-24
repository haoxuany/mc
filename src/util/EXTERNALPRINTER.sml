
signature EXTERNALPRINTER = sig
  type t
  val print : PrettyPrint.ppstream -> t -> unit
end
