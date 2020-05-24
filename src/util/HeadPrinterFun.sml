
functor HeadPrinterFun(
  structure ExternalPrinter : EXTERNALPRINTER
)= struct

  datatype 'a t =
    Head of string * 'a t list
  | Int of int
  | Raw of string
  | Other of 'a
  | List of 'a t list

  val head = fn s => fn l => Head (s, l)
  val int = fn i => Int i
  val raw = fn s => Raw s
  val list = fn l => List l
  val other = fn m => Other m

  local open PrettyPrint in

  val print =
    fn (outstream : TextIO.outstream) =>
    fn (p : ExternalPrinter.t t) =>
  let
    val stream = makeStream outstream 0

    fun pp p =
      case p of
        Head (s, l) => let
          val () = print stream s
        in
          case l of
            nil => ()
          | _ => pp (List l)
        end
      | Int i => print stream (Int.toString i)
      | Raw s => print stream s
      | Other a => ExternalPrinter.print stream a
      | List l => let
          val () = openBox stream Freestyle 2
          val () = print stream "["
          fun inner l =
            case l of
              nil => ()
            | [one] => pp one
            | h :: t => (pp h; print stream ","; inner t)
          val () = inner l
          val () = print stream "]"
          val () = closeBox stream
        in () end
  in pp p end

  end
end
