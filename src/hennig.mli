type characters = Range of (int * int) | Single of int | All
type cost_change = bool * int * (string * string) * int
type char_change =
    Additive of characters list
  | NonAdditive of characters list
  | Active of characters list
  | Inactive of characters list
  | Sankoff of characters list
  | Weight of (int * characters list)
type char_name = string
type gappy = [ `Gap | `Nogap ] option
type command =
    Nstates of [ `Dna of gappy | `Number of int | `Protein of gappy ] option
  | Ccode of char_change list
  | Cost of cost_change list
  | Tread of string
  | Xread of string
  | Ignore
  | Charname of char_name list
val is_hennig : [< `Local of string | `Remote of string ] -> bool
