(* could make this a module and make a .mli file *)
(* list this in module thing? in dune *)

type suit = Spade | Clover | Heart | Diamond

type rank =
  | Ace
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Ten
  | Jack
  | Queen
  | King

type card = { suit : suit; rank : rank }

let all_suits = [ Clover; Spade; Heart; Diamond ]

let all_ranks =
  [
    Ace; Two; Three; Four; Five; Six; Seven; Eight; Nine; Ten; Jack; Queen; King;
  ]

let int_of_rank = function
  | Ace -> 1
  | Two -> 2
  | Three -> 3
  | Four -> 4
  | Five -> 5
  | Six -> 6
  | Seven -> 7
  | Eight -> 8
  | Nine -> 9
  | Ten | Jack | Queen | King -> 10

let string_of_rank = function
  | Jack -> "J"
  | Queen -> "Q"
  | King -> "K"
  | Ace -> "A"
  | r -> r |> int_of_rank |> string_of_int

let string_of_suit = function
  | Spade -> "Spade"
  | Clover -> "Clover"
  | Heart -> "Heart"
  | Diamond -> "Diamond"

let string_of_card card =
  string_of_suit card.suit ^ ":" ^ string_of_rank card.rank
