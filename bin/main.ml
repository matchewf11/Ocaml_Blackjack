let bet_diff = 10
let init_balence = 500
let init_bet = init_balence / 5
let window_width = 800
let window_height = 600
let font_size = (window_width + window_height) / 70
let header_height = window_height / 20
let cushion = (window_width + window_height) / 700
let balance_x = window_width / 4
let bet_x = window_width / 2
let fps_x = window_width / 4 * 3
let deck_text_x = (window_width / 2) - (font_size * 2)
let deck_text_y = window_height / 8
let card_width = window_width / 8
let card_height = window_height / 4

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

type phase =
  | Betting
  | Dealing
  | PlayerTurn
  | DealerTurn
  | RoundOver
  | GameOver

type card = { suit : suit; rank : rank }

type game_state = {
  phase : phase;
  deck : card list;
  balance : int;
  bet : int;
  player_hand : card list;
  dealer_hand : card list;
}

let all_suits = [ Clover; Spade; Heart; Diamond ]

let all_ranks =
  [
    Ace; Two; Three; Four; Five; Six; Seven; Eight; Nine; Ten; Jack; Queen; King;
  ]

let shuffled_deck () =
  let unshuffled_deck =
    List.map
      (fun s -> List.map (fun r -> { rank = r; suit = s }) all_ranks)
      all_suits
    |> List.flatten
  in
  let rand = Random.State.make_self_init () in
  unshuffled_deck
  |> List.map (fun c -> (Random.State.bits rand, c))
  |> List.sort (fun (k1, _) (k2, _) -> compare k1 k2)
  |> List.map snd

let string_of_phase = function
  | Betting -> "Betting"
  | Dealing -> "Dealing"
  | PlayerTurn -> "PlayerTurn"
  | DealerTurn -> "DealerTurn"
  | RoundOver -> "RoundOver"
  | GameOver -> "GameOver"

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

let int_of_hand hand =
  let rec aux acc aces = function
    | [] -> if aces > 0 && acc <= 11 then aux (acc + 10) (aces - 1) [] else acc
    | x :: xs ->
        let ace = if x.rank = Ace then aces + 1 else aces in
        aux (acc + int_of_rank x.rank) ace xs
  in
  aux 0 0 hand

let draw_card x y color =
  Raylib.draw_rectangle x y card_width card_height color;
  Raylib.draw_rectangle_lines x y card_width card_height Raylib.Color.black

let draw_game game_state =
  let open Raylib in
  begin_drawing ();
  draw_rectangle 0 0 window_width header_height Color.brown;
  draw_text
    ("Phase: " ^ string_of_phase game_state.phase)
    cushion cushion font_size Color.black;
  draw_text
    ("Balance: " ^ string_of_int game_state.balance)
    balance_x cushion font_size Color.black;
  draw_text
    ("Bet: " ^ string_of_int game_state.bet)
    bet_x cushion font_size Color.black;
  draw_fps fps_x cushion;
  draw_rectangle 0 header_height window_width
    (window_height - header_height)
    Color.green;
  draw_card deck_text_x deck_text_y Color.red;
  draw_text
    ("Deck: " ^ string_of_int (List.length game_state.deck))
    deck_text_x deck_text_y font_size Color.black;

  let player_output =
    "PlayerHand: "
    ^ string_of_int (int_of_hand game_state.player_hand)
    ^ " : Revealed"
    ^ List.fold_left
        (fun acc c -> acc ^ " " ^ string_of_card c)
        " " game_state.player_hand
  in
  print_endline player_output;

  let dealer_output =
    "DealerHand: "
    ^ string_of_int (int_of_hand game_state.dealer_hand)
    ^ " : Revealed"
    ^ List.fold_left
        (fun acc c -> acc ^ " " ^ string_of_card c)
        " " game_state.dealer_hand
  in
  print_endline dealer_output;
  end_drawing ()

let init_state () =
  {
    phase = Betting;
    deck = shuffled_deck ();
    balance = init_balence;
    bet = init_bet;
    player_hand = [];
    dealer_hand = [];
  }

let betting_phase game_state =
  let open Raylib in
  if is_key_pressed Key.I then
    { game_state with bet = min game_state.balance (game_state.bet + bet_diff) }
  else if is_key_pressed Key.D then
    { game_state with bet = max 1 (game_state.bet - bet_diff) }
  else if is_key_pressed Key.S then { game_state with phase = Dealing }
  else game_state

let game_over_phase game_state =
  if Raylib.is_key_pressed Raylib.Key.C then init_state () else game_state

let dealing_phase game_state =
  let a, b, c, d, rest =
    match game_state.deck with
    | a :: b :: c :: d :: rest -> (a, b, c, d, rest)
    | _ -> (
        match shuffled_deck () with
        | a :: b :: c :: d :: rest -> (a, b, c, d, rest)
        | _ -> raise (Failure "new deck did not have 4 cards"))
  in
  {
    game_state with
    phase = PlayerTurn;
    deck = rest;
    player_hand = [ a; c ];
    dealer_hand = [ b; d ];
  }

let rec dealer_turn_phase game_state =
  let hand_val = int_of_hand game_state.dealer_hand in
  if hand_val >= 17 then { game_state with phase = RoundOver }
  else
    let x, xs =
      match game_state.deck with
      | x :: xs -> (x, xs)
      | _ -> (
          match shuffled_deck () with
          | x :: xs -> (x, xs)
          | _ -> raise (Failure "new deck did not have a card"))
    in
    dealer_turn_phase
      { game_state with deck = xs; dealer_hand = x :: game_state.dealer_hand }

let round_over_phase game_state =
  let player_val = int_of_hand game_state.player_hand in
  let dealer_val = int_of_hand game_state.dealer_hand in
  let player_won =
    if player_val > 21 then false
    else if dealer_val > 21 then true
    else if player_val = dealer_val then false
    else player_val > dealer_val
  in
  let new_balance =
    if player_won then game_state.balance + game_state.bet
    else game_state.balance - game_state.bet
  in
  {
    game_state with
    phase = (if new_balance <= 0 then GameOver else Betting);
    balance = new_balance;
    bet = min game_state.bet new_balance;
    player_hand = [];
    dealer_hand = [];
  }

(* Add double and split later *)
let player_turn_phase game_state =
  let open Raylib in
  if int_of_hand game_state.player_hand >= 21 then
    { game_state with phase = DealerTurn }
  else if is_key_pressed Key.H then
    let x, xs =
      match game_state.deck with
      | x :: xs -> (x, xs)
      | _ -> (
          match shuffled_deck () with
          | x :: xs -> (x, xs)
          | _ -> raise (Failure "shuffled_deck did not make a card"))
    in
    { game_state with deck = xs; player_hand = x :: game_state.player_hand }
  else if is_key_pressed Key.S then { game_state with phase = DealerTurn }
  else game_state

let update_game game_state =
  match game_state.phase with
  | Betting -> betting_phase game_state
  | Dealing -> dealing_phase game_state
  | PlayerTurn -> player_turn_phase game_state
  | DealerTurn -> dealer_turn_phase game_state
  | RoundOver -> round_over_phase game_state
  | GameOver -> game_over_phase game_state

let target_fps = 10

let rec game_loop game_state =
  let open Raylib in
  if window_should_close () then close_window ()
  else
    let updated_game = update_game game_state in
    draw_game updated_game;
    game_loop updated_game

let setup_game () =
  let open Raylib in
  init_window window_width window_height "BlackJack";
  set_target_fps target_fps

let () = setup_game () |> init_state |> game_loop
