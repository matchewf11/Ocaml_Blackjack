let bet_diff = 10
let init_balence = 500
let init_bet = init_balence / 5

type phase =
  | Betting
  | Dealing
  | PlayerTurn
  | DealerTurn
  | RoundOver
  | GameOver

type betting_actions = Inc | Dec | Confirm | Nothing
type player_turn_actions = Hit | Stand | Nothing
type gameover_actions = Continue | Nothing

type phase_actions = {
  player : player_turn_actions;
  betting : betting_actions;
  game_over : gameover_actions;
}

type game_state = {
  phase : phase;
  deck : Card.card list;
  balance : int;
  bet : int;
  player_hand : Card.card list;
  dealer_hand : Card.card list;
}

let string_of_phase = function
  | Betting -> "Betting"
  | Dealing -> "Dealing"
  | PlayerTurn -> "PlayerTurn"
  | DealerTurn -> "DealerTurn"
  | RoundOver -> "RoundOver"
  | GameOver -> "GameOver"

let shuffled_deck () =
  let rand = Random.State.make_self_init () in
  List.map
    (fun s -> List.map (fun r -> { Card.rank = r; suit = s }) Card.all_ranks)
    Card.all_suits
  |> List.flatten
  |> List.map (fun c -> (Random.State.bits rand, c))
  |> List.sort (fun (k1, _) (k2, _) -> compare k1 k2)
  |> List.map snd

let init_state () =
  {
    phase = Betting;
    deck = shuffled_deck ();
    balance = init_balence;
    bet = init_bet;
    player_hand = [];
    dealer_hand = [];
  }

let int_of_hand hand =
  let open Card in
  let rec aux acc aces = function
    | [] -> if aces > 0 && acc <= 11 then aux (acc + 10) (aces - 1) [] else acc
    | x :: xs ->
        let ace = if x.rank = Ace then aces + 1 else aces in
        aux (acc + int_of_rank x.rank) ace xs
  in
  aux 0 0 hand

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

let round_over_phase game_state =
  let p_val = int_of_hand game_state.player_hand in
  let d_val = int_of_hand game_state.dealer_hand in
  let player_won = p_val <= 21 && (d_val > 21 || p_val > d_val) in
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

let game_over_phase game_state = function
  | Continue -> init_state ()
  | Nothing -> game_state

let betting_phase game_state = function
  | Inc ->
      {
        game_state with
        bet = min game_state.balance (game_state.bet + bet_diff);
      }
  | Dec -> { game_state with bet = max 1 (game_state.bet - bet_diff) }
  | Confirm -> { game_state with phase = Dealing }
  | Nothing -> game_state

let draw_card deck =
  match deck with
  | x :: xs -> (x, xs)
  | _ -> (
      match shuffled_deck () with
      | x :: xs -> (x, xs)
      | _ -> raise (Failure "new deck did not have a card"))

let rec dealer_turn_phase game_state =
  let hand_val = int_of_hand game_state.dealer_hand in
  if hand_val >= 17 then { game_state with phase = RoundOver }
  else
    let x, xs = draw_card game_state.deck in
    dealer_turn_phase
      { game_state with deck = xs; dealer_hand = x :: game_state.dealer_hand }

(* Add double and split later *)
let player_turn_phase game_state = function
  | _ when int_of_hand game_state.player_hand >= 21 ->
      { game_state with phase = DealerTurn }
  | Hit ->
      let x, xs = draw_card game_state.deck in
      { game_state with deck = xs; player_hand = x :: game_state.player_hand }
  | Stand -> { game_state with phase = DealerTurn }
  | Nothing -> game_state

let update_game game_state actions =
  match game_state.phase with
  | Betting -> betting_phase game_state actions.betting
  | Dealing -> dealing_phase game_state
  | PlayerTurn -> player_turn_phase game_state actions.player
  | DealerTurn -> dealer_turn_phase game_state
  | RoundOver -> round_over_phase game_state
  | GameOver -> game_over_phase game_state actions.game_over
