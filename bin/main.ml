let window_width = 800
let window_height = 600
let target_fps = 10
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
let hand_spacing = card_height / 4
let player_hand_x = window_width / 4
let hands_y = window_width / 2
let dealer_hand_x = window_width / 4 * 3

let draw_card_rect x y color =
  Raylib.draw_rectangle x y card_width card_height color;
  Raylib.draw_rectangle_lines x y card_width card_height Raylib.Color.black

let draw_hand hand start_x start_y =
  let open Raylib in
  List.iteri
    (fun i card ->
      let y = start_y + (i * hand_spacing) in
      draw_card_rect start_x y Color.white;
      draw_text
        (Blackjack.Card.string_of_card card)
        start_x y font_size Color.black)
    hand

let draw_game game_state =
  let open Raylib in
  let open Blackjack.State in
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
  draw_card_rect deck_text_x deck_text_y Color.red;
  draw_text
    ("Deck: " ^ string_of_int (List.length game_state.deck))
    deck_text_x deck_text_y font_size Color.black;
  draw_hand game_state.player_hand player_hand_x hands_y;
  draw_hand game_state.dealer_hand dealer_hand_x hands_y;
  end_drawing ()

let get_actions () =
  let open Raylib in
  let open Blackjack.State in
  {
    betting =
      (if is_key_pressed Key.W then Inc
       else if is_key_pressed Key.S then Dec
       else if is_key_pressed Key.Enter then Confirm
       else Nothing);
    player =
      (if is_key_pressed Key.W then Hit
       else if is_key_pressed Key.S then Stand
       else Nothing);
    game_over = (if is_key_pressed Key.Enter then Continue else Nothing);
  }

let rec game_loop game_state =
  let open Raylib in
  let open Blackjack.State in
  if window_should_close () then close_window ()
  else
    let updated_game = update_game game_state (get_actions ()) in
    draw_game updated_game;
    game_loop updated_game

let setup_game () =
  let open Raylib in
  init_window window_width window_height "BlackJack";
  set_target_fps target_fps

let () = setup_game () |> Blackjack.State.init_state |> game_loop

(* test library stuff *)
(* do revealed card vs not revealed stuff *)
(* make sure todo double and split *)
