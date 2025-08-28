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

let draw_game game_state =
  let open Raylib in
  let open Blackjack.State in
  let draw_card x y color =
    Raylib.draw_rectangle x y card_width card_height color;
    Raylib.draw_rectangle_lines x y card_width card_height Raylib.Color.black
  in
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
        (fun acc c -> acc ^ " " ^ Blackjack.Card.string_of_card c)
        " " game_state.player_hand
  in
  print_endline player_output;

  let dealer_output =
    "DealerHand: "
    ^ string_of_int (int_of_hand game_state.dealer_hand)
    ^ " : Revealed"
    ^ List.fold_left
        (fun acc c -> acc ^ " " ^ Blackjack.Card.string_of_card c)
        " " game_state.dealer_hand
  in
  print_endline dealer_output;
  end_drawing ()

let rec game_loop game_state =
  let open Raylib in
  let open Blackjack.State in
  if window_should_close () then close_window ()
  else
    let updated_game =
      Blackjack.State.update_game game_state
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
    in
    draw_game updated_game;
    game_loop updated_game

let setup_game () =
  let open Raylib in
  init_window window_width window_height "BlackJack";
  set_target_fps target_fps

let () = setup_game () |> Blackjack.State.init_state |> game_loop
