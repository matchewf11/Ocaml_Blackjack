(* Absolute Fields *)
let target_fps = 60
let window_width = 800
let window_height = 600
let init_balence = 500

(* Relative fields *)
let header_height = window_height / 20
let font_size = (window_width + window_height) / 70
let cushion = (window_width + window_height) / 700
let balance_x = window_width / 4
let bet_x = window_width / 2
let fps_x = window_width / 4 * 3
let init_bet = init_balence / 5
let deck_text_x = (window_width / 2) - (font_size * 2)
let deck_text_y = window_height / 8
let card_width = window_width / 8
let card_height = window_height / 4

type phase = Betting | Playing | Won | Lost
type card = { rank : string; suit : string }
type player = { hand : card list; balance : int; bet : int }

type game_state = {
  phase : phase;
  deck : card list;
  player : player;
  dealer_hand : card list;
}

let string_of_phase = function
  | Betting -> "Betting"
  | Playing -> "Playing"
  | Won -> "Won"
  | Lost -> "Lost"

let update_game game_state = game_state

let draw_game game_state =
  let open Raylib in
  begin_drawing ();

  draw_rectangle 0 0 window_width header_height Color.brown;
  draw_text
    ("Phase: " ^ string_of_phase game_state.phase)
    cushion cushion font_size Color.black;
  draw_text
    ("Balance: " ^ string_of_int game_state.player.balance)
    balance_x cushion font_size Color.black;
  draw_text
    ("Bet: " ^ string_of_int game_state.player.bet)
    bet_x cushion font_size Color.black;
  draw_fps fps_x cushion;

  draw_rectangle 0 header_height window_width
    (window_height - header_height)
    Color.green;

  draw_rectangle deck_text_x deck_text_y card_width card_height Color.red;
  draw_text "Deck" deck_text_x deck_text_y font_size Color.black;

  (* draw player and dealer hand *)
  end_drawing ()

let rec game_loop game_state =
  (if Raylib.window_should_close () then Raylib.close_window ()
   else
     let updated_game = update_game game_state in
     draw_game updated_game;
     game_loop updated_game);
  ()

let setup_game () =
  Raylib.init_window window_width window_height "BlackJack";
  Raylib.set_target_fps target_fps;
  {
    phase = Betting;
    deck = [];
    player = { balance = init_balence; bet = init_bet; hand = [] };
    dealer_hand = [];
  }

let () = setup_game () |> game_loop
