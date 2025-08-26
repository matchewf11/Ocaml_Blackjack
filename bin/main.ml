(* Window Fields *)
let game_name = "Blackjack"
let target_fps = 60
let header_height = 30
let window_width = 800
let window_height = 600

(* Header fields *)
let cushion = 2
let font_size = 20
let balence_x = window_width / 4
let bet_x = window_width / 2
let fps_x = window_width / 4 * 3

(* Game Fields *)
let init_balence = 500
let init_bet = init_balence / 5

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
  clear_background Color.raywhite;

  (* Draw Header *)
  draw_rectangle 0 0 window_width header_height Color.brown;
  draw_text
    ("Phase: " ^ string_of_phase game_state.phase)
    cushion cushion font_size Color.black;
  draw_text
    ("Balance: " ^ string_of_int game_state.player.balance)
    balence_x cushion font_size Color.black;
  draw_text
    ("Bet: " ^ string_of_int game_state.player.bet)
    bet_x cushion font_size Color.black;
  draw_fps fps_x cushion;

  (* Draw Board *)
  draw_rectangle 0 header_height window_width
    (window_height - header_height)
    Color.green;

  (* draw deck *)
  (* draw player hand *)
  (* draw dealer hand *)
  end_drawing ()

let rec game_loop game_state =
  (if Raylib.window_should_close () then Raylib.close_window ()
   else
     let updated_game = update_game game_state in
     draw_game updated_game;
     game_loop updated_game);
  ()

let setup_game () =
  Raylib.init_window window_width window_height game_name;
  Raylib.set_target_fps target_fps;
  {
    phase = Betting;
    deck = [];
    player = { balance = init_balence; bet = init_bet; hand = [] };
    dealer_hand = [];
  }

let () = setup_game () |> game_loop
