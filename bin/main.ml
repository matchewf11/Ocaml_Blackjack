(* Window Fields *)
let game_name = "Blackjack"
let target_fps = 60
let window_width = 800
let window_height = 600

(* Game Fields *)
let init_balence = 500
let init_bet = init_balence / 5

type phase = Betting | Playing | Won | Lost
type card = { rank : string; suit : string }

type game_state = {
  phase : phase;
  deck : card list;
  player_balance : int;
  player_bet : int;
  player_hand : card list;
  dealer_hand : card list;
}

let update_game game_state = game_state

let draw_game game_state =
  Raylib.begin_drawing ();
  Raylib.clear_background Raylib.Color.raywhite;
  (* draw hints *)
  (* draw phase *)
  (* draw deck *)
  (* draw player balence *)
  (* draw player bet *)
  (* draw player hand *)
  (* draw dealer hand *)
  Raylib.end_drawing ()

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
    player_balance = init_balence;
    player_bet = init_bet;
    player_hand = [];
    dealer_hand = [];
  }

let () = setup_game () |> game_loop
