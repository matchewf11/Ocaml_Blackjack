let target_fps = 60

let rec game_loop game_state =
  let open Blackjack in
  let open Raylib in
  (if window_should_close () then close_window ()
   else
     let updated_game = update_game game_state in
     draw_game updated_game;
     game_loop updated_game);
  ()

let setup_game () =
  let open Blackjack in
  let open Raylib in
  init_window window_width window_height "BlackJack";
  set_target_fps target_fps

let () = setup_game () |> Blackjack.init_state |> game_loop
