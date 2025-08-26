let game_name = "Blackjack"
let target_fps = 60
let window_width = 800
let window_height = 600

let draw_game () =
  Raylib.begin_drawing ();
  Raylib.clear_background Raylib.Color.raywhite;
  Raylib.end_drawing ()

let rec game_loop () =
  if Raylib.window_should_close () then Raylib.close_window ()
  else draw_game () |> game_loop

let setup_game () =
  Raylib.init_window window_width window_height game_name;
  Raylib.set_target_fps target_fps

let () = setup_game () |> game_loop
