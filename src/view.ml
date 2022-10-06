open ANSITerminal;;

open Constants;;
open Types;;
open Jarvis;;

let background_color = on_default;;
let border_color = black;;
let label_color = blue;;
let tree_color = green;;
let rock_color = black;;

let squad_colors = [cyan; magenta];;

let print_newline () =
  print_string [background_color; border_color] "\n";;

let print_horizontal_border () =
  print_string [background_color; border_color] (String.make (world_width + 2) '=');;

let print_vertical_border () =
  print_string [background_color; border_color] "|";;

let print_space () =
  print_string [background_color; background_color] " ";;

let print_spacer n =
  print_string [background_color; background_color] (String.make n ' ');;

let print_tree () =
  print_string [background_color; green] "T";;

let print_rock () =
  print_string [background_color; black] "o";;

let print_world w =
  let generate_texel_list w =
    let create_soldier_texel color (soldier:soldier) =
      { x=soldier.x; y=soldier.y; color=[color]; value=soldier.avatar } in
    let create_cover_texel cover =
      match cover.def with
      | 1 -> { x=cover.x; y=cover.y; color=[rock_color]; value='o' }
      | _ -> { x=cover.x; y=cover.y; color=[tree_color]; value='T' } in
    let create_highlight_texel () =
      match w.pending_cmd with
      | Command(Move(s, h, v)) -> [{ x=(s.x + h); y=(s.y + v); color=[Inverse; blue]; value='X' }]
      | _ -> [] in
    let cmp_texel t1 t2 =
      if t1.y = t2.y then t1.x - t2.x else t1.y - t2.y in
    let texels = List.fold_left List.append [] [(List.map (create_soldier_texel (List.nth squad_colors 0)) (List.nth w.squads 0).soldiers);
                                                (List.map (create_soldier_texel (List.nth squad_colors 1)) (List.nth w.squads 1).soldiers);
                                                (List.map create_cover_texel w.cover);
                                                create_highlight_texel ()] in
      List.sort cmp_texel texels in

  let print_item texels x y =
    if List.length texels > 0 then begin
      let item = List.hd texels in
      if item.x = x && item.y = y then begin
        print_string (List.append [background_color] item.color) (String.make 1 item.value);
        List.tl texels;
      end else begin
        print_space ();
        texels;
      end;
    end else begin
      print_space ();
      texels;
    end in

  let rec loop_y y texels =
    let rec loop_x x texels =
      let texels = print_item texels x y in
      if x = (world_width-1) then texels else loop_x (x+1) texels in
    print_vertical_border ();
    let texels = loop_x 0 texels in
    print_vertical_border ();
    print_newline ();
    if y = (world_height-1) then () else loop_y (y+1) texels in

  print_newline (); (* This prevents the last command from pushing the top border over *)
  print_horizontal_border ();
  print_newline ();

  let texels = generate_texel_list w in
  loop_y 0 texels;;

let print_stats w =
  let print_empty_line () =
    print_vertical_border ();
    print_spacer 24;
    print_vertical_border ();
    print_spacer 50;
    print_vertical_border ();
    print_spacer 24 ;
    print_vertical_border ();
    print_newline () in

  let print_hp hp =
    let color = match hp with
    | _ when hp > 6 -> green
    | _ when hp > 3 -> yellow
    | _ -> red in
    print_string [background_color; color] ((string_of_int hp) ^ "/" ^ (string_of_int max_hp));
    print_string [background_color; label_color] " HP" in

  let print_def def =
    let color = match def with
    | _ when def < 2 -> red
    | _ when def < 4 -> yellow
    | _ -> green in
    print_string [background_color; color] (string_of_int def);
    print_string [background_color; label_color] " DEF" in

  let print_actions act =
    let color = match act with
    | _ when act > 1 -> green
    | _ when act = 1 -> yellow
    | _ -> red in
    print_string [background_color; color] (string_of_int act);
    print_string [background_color; label_color] " ACT" in

  print_horizontal_border ();
  print_newline ();
  print_empty_line ();

  (* LINE 1 *)
  print_vertical_border ();

  let soldier = (List.nth (List.nth w.squads 0).soldiers 0) in
  print_string [background_color; (List.nth squad_colors 0)] (" " ^ (String.make 1 soldier.avatar) ^ ": ");
  print_hp soldier.hp;
  print_spacer 3;

  let soldier = (List.nth (List.nth w.squads 0).soldiers 1) in
  print_string [background_color; (List.nth squad_colors 0)] ((String.make 1 soldier.avatar) ^ ": ");
  print_hp soldier.hp;
  print_spacer 2;

  print_vertical_border ();
  let my_color = (List.nth squad_colors w.turn) in
  let opponent_color = if w.turn = 0 then (List.nth squad_colors 1) else (List.nth squad_colors 0) in
  let _ = match w.pending_cmd with
  | Command(Attack(s1, s2)) -> begin
    print_spacer 17;
    print_string [background_color; my_color] (String.make 1 s1.avatar);
    print_string [background_color; red] "    ATTACKS    ";
    print_string [background_color; opponent_color] (String.make 1 s2.avatar);
    print_spacer 16;
  end
  | _ -> print_spacer 50 in
  print_vertical_border ();

  let soldier = (List.nth (List.nth w.squads 1).soldiers 0) in
  print_string [background_color; (List.nth squad_colors 1)] (" " ^ (String.make 1 soldier.avatar) ^ ": ");
  print_hp soldier.hp;
  print_spacer 3;

  let soldier = (List.nth (List.nth w.squads 1).soldiers 1) in
  print_string [background_color; (List.nth squad_colors 1)] ((String.make 1 soldier.avatar) ^ ": ");
  print_hp soldier.hp;
  print_spacer 2;

  print_vertical_border ();
  print_newline ();

  (* LINE 2 *)
  print_vertical_border ();

  let soldier = (List.nth (List.nth w.squads 0).soldiers 0) in
  print_spacer 6;
  print_def soldier.def;

  let soldier = (List.nth (List.nth w.squads 0).soldiers 1) in
  print_spacer 7;
  print_def soldier.def;
  print_space ();

  print_vertical_border ();
  let my_color = (List.nth squad_colors w.turn) in
  let opponent_color = if w.turn = 0 then (List.nth squad_colors 1) else (List.nth squad_colors 0) in
  let _ = match w.pending_cmd with
  | Command(Attack(s1, s2)) -> begin
    print_spacer 12;
    print_string [background_color; my_color] ("AIM: " ^ (string_of_int s1.aim));
    print_spacer 15;
    print_string [background_color; opponent_color] ("DEF: " ^ (string_of_int s2.def));
    print_spacer 11;
  end
  | _ -> print_spacer 50 in
  print_vertical_border ();

  let soldier = (List.nth (List.nth w.squads 1).soldiers 0) in
  print_spacer 6;
  print_def soldier.def;

  let soldier = (List.nth (List.nth w.squads 1).soldiers 1) in
  print_spacer 7;
  print_def soldier.def;
  print_space ();

  print_vertical_border ();
  print_newline ();

  (* LINE 3 *)
  print_vertical_border ();

  let soldier = (List.nth (List.nth w.squads 0).soldiers 0) in
  print_spacer 6;
  print_actions soldier.actions;

  let soldier = (List.nth (List.nth w.squads 0).soldiers 1) in
  print_spacer 7;
  print_actions soldier.actions;
  print_space ();

  print_vertical_border ();
  print_spacer 50;
  print_vertical_border ();

  let soldier = (List.nth (List.nth w.squads 1).soldiers 0) in
  print_spacer 6;
  print_actions soldier.actions;

  let soldier = (List.nth (List.nth w.squads 1).soldiers 1) in
  print_spacer 7;
  print_actions soldier.actions;
  print_space ();

  print_vertical_border ();
  print_newline ();

  (* LINE 4 *)
  print_empty_line ();

  (* LINE 5 *)
  print_vertical_border ();

  let soldier = (List.nth (List.nth w.squads 0).soldiers 2) in
  print_string [background_color; (List.nth squad_colors 0)] (" " ^ (String.make 1 soldier.avatar) ^ ": ");
  print_hp soldier.hp;
  print_spacer 3;

  let soldier = (List.nth (List.nth w.squads 0).soldiers 3) in
  print_string [background_color; (List.nth squad_colors 0)] ((String.make 1 soldier.avatar) ^ ": ");
  print_hp soldier.hp;
  print_spacer 2;

  print_vertical_border ();
  print_spacer 50;
  print_vertical_border ();

  let soldier = (List.nth (List.nth w.squads 1).soldiers 2) in
  print_string [background_color; (List.nth squad_colors 1)] (" " ^ (String.make 1 soldier.avatar) ^ ": ");
  print_hp soldier.hp;
  print_spacer 3;

  let soldier = (List.nth (List.nth w.squads 1).soldiers 3) in
  print_string [background_color; (List.nth squad_colors 1)] ((String.make 1 soldier.avatar) ^ ": ");
  print_hp soldier.hp;
  print_spacer 2;

  print_vertical_border ();
  print_newline ();

  (* LINE 6 *)
  print_vertical_border ();

  let soldier = (List.nth (List.nth w.squads 0).soldiers 2) in
  print_spacer 6;
  print_def soldier.def;

  let soldier = (List.nth (List.nth w.squads 0).soldiers 3) in
  print_spacer 7;
  print_def soldier.def;
  print_space ();

  print_vertical_border ();
  print_spacer 50;
  print_vertical_border ();

  let soldier = (List.nth (List.nth w.squads 1).soldiers 2) in
  print_spacer 6;
  print_def soldier.def;

  let soldier = (List.nth (List.nth w.squads 1).soldiers 3) in
  print_spacer 7;
  print_def soldier.def;
  print_space ();

  print_vertical_border ();
  print_newline ();

  (* LINE 7 *)
  print_vertical_border ();

  let soldier = (List.nth (List.nth w.squads 0).soldiers 2) in
  print_spacer 6;
  print_actions soldier.actions;

  let soldier = (List.nth (List.nth w.squads 0).soldiers 3) in
  print_spacer 7;
  print_actions soldier.actions;
  print_space ();

  print_vertical_border ();
  print_spacer 50;
  print_vertical_border ();

  let soldier = (List.nth (List.nth w.squads 1).soldiers 2) in
  print_spacer 6;
  print_actions soldier.actions;

  let soldier = (List.nth (List.nth w.squads 1).soldiers 3) in
  print_spacer 7;
  print_actions soldier.actions;
  print_space ();

  print_vertical_border ();
  print_newline ();

  (* LINE 8 *)
  print_empty_line ();;

let print_prompt w =
  let my_color = (List.nth squad_colors w.turn) in
  let opponent_color = if w.turn = 0 then (List.nth squad_colors 1) else (List.nth squad_colors 0) in
  let prompt_attack s1 s2 =
    print_string [background_color; my_color] ((String.make 1 s1.avatar));
    print_string [background_color; red] " ATTACKS ";
    print_string [background_color; opponent_color] (String.make 1 s2.avatar);
    print_string [background_color; my_color] " CONTINUE? (Y/N)" in
  let prompt_move (s:soldier) h v =
    let new_x = s.x + h in
    let new_y = s.y + v in
    let new_pos_str = "[" ^ (string_of_int new_x) ^ "," ^ (string_of_int new_y) ^ "]" in
    print_string [background_color; my_color] ("MOVE " ^ (String.make 1 s.avatar) ^ " TO " ^ new_pos_str ^ ". CONTINUE? (Y/N)") in
  let prompt_cover s =
    () in

  print_horizontal_border ();
  print_newline ();

  let prompt_str = "|> P" ^ (string_of_int (w.turn + 1)) ^ " - " in
  print_string [background_color; my_color] prompt_str;

  let _  = match w.pending_cmd with
  | None -> print_string [background_color; my_color] "[]"
  | Command(Attack(s1, s2)) -> prompt_attack s1 s2
  | Command(Move(s, h, v)) -> prompt_move s h v
  | Command(Cover(s)) -> prompt_cover s in

  print_string [background_color; my_color] " -> ";;

(* This is a hack to make sure that the game shows up at the bottom of a newly opened terminal *)
let clear_screen () =
  let rec loop n =
    if n = 0 then () else begin print_newline (); loop (n-1); end; in
  loop 100;
  erase Screen;;
