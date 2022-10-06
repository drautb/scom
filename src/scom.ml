open ANSITerminal;;
open BatString;;

open Constants;;
open Types;;

Random.init 1;;

let initialize_world () = {
  squads = [{ soldiers=[{ x=1; y=1; avatar='A'; aim=5; hp=9; def=0; actions=2 };
                        { x=3; y=1; avatar='B'; aim=5; hp=9; def=0; actions=2 };
                        { x=2; y=2; avatar='C'; aim=5; hp=9; def=0; actions=2 };
                        { x=4; y=2; avatar='D'; aim=5; hp=9; def=0; actions=2 }]};
            { soldiers=[{ x=95; y=27; avatar='A'; aim=5; hp=9; def=0; actions=2 };
                        { x=97; y=27; avatar='B'; aim=5; hp=9; def=0; actions=2 };
                        { x=96; y=28; avatar='C'; aim=5; hp=9; def=0; actions=2 };
                        { x=98; y=28; avatar='D'; aim=5; hp=9; def=0; actions=2 }]}];
  cover = [{ x=13; y=6; def=2 };
           { x=19; y=7; def=1 }];
  turn = Random.int 2;
  (* pending_cmd = None; *)
  (* pending_cmd = Command(Move({x=1; y=1; avatar='A'; hp=9; def=0; actions=2;}, 7, 3)); *)
  pending_cmd = Command(Attack({ x=1; y=1; avatar='A'; aim=5; hp=9; def=0; actions=2 },{ x=98; y=28; avatar='D'; aim=5; hp=9; def=0; actions=2 }));
};;

let process_command cmd =
  let cmd = String.lowercase cmd in
  print_string [View.background_color; blue] cmd;;

(*
MAIN LOOP
*)
let main () =
  let the_world = initialize_world () in

  let rec loop () =
    erase Screen;
    View.print_world the_world;
    View.print_stats the_world;
    View.print_prompt the_world;

    process_command (read_line());

    loop () in

  loop ();;

(* Get this party started *)
print_endline "\nWelcome to S-COM.\n\n";;
Unix.sleep(0);;

View.clear_screen ();;
main ();;

