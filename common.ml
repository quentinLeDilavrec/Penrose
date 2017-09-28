#load "graphics.cma";;
open Graphics;;


let random_color() =
  let r = Random.int 255
  and g = Random.int 255
  and b = Random.int 255
  in rgb r g b;;


(* Create a new window *)
let open_graph2 width height =
open_graph (" "^(string_of_int width)^"x"^(string_of_int height)^"+0-0");;

let keep_open () =
ignore (Graphics.read_key ());;