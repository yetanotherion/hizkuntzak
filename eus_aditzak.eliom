(**************************************************************************)
(*  Copyright 2014, Ion Alberdi <nolaridebi at gmail.com>                 *)
(*                                                                        *)
(*  Licensed under the Apache License, Version 2.0 (the "License");       *)
(*  you may not use this file except in compliance with the License.      *)
(*  You may obtain a copy of the License at                               *)
(*                                                                        *)
(*      http://www.apache.org/licenses/LICENSE-2.0                        *)
(*                                                                        *)
(*  Unless required by applicable law or agreed to in writing, software   *)
(*  distributed under the License is distributed on an "AS IS" BASIS,     *)
(*  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or       *)
(*  implied.  See the License for the specific language governing         *)
(*  permissions and limitations under the License.                        *)
(**************************************************************************)
{shared{
open Eliom_content
}}
{client{
open Animation

type rect = {
  start_x: float;
  start_y: float;
  height: float;
  width: float;
}

let rect_to_string x =
  Printf.sprintf "x: %f, y: %f, width:%f height:%f" x.start_x x.start_y x.width x.height

let create_rect x y height width =
  {start_x = x;
   start_y = y;
   height = height;
   width = width}

type t = {
  canvas: Dom_html.canvasElement Js.t;
  zone: rect;
}

let get_context t = t.canvas##getContext (Dom_html._2d_)

let reset t =
  let ctx = get_context t in
  ctx##clearRect(0.0, 0.0, t.zone.width, t.zone.height)

let enumerate l =
  let _, r = List.fold_left (fun (curr_idx, res) elt -> (curr_idx + 1, (curr_idx, elt) :: res)) (0, []) l in
  List.rev r

let create height width canvas =
  let dom_canvas = Html5.To_dom.of_canvas canvas in
  {
    zone = create_rect 0.0 0.0 (float_of_int height) (float_of_int width);
    canvas = dom_canvas;
  }

let draw_line t origin dest =
  let ctx = get_context t in
  let () = ctx##beginPath() in
  let () = ctx##moveTo(origin.x, origin.y) in
  let () = ctx##lineTo(dest.x, dest.y) in
  let () = ctx##closePath() in
  ctx##stroke()

let draw_horizontal_rects t bo x width height ylabels =
  let ctx = get_context t in
  (* let previousfillStyle = ctx##fillStyle in *)
  let f = match bo with
    | None -> fun y -> ctx##clearRect(x, y, width, height)

    | Some b ->
      let () = ctx##fillStyle <- Js.string b in
      fun y -> ctx##fillRect(x, y, width, height)
  in
  let () = List.iter f ylabels in
  (* let () = ctx##fillStyle <- previousfillStyle in *)
  ()

let draw_horizontal_lines_in_rect_zone t rect_zone l =
  List.iter (fun y -> draw_line t {x=rect_zone.start_x; y=y} {x=rect_zone.width; y=y}) l

let idx_to_y_label y_start rect_size =
  fun y -> y_start +. (float_of_int y) *. rect_size

let rect_idx_to_y_label y_start rect_size l = List.map (idx_to_y_label y_start rect_size) l

let alternate_horizontal_rectangles_in_rect_zone ?style:(s="rgb(204, 204, 255)") ?start_with_bg:(sbg=false) t rect_zone nb_rectangles =
  let rectangles_idx = range 0 nb_rectangles in
  let has_background =
    if sbg then fun x -> x mod 2 == 0
    else fun x -> x mod 2 == 1
  in
  let rect_with_background = List.filter has_background rectangles_idx in
  let rect_without_background = List.filter (fun x -> not (has_background x)) rectangles_idx in
  let each_rect_size = rect_zone.height /. (float_of_int nb_rectangles) in
  let to_y_labels = rect_idx_to_y_label rect_zone.start_y each_rect_size in
  let () = draw_horizontal_rects t None rect_zone.start_x rect_zone.width each_rect_size (to_y_labels rect_without_background) in
  draw_horizontal_rects t (Some s) rect_zone.start_x rect_zone.width each_rect_size (to_y_labels rect_with_background)

module Text = struct
  type location = [`Start | `Center | `End]

  type t = {
    x_location: location;
    y_location: location;
    police: string;
    size_in_pixel: int;
    text: string;
  }

  let create
      ?x_location:(xloc=`Center)
      ?y_location:(yloc=`Start)
      police size text =
    { x_location = xloc;
      y_location = yloc;
      police = police;
      size_in_pixel = size;
      text = text; }

  let message_x_length ctx string =
    let m = ctx##measureText(Js.string string) in
    m##width

  let message_length ctx t =
    message_x_length ctx t.text

  let set_police ctx size_in_pixel police_name =
    let police = Printf.sprintf "%dpx %s" size_in_pixel police_name in
    ctx##font <- Js.string police

  let text_fits_rectangle ctx police_in_pixel rectangle message =
    (* XXX check it fits the height too,
       police_in_pixel howevers does not seem to be the height
       of the letter *)
    (message_x_length ctx message) <= rectangle.width

  let write ctx t p = ctx##fillText(Js.string t.text, p.x, p.y)

  let get_x_axis axis_start axis_end location text_width =
    let half_of_text = text_width /. 2.0 in
    match location with
      | `Start -> axis_start
      | `End -> axis_end -. text_width
      | `Center -> ((axis_end +. axis_start) /. 2.0) -. half_of_text

  let get_y_axis axis_start axis_end location text_height =
    let half_of_text = text_height /. 2.0 in
    match location with
      | `Start -> (axis_start +. text_height)
      | `End -> axis_end
      | `Center -> ((axis_end +. axis_start) /. 2.0) -. half_of_text

  let size_text ctx police_name rectangle message =
    let tries = List.rev (range 5 25) in
    let res =
      List.fold_left (fun police_size curr_police_size ->
      match police_size with
        | None ->
          let () = set_police ctx curr_police_size police_name in
          if text_fits_rectangle ctx (float_of_int curr_police_size) rectangle message then
            Some curr_police_size
          else
            None
        | _ -> police_size) None tries
    in
    match res with
      | None -> raise (Failure "could not fix text in rectangle")
      | Some x -> x


  let get_position_in_rectangle c rectangle t =
    let () = set_police c t.size_in_pixel t.police in
    let x_start = rectangle.start_x in
    let x_end = rectangle.start_x +. rectangle.width in
    let text_size_in_x = message_x_length c t.text in
    let y_start = rectangle.start_y in
    let y_end = rectangle.start_y +. rectangle.height in
    let text_size_in_y = float_of_int t.size_in_pixel in
    let x = get_x_axis x_start x_end t.x_location text_size_in_x in
    let y = get_y_axis y_start y_end t.y_location text_size_in_y in
    {x=x;
     y=y}
end

let split_rectangle rectangle ncolumns nlines =
  let columns_width = rectangle.width /. (float_of_int ncolumns) in
  let line_height = rectangle.height /. (float_of_int (nlines + 1)) in
  let xidxl = range 0 ncolumns in
  let yidxl = range 0 nlines in
  let f_x = idx_to_y_label rectangle.start_x columns_width in
  let f_y = idx_to_y_label rectangle.start_y line_height in
  List.map (fun yidx ->
    let y_axis = f_y yidx in
    List.map (fun xidx -> {start_x=f_x xidx;
                           start_y=y_axis;
                           height=line_height;
                           width=columns_width}) xidxl) yidxl


module NorNork = struct
  let nb_of_rectangles = 10

   type elt = {
     position: point;
     text: Text.t;
     mutable next_position: point list;
   }

  let end_of_text_position t elt =
    let ctx = get_context t in
    {x=elt.position.x +. (Text.message_length ctx elt.text);
     y=elt.position.y}

  let draw_tabular t =
    let one_rectangle_size = t.zone.height /. (float_of_int nb_of_rectangles) in
    (* first we split the zone in t into two zones:
       - 9 rectangles of size one_rectangle_size
       - 1 rectangle of size one_rectangle_size * 2 in the end
         (so that we can put the asterisk for haiek)
     *)
    let boundary_y_idx = nb_of_rectangles - 2 in
    let zone_boundary = float_of_int (boundary_y_idx) in
    let first_zone = create_rect 0.0 0.0 (one_rectangle_size *. zone_boundary) t.zone.width in
    let second_zone = create_rect 0.0 (one_rectangle_size *. zone_boundary) (one_rectangle_size *. 2.0) t.zone.width in
    (* we draw the rectangles *)
    let () = alternate_horizontal_rectangles_in_rect_zone t first_zone boundary_y_idx in
    let () = alternate_horizontal_rectangles_in_rect_zone ~start_with_bg:true t second_zone 1 in
    (* and three horizontal lines as delimiter *)
    let to_y_labels = rect_idx_to_y_label 0.0 one_rectangle_size in
    draw_horizontal_lines_in_rect_zone t t.zone (to_y_labels [0; 1; 11])

  let draw_text_element t text_element =
    let position, next_position =
      match text_element.next_position with
        | [] -> text_element.position, []
        | hd :: tl -> hd, tl
    in
    let () = text_element.next_position <- next_position in
    Text.write (get_context t) text_element.text position

  let draw_text t text_table =
    let ctx = get_context t in
    let () = ctx##fillStyle <- Js.string "rgb(0, 0, 0)" in
    List.iter (fun l ->
      List.iter (fun x -> draw_text_element t x) l)
      text_table

  let create ?police_name:(p="serif") t =
    let rectangles = split_rectangle t.zone 4 (nb_of_rectangles - 1) in
    let cs x = (x, `Start) in
    let ce x = (x, `End) in

    let elts = [[cs "NOR"; cs "Beginning"; ce "Ending"; ce "NORK"];
                [cs "NI"; cs "NAU"; ce "T"; ce "NIK"];
                [cs "HI"; cs "HAU"; ce "K/N"; ce "HIK"];
                [cs "HURA"; cs "DU"; ce "-"; ce "HARK"];
                [cs "GU"; cs "GAITU"; ce "GU"; ce "GUK"];
                [cs "ZU"; cs "ZAITU"; ce "ZU"; ce "ZUK"];
                [cs "ZUEK"; cs "ZAITUZTE"; ce "ZUE"; ce "ZUEK"];
                [cs "HAIEK"; cs "DITU"; ce "(Z)* TE"; ce "HAIEK"];
                [cs ""; cs ""; cs "*: NOR=GU,ZU,HAIEK"; cs ""]] in

    let ctx = get_context t in
    List.map2 (fun line_rect line_elts ->
      List.map2 (fun rect (elt, loc) ->
        let size = Text.size_text ctx p rect elt in
        let text = Text.create ~x_location:loc p size elt in
        {position=Text.get_position_in_rectangle ctx rect text;
         text=text;
         next_position=[]})
        line_rect line_elts)
      rectangles elts

  let draw t text =
    let () = reset t in
    let () = draw_tabular t in
    draw_text t text

end

}}

{server{

let create_canvas_elt height width =
  Html5.D.canvas ~a:[ Html5.D.a_width width; Html5.D.a_height height ]
           [Html5.D.pcdata "your browser doesn't support canvas";
            Html5.D.br ()]

 }}
{client{

let compute_line_animation ?nb_of_steps:(ns=100) wait_before_move t table start ending =
  let ctx = get_context t in
  let middle = {x=t.zone.width /. 2.0;
                y=t.zone.height /. 2.0} in
  let get_elt_length ctx elt =
    Text.message_x_length ctx elt.NorNork.text.Text.text
  in
  let start_length = get_elt_length ctx start in
  let start_destination = {x=middle.x -. start_length;
                           y=middle.y} in
  let ending_destination = middle in
  let _compute_move elt dst =
    let src = elt.NorNork.position in
    let line = Line.create src dst in
    let start_x = src.x in
    let x_step = (dst.x -. src.x) /. (float_of_int ns) in
    let x_range = List.map (fun x -> start_x +. (float_of_int x) *. x_step) (range 0 ns) in
    List.map (fun x -> {x=x;
                        y=Line.y_axis line x}) x_range
  in
  let repeat_move position ns =
    List.fold_left (fun accum _ -> position :: accum) [] (range 0 ns)
  in
  let stay_after_last_move move =
    let last = List.hd (List.rev move) in
    move @ (repeat_move last ns)
  in
  let compute_move elt dst =
    let start = repeat_move elt.NorNork.position wait_before_move in
    stay_after_last_move (start @ (_compute_move elt dst))
  in
  let () = start.NorNork.next_position <- compute_move start start_destination in
  let () = ending.NorNork.next_position <- compute_move ending ending_destination in
  ()

let compute_one_rebound ?nb_of_steps:(ns=200) t table src dest =
  let v = SymBasketball.create src dest in
  let x_steps = (dest.x -. src.x) /. (float_of_int ns) in
  let x = List.map (fun i -> src.x +. x_steps *. (float_of_int i)) (range 0 ns) in
  List.map (fun x ->
    {y=SymBasketball.compute_ordinate v x;
     x=x}) x

let compute_jump ?nb_of_steps:(ns=200) ?max_jumps:(mj=10) t table elt dest =
  let src = elt.NorNork.position in
  let num_of_jumps = (Random.int (mj - 1) + 1) in
  let step_for_everyone = ns / num_of_jumps in
  let remaining_steps = ns - (step_for_everyone * num_of_jumps) in
  let x_shift = (dest.x -. src.x) /. (float_of_int num_of_jumps) in
  let res =
    List.fold_left (fun accum i ->
      let nb_of_steps = step_for_everyone in
      let nb_of_steps =
        if i == num_of_jumps - 1 then nb_of_steps + remaining_steps
        else nb_of_steps
      in
      let dest = {x=src.x +. (float_of_int (i + 1)) *. x_shift;
                  y=src.y} in
      let src = {x=src.x +. (float_of_int i) *. x_shift;
                 y=src.y}
      in
      let curr_jump = compute_one_rebound ~nb_of_steps:nb_of_steps t table src dest in
      List.append accum curr_jump) [] (range 0 num_of_jumps)
  in
  elt.NorNork.next_position <- res

let set_animation t table =
  let nau = List.nth (List.nth table 1) 1 in
  let zu = List.nth (List.nth table 5) 2 in
  let before_nb_steps = 100 in
  let () = compute_line_animation before_nb_steps t table nau zu in
  let ni = List.nth (List.nth table 1) 0 in
  let zuk = List.nth (List.nth table 5) 3 in
  let ni_end = (NorNork.end_of_text_position t ni) in
  let ni_length = ni_end.x -. ni.NorNork.position.x in
  let () = compute_jump ~nb_of_steps:before_nb_steps
                        t table ni {x=nau.NorNork.position.x -. ni_length;
                                    y=nau.NorNork.position.y} in
  let () = compute_jump ~nb_of_steps:before_nb_steps
                        t table zuk (NorNork.end_of_text_position t zu) in
  ()

let init_client height width canvas_elt =
  let () = Random.self_init () in
  let t = create height width canvas_elt in
  let table = NorNork.create t in
  let () = set_animation t table in
  t, table

let refresh_ui t table =
  while_lwt true do
    lwt () = Lwt_js_events.request_animation_frame () in
    let () = NorNork.draw t table in
    Lwt.return_unit
  done

}}
{server{
let service unused unused_bis =
  let height, width = 300, 700 in
  let canvas_elt = create_canvas_elt height width in
  let _ = {unit{
    let t, table = init_client %height %width %canvas_elt in
    Lwt.async (fun () -> refresh_ui t table)
  }}
  in
  let page =
    Html5.D.html
    (Html5.D.head
       (Html5.D.title (Html5.D.pcdata "Aditzak how to"))
       [ Html5.D.css_link
	   ~uri:(Html5.D.make_uri
		   (Eliom_service.static_dir ()) ["css";"hizkuntzak.css"]) ();
	])
    (Html5.D.body [
       Html5.D.div ~a:[] [canvas_elt]])
  in
  Lwt.return page

}}
