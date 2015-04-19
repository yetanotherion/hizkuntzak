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

let create_canvas_elt height width =
  Html5.D.canvas ~a:[ Html5.D.a_width width; Html5.D.a_height height ]
           [Html5.D.pcdata "your browser doesn't support canvas";
            Html5.D.br ()]

module Questions = struct
  type genre = [ `Male | `Female ]
  type nork = [ `Nik | `Hik of genre | `Hark | `Guk | `Zuk | `Zuek | `Haiek ]
  type nor = [ `Ni | `Hi | `Hura | `Gu | `Zu | `Zuek | `Haiek ]
  type nori = [ `Niri | `Hiri of genre | `Hari | `Guri | `Zuri | `Zuei | `Haiei ]
  type question = [ `Nor of nor |`NorNork of (nor * nork) | `NorNori of (nor * nori)] * string * [ `Past | `Present ]
end

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
  mutable run: bool;
}

let get_context t = t.canvas##getContext (Dom_html._2d_)

let reset t =
  let ctx = get_context t in
  ctx##clearRect(0.0, 0.0, t.zone.width, t.zone.height)

let create height width canvas =
  let dom_canvas = Html5.To_dom.of_canvas canvas in
  {
    zone = create_rect 0.0 0.0 (float_of_int height) (float_of_int width);
    canvas = dom_canvas;
    run = true;
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

  let message_height t =
    (float_of_int t.size_in_pixel) /. 2.0

  let write ctx t p =
    let () = set_police ctx t.size_in_pixel t.police in
    ctx##fillText(Js.string t.text, p.x, p.y)

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

type elt = {
  position: point;
  text: Text.t;
  mutable next_position: point list;
}

let create_elt position text = {position=position;
                                text=text;
                                next_position=[]}

let end_of_text_position t elt =
  let ctx = get_context t in
  {x=elt.position.x +. (Text.message_length ctx elt.text);
   y=elt.position.y}

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
    List.iter (fun l ->
      List.iter (fun x -> draw_text_element t x) l) l)
    text_table

let repeat_move position ns =
  List.fold_left (fun accum _ -> position :: accum) [] (range 0 ns)

let compute_line_move ?nb_of_steps:(ns=200) src dst =
  let line = Line.create src dst in
  List.map (fun x -> {x=x;
                      y=Line.y_axis line x}) (xrange src dst ns)

let compute_line_animation ?nb_of_steps:(ns=200) wait_before_move t table blocks =
  let ctx = get_context t in
  let middle = {x=t.zone.width /. 2.0;
                y=t.zone.height /. 2.0} in
  let get_elt_length ctx elt =
    Text.message_x_length ctx elt.text.Text.text
  in
  let start = List.hd blocks in
  let start_length = get_elt_length ctx start in
  let start_destination = {x=middle.x -. start_length;
                           y=middle.y} in
  let stay_after_last_move move =
    let last = List.hd (List.rev move) in
    move @ (repeat_move last ns)
  in
  let compute_move elt dst =
    let start = repeat_move elt.position wait_before_move in
    stay_after_last_move (start @ (compute_line_move ~nb_of_steps:ns elt.position dst))
  in
  let () = start.next_position <- compute_move start start_destination in
  let _, src_dest = List.fold_left (fun (previous_end, accum) elt ->
    let next_dest = {x=previous_end.x +. (get_elt_length ctx elt);
                     y=middle.y} in
    (next_dest, (elt, compute_move elt previous_end) :: accum)) (middle, []) (List.tl blocks)
  in
  let src_dest = List.rev src_dest in
  let () = List.iter (fun (elt, move) ->
    elt.next_position <- move) src_dest in
  ()

let compute_one_rebound ?nb_of_steps:(ns=200) t table src dest =
  let v = SymBasketball.create src dest in
  List.map (fun x ->
    {y=SymBasketball.compute_ordinate v x;
     x=x}) (xrange src dest ns)

let compute_jump ?nb_of_steps:(ns=200) ?number_of_jumps:(nj=2) t table elt dest =
  let src = elt.position in
  let res =
    let step_for_everyone = ns / nj in
    let remaining_steps = ns - step_for_everyone * nj in
    let x_range = xrange src dest (nj + 1) in
    let _, src_dest = List.fold_left
      (fun (previous_dest, accum) dest ->
        (dest, (previous_dest, dest) :: accum))
      (List.hd x_range, []) (List.tl x_range)
    in
    let src_dest = enumerate (List.rev src_dest) in
    List.fold_left (fun accum (i, (src_x, dest_x)) ->
      let nb_of_steps = step_for_everyone in
      let nb_of_steps =
        if i == nj - 1 then nb_of_steps + remaining_steps
        else nb_of_steps
      in
      let src = {x=src_x;
                 y=src.y} in
      let dest = {x=dest_x;
                  y=dest.y} in
      let curr_jump = compute_one_rebound ~nb_of_steps:nb_of_steps t table src dest in
      List.append accum curr_jump) [] src_dest
  in
  elt.next_position <- res

module NorNork = struct
  let nb_of_rectangles = 10

  type param = Questions.nor * Questions.nork

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

  let create ?police_name:(p="serif") t =
    let rectangles = split_rectangle t.zone 4 (nb_of_rectangles - 1) in
    let cs x = ([x], `Start) in
    let ce x = ([x], `End) in
    let ceL x = (x, `End) in

    let elts = [[cs "NOR"; cs "Beginning"; ce "Ending"; ce "NORK"];
                [cs "NI"; cs "NAU"; ce "T"; ce "NIK"];
                [cs "HI"; cs "HAU"; ceL ["K"; "/"; "N"]; ce "HIK"];
                [cs "HURA"; cs "DU"; ce "-"; ce "HARK"];
                [cs "GU"; cs "GAITU"; ce "GU"; ce "GUK"];
                [cs "ZU"; cs "ZAITU"; ce "ZU"; ce "ZUK"];
                [cs "ZUEK"; cs "ZAITUZTE"; ce "ZUE"; ce "ZUEK"];
                [cs "HAIEK"; cs "DITU"; ceL ["("; "Z"; ")*"; "TE"]; ce "HAIEK"];
                [cs ""; cs ""; ceL ["*: NOR="; "GU"; ","; "ZU"; ","; "HAIEK"]; ce ""]] in
    let ctx = get_context t in
    List.map2 (fun line_rect line_elts ->
      List.map2 (fun rect locElts ->
        let (elts, loc) = locElts in
        let all_elt_string = List.fold_left (fun accum x -> accum ^ x) "" elts in
        let size = Text.size_text ctx p rect all_elt_string in
        let position = Text.get_position_in_rectangle ctx rect
          (Text.create ~x_location:loc p size all_elt_string)
        in
        let _, all_elts = List.fold_left
          (fun (current_position, accum) message ->
            let curr_text = Text.create ~x_location:loc p size message in
            let curr_elt = create_elt current_position curr_text in
            let end_position = end_of_text_position t curr_elt in
            (end_position, curr_elt :: accum)) (position, []) elts
        in
        List.rev all_elts)
        line_rect line_elts)
      rectangles elts

  let draw t text =
    let () = reset t in
    let () = draw_tabular t in
    draw_text t text

  let get_elements_to_move table norNork =
    let nor, nork = norNork in

    let nor_to_idx nor =
      match nor with
        | `Ni -> 1
        | `Hi -> 2
        | `Hura -> 3
        | `Gu -> 4
        | `Zu -> 5
        | `Zuek -> 6
        | `Haiek -> 7
    in

    let nork_to_idx nork =
      match nork with
        | `Nik -> 1
        | `Hik _ -> 2
        | `Hark -> 3
        | `Guk -> 4
        | `Zuk -> 5
        | `Zuek -> 6
        | `Haiek -> 7
    in

    let get_nor_element_in_table nor =
      List.hd (List.nth (List.nth table (nor_to_idx nor)) 0)
    in

    let get_nor_verb_part_element_in_table nor =
      List.hd (List.nth (List.nth table (nor_to_idx nor)) 1)
    in

    let get_nork_verb_part_in_table nork =
      let elt = List.nth (List.nth table (nork_to_idx nork)) 2 in
      match nork with
        | `Nik |  `Hark | `Guk | `Zuk | `Zuek | `Hik `Male ->
          List.hd elt
        | `Hik `Female -> List.nth elt 2
        | `Haiek -> List.nth elt 3
    in

    let get_nork_element_in_table nork =
      List.hd (List.nth (List.nth table (nork_to_idx nork)) 3)
    in

    let nor_sure =
      get_nor_element_in_table nor,
      get_nor_verb_part_element_in_table nor
    in

    let nork_sure =
      get_nork_element_in_table nork,
      get_nork_verb_part_in_table nork
    in

    let haiek_optional_verb_part = List.nth (List.nth (List.nth table 7) 2) 1 in
    let nor_optional_verb_part = List.nth (List.nth table 8) 2 in
    let nor_optional =
      match nork with
        | `Nik | `Hik _ | `Hark | `Guk | `Zuk | `Zuek -> None
        | `Haiek -> begin
          match nor with
            | `Ni | `Hi | `Hura | `Zuek -> None
            | `Gu -> Some (List.nth nor_optional_verb_part 1,
                           haiek_optional_verb_part)
            | `Zu -> Some (List.nth nor_optional_verb_part 3,
                           haiek_optional_verb_part)
            | `Haiek -> Some (List.nth nor_optional_verb_part 5,
                              haiek_optional_verb_part)
        end
    in
    nor_sure, nork_sure, nor_optional

  let set_animation ?nb_of_steps:(ns=100) ?max_num_of_jumps:(mnj=2) t table norNork =
    let (nor, nor_verb), (nork, nork_verb), nor_optional = get_elements_to_move table norNork in
    let steps_until_touching_verb_parts = ns / 2 in
    let line_animation_l =
      match nor_optional with
        | None -> [nor_verb; nork_verb]
        | Some (_, z) -> [nor_verb; z; nork_verb]
    in

    let () = compute_line_animation ~nb_of_steps:ns steps_until_touching_verb_parts t table line_animation_l in
    let nor_end = (end_of_text_position t nor) in
    let nor_length = nor_end.x -. nor.position.x in
    let num_of_jumps = (Random.int mnj) + 1 in
    let () = compute_jump ~nb_of_steps:steps_until_touching_verb_parts
                          ~number_of_jumps:num_of_jumps
                          t table nor {x=nor_verb.position.x -. nor_length;
                                      y=nor_verb.position.y} in
    let () = compute_jump ~nb_of_steps:steps_until_touching_verb_parts
                          ~number_of_jumps:num_of_jumps
                          t table nork (end_of_text_position t nork_verb) in
    match nor_optional with
      | None -> ()
      | Some (nor_star, z) ->
        nor_star.next_position <- compute_line_move ~nb_of_steps:steps_until_touching_verb_parts
          nor_star.position
          {x=z.position.x;
           y=z.position.y +. (Text.message_height nor_star.text);
          }
end


type table = elt list list list

type animation = {
  canvas: t;
  table: table;
  mutable nb_times_animation_is_run: int;
}

module type Table = sig
  type param
  val create: ?police_name:string -> t -> table
  val draw: t -> table -> unit
  val set_animation: ?nb_of_steps:int -> ?max_num_of_jumps:int -> t -> table -> param -> unit
end


module MakeAnimation (T:Table) = struct

  let compute_nb_of_animation_steps t =
    (* the same javascript code is executed faster each execution
       (most probably due to optimizations like
       https://www.webkit.org/blog/3362/introducing-the-webkit-ftl-jit/).
       This has the side effect of making the same animation
       go faster at each execution. It makes understanding
       the tables harder, which is the opposite of the animation goal.
       We count the number of times the animation is run
       as a workaround to slow down the animation execution.
    *)
    if t.nb_times_animation_is_run = 0 then 100
    else
      let max_nb_steps = 500 in
      let slow_down_factor = int_of_float (100.0 /. float_of_int t.nb_times_animation_is_run) in
      Pervasives.min max_nb_steps (100 + slow_down_factor)

  let create_animation height width canvas_elt =
    let () = Random.self_init () in
    let t = create height width canvas_elt in
    let table = T.create t in
    let () = T.draw t table in
    {canvas=t;
     table=table;
     nb_times_animation_is_run=0}

  let start_animation t param =
    let () = T.set_animation ~nb_of_steps:(compute_nb_of_animation_steps t) t.canvas t.table param in
    let () = t.nb_times_animation_is_run <-
      t.nb_times_animation_is_run + 1
    in
    while_lwt t.canvas.run do
      lwt () = Lwt_js_events.request_animation_frame () in
      let () = T.draw t.canvas t.table in
      Lwt.return_unit
    done

  let stop_animation t = t.canvas.run <- false
 end

module NorNorkAnimation = MakeAnimation (NorNork)

}}
