{shared{
open Eliom_content
}}
{client{

type point = {
  x: float;
  y: float;
}

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

let log s = Firebug.console##log(Js.string s)

let range ?step:(s=1) start_idx end_idx =
  (* range 0 3 == [0; 1; 2] *)
  let rec _range cidx eidx accum =
    if cidx + s >= eidx then List.rev (cidx :: accum)
    else _range (cidx + s) eidx (cidx :: accum)
  in
  _range start_idx end_idx []

let enumerate l =
  let _, r = List.fold_left (fun (curr_idx, res) elt -> (curr_idx + 1, (curr_idx, elt) :: res)) (0, []) l in
  List.rev r

let create height width canvas =
  let dom_canvas = Html5.To_dom.of_canvas canvas in
  {
    zone = create_rect 0.0 0.0 (float_of_int height) (float_of_int width);
    canvas = dom_canvas;
  }

let get_context t = t.canvas##getContext (Dom_html._2d_)


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
  let ps = Printf.sprintf in
  let f = match bo with
    | None -> fun y -> begin
      let () = log (ps "Writting rectangle x: %f y: %f width: %f height:%f" x y width height) in
      ctx##clearRect(x, y, width, height)
    end
    | Some b ->
      let () = ctx##fillStyle <- Js.string b in
      fun y -> begin
        let () = log (ps "Writting rectangle x: %f y: %f width: %f height:%f" x y width height) in
        ctx##fillRect(x, y, width, height)
      end
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

  let set_police ctx size_in_pixel police_name =
    let police = Printf.sprintf "%dpx %s" size_in_pixel police_name in
    ctx##font <- Js.string police

  let text_fits_rectangle ctx police_in_pixel rectangle message =
    (* XXX check it fits the height too,
       police_in_pixel howevers does not seem to be the height
       of the letter *)
    (message_x_length ctx message) <= rectangle.width

  let write ctx t x y = ctx##fillText(Js.string t.text, x, y)

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


  let write_in_rectangle c rectangle t =
    let () = set_police c t.size_in_pixel t.police in
    let x_start = rectangle.start_x in
    let x_end = rectangle.start_x +. rectangle.width in
    let text_size_in_x = message_x_length c t.text in
    let y_start = rectangle.start_y in
    let y_end = rectangle.start_y +. rectangle.height in
    let text_size_in_y = float_of_int t.size_in_pixel in
    let x = get_x_axis x_start x_end t.x_location text_size_in_x in
    let y = get_y_axis y_start y_end t.y_location text_size_in_y in
    let () = log (Printf.sprintf "writting in rectangle %s x:%f y:%f" (rect_to_string rectangle) x y) in
    write c t x y
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



let create_nor_nork_tabular ?style:(s="rgb(204, 204, 255)") t =
  let nb_of_rectangles = 10 in
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
  let () = draw_horizontal_lines_in_rect_zone t t.zone (to_y_labels [0; 1; 11]) in
  (* now lets write the text *)
  let rectangles = split_rectangle t.zone 4 (nb_of_rectangles - 1) in
  let () = log (Printf.sprintf "number_of_lines %d" (List.length rectangles)) in
  let () = List.iter (fun l ->
    let () = log (Printf.sprintf "line has %d elements" (List.length l)) in
    List.iter (fun x -> log (rect_to_string x)) l)
    rectangles
  in
  let cc x = (x, `Center) in
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
  let () = ctx##fillStyle <- Js.string "rgb(0, 0, 0)" in
  let police_name = "serif" in
  List.iter2 (fun line_rect line_elts ->
    List.iter2 (fun rect (elt, loc) ->
      let size = Text.size_text ctx police_name rect elt in
      let () = log (Printf.sprintf "size: %dpx" size) in
      let text = Text.create ~x_location:loc police_name size elt in
      Text.write_in_rectangle ctx rect text) line_rect line_elts)
    rectangles elts

}}

{server{

let create_canvas_elt height width =
  Html5.D.canvas ~a:[ Html5.D.a_width width; Html5.D.a_height height ]
           [Html5.D.pcdata "your browser doesn't support canvas";
            Html5.D.br ()]

 }}
{client{
let init_client height width canvas_elt =
  let t = create height width canvas_elt in
  let () = create_nor_nork_tabular t in
  ()
}}
{server{
let service unused unused_bis =
  let height, width = 300, 700 in
  let canvas_elt = create_canvas_elt height width in
  let _ = {unit{
    init_client %height %width %canvas_elt
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
