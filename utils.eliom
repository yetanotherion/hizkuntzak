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
open Eliom_lib
open Eliom_content
open Html5.D

let must x =
  match x with
    | None -> assert(false)
    | Some x -> x

let max_of_non_empty_list l =
  List.fold_left
    (fun accum elt ->
      Pervasives.max accum elt)
    (List.hd l)
    (List.tl l)

let list_to_select s l =
  let list = List.map (fun name -> Option ([], name, Some (pcdata name), true)) l in
  let head = Option ([], s, Some (pcdata s), true)  in
  head, list

let build_raw_select name default others =
  let h, l = list_to_select default others in
  raw_select ~a:[] ~required:(pcdata "") ~name:name h l

module Translation = struct
    type t = {
        source: string;
        dest: string;
        description: string;
      }
  end

}}

{server{
let create_bootstrap_head () =
  let utf8_meta = meta ~a:[a_charset "utf8"] () in
  let viewport_meta = meta ~a:[a_name "viewport";
                               a_content "width=device-width, initial-scale=1"] () in
  let href_link = uri_of_string (fun () -> "http://maxcdn.bootstrapcdn.com/bootstrap/3.2.0/css/bootstrap.min.css") in
  let stylesheet = link ~rel:[`Stylesheet] ~href:href_link () in
  let make_script x = script ~a:[a_src (uri_of_string (fun () -> x))] (pcdata "") in
  let js_scripts = List.map make_script ["https://ajax.googleapis.com/ajax/libs/jquery/1.11.1/jquery.min.js";
                                         "http://maxcdn.bootstrapcdn.com/bootstrap/3.2.0/js/bootstrap.min.js"] in
  [utf8_meta;
   viewport_meta;
   stylesheet] @ js_scripts

}}

{client{
  let log s = Firebug.console##log(Js.string s)

  let get_input_text element = Js.to_string element##value

  let write_in_input input str =
    input##value <- Js.string str

  let hidde_element elt =
    if not (Js.to_bool (elt##classList##contains(Js.string "hidden")))
    then elt##classList##add(Js.string "hidden")

  let show_element elt =
    if (Js.to_bool (elt##classList##contains(Js.string "hidden")))
    then elt##classList##remove(Js.string "hidden")

  module ReactList = struct
    let list t =
      let open ReactiveData.RList in
      make_from
        (React.S.value t)
        (React.E.map (fun e -> Set e) (React.S.changes t))
  end

  let create_button ?additional_class:(a=[]) t name onclick =
    let b_class =
      match t with
      | `Action -> ["btn"; "btn-primary"; "btn-block"]
      | `ActionLittle -> ["btn"; "btn-primary"; "btn-xs"]
      | `Goto -> ["btn"; "btn-success"; "btn-xs"]
      | `ActionLittleRed -> ["btn"; "btn-danger"; "btn-xs"]
    in
    let b_class = b_class @ a in
    let b = Tyxml_js.Html5.(button ~a:[a_class b_class] [pcdata name]) in
    let () = Lwt_js_events.(async (fun () -> clicks
                                             (Tyxml_js.To_dom.of_button b)
                                             (fun _ _ -> onclick ()))) in
    b

  let create_table header body =
    let open Tyxml_js in
    let header_data = Html5.(List.map (fun x -> th x)) header in
    let table_head = Html5.(thead [tr header_data]) in
    let get_line line_elements = List.map Html5.(fun elt -> td elt) line_elements in
    let body_data = List.map (fun x -> Html5.tr (get_line x)) body in
    let table_body = Html5.([tbody body_data]) in
    Html5.(tablex ~a:[a_class ["table"; "table-striped"]] ~thead:table_head table_body)

  let create_input ?input_type:(it=`Text) ?name_for_placeholder:(n=true) name =
    match n with
    | true -> Tyxml_js.Html5.(input ~a:[a_input_type it; a_class ["form-control"]; a_placeholder name] ())
    | false ->Tyxml_js.Html5.(input ~a:[a_input_type it; a_class ["form-control"]; a_value name] ())

  let input_value i = Js.to_string (Tyxml_js.To_dom.of_input i) ## value
  let select_value s = Js.to_string (Tyxml_js.To_dom.of_select s) ## value
  let create_select elements =
    let str_to_opt str = Tyxml_js.Html5.(option (pcdata str)) in
    Tyxml_js.Html5.select (List.map str_to_opt elements)

  let reorder_list l =
    let rec _reoder_list remaining res =
      match remaining with
      | [] -> res
      | _ ->
         let idx = Random.int (List.length remaining) in
         let element = List.nth remaining idx in
         _reoder_list (List.filter
                         (fun x -> x <> element)
                         remaining)
                      (element :: res)
    in
    _reoder_list l []

  let first_n l nb_element =
    let nb_element = Pervasives.min (List.length l) nb_element in
    let rec _first_n l nb_element res =
      match nb_element with
      | 0 -> List.rev res
      | _ -> _first_n (List.tl l) (nb_element - 1) ((List.hd l) :: res)
    in
    _first_n l nb_element []

}}
