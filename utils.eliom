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

module MakeTranslation (F: sig type t end) = struct
    type data = {
        id: Int32.t;
        source: string;
        source_lang: string;
        dest: string;
        dest_lang: string;
        description: string;
        owner: F.t;
      }

    type correction_data = {
        correction_d: data;
        corrected_id: Int32.t;
        validated: bool;
      }

    type t = {
        content: data;
        correction: correction_data option;
      }
    let get_data_id x = x.id
  end

module Translation = struct
    module M = MakeTranslation(struct type t = Int32.t end)
    include M
    module Int32Set = Set.Make (struct type t = Int32.t
                                       let compare = Pervasives.compare
                                end)
    let get_elt_owners t =
      let owner = [t.content.owner] in
      match t.correction with
        | None -> owner
        | Some x -> x.correction_d.owner :: owner

     let get_distinct_owners l =
       let owners = List.map get_elt_owners l in
       let flattened = List.flatten owners in
       let res = List.fold_left (fun accum x ->
                                 Int32Set.add x accum)
                                Int32Set.empty
                                flattened in
       Int32Set.elements res
  end

module Owner = struct
    type t = {
        username: string;
        preferred_lang_src: string;
        preferred_lang_dst: string;
        id: Int32.t;
      }
    let get_username t = t.username
  end
module TranslationInModel = MakeTranslation(struct
                                               type t = Owner.t
                                               let compare = Pervasives.compare
                                             end)
let convert_data x owner =
  {TranslationInModel.id=x.Translation.id;
   TranslationInModel.source=x.Translation.source;
   TranslationInModel.source_lang=x.Translation.source_lang;
   TranslationInModel.dest=x.Translation.dest;
   TranslationInModel.dest_lang=x.Translation.dest_lang;
   TranslationInModel.description=x.Translation.description;
   TranslationInModel.owner=owner; }

let convert_translations l owners =
  let h = Hashtbl.create (List.length owners) in
  let () = List.iter (fun x -> Hashtbl.add h x.Owner.id x) owners in
  let convert_data_with_h x =
    convert_data x (Hashtbl.find h x.Translation.owner)
  in
  let convert_correction x =
    {TranslationInModel.correction_d=convert_data_with_h
                                       x.Translation.correction_d;
     TranslationInModel.corrected_id=x.Translation.corrected_id;
     TranslationInModel.validated=x.Translation.validated}
  in
  let res = List.map (fun x ->
                      let content = convert_data_with_h x.Translation.content
                      in
                      let correction = match x.Translation.correction with
                        | None -> None
                        | Some x -> Some (convert_correction x) in
                      TranslationInModel.({content; correction})) l in
  let () = Hashtbl.reset h in
  res
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
