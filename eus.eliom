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

module RandomVerbs: sig
  val random_nor_verb: unit -> string
  val random_nor_nork_verb: unit -> string
  val random_nor_nori_verb: unit -> string
end =
struct
  let intransitive_verbs = ["izan"]
  let transitive_verbs = ["ukan"; "ikusi"; "jan"]
  let nor_nori_verbs = ["gustatu"]
  let random_nor_verb () = Games.random_element intransitive_verbs
  let random_nor_nork_verb () = Games.random_element transitive_verbs
  let random_nor_nori_verb () = Games.random_element nor_nori_verbs
end

module IndicativePastPresentShared = struct
  include Eus_aditzak.Questions

  type t = {
    v_mode: [ `Nor | `NorNork | `NorNori | `All ];
    t_mode: [ `Past | `Present | `All ];
  }
  let title = "indicative"
  let description = "The first game consists at conjugating verbs"
  let default_num_of_questions = 5
  let other_number_of_questions = [1; 10; 25; 50; 100]
  let correct_answer_message = "Oso ondo !"
  let bad_answer_prefix = "Zuzenketa: "
  let v_mode_argument =
    let open Games in
    {argument_description = "In which mode would you like to conjugate the verbs?";
     argument_label="mode";
     non_default_arguments = ["nor"; "nor/nori"; "all" ];
     default_argument = "nor/nork"}

  let v_time_argument =
    let open Games in
    {argument_description = "In which time would you like to conjugate the verbs?";
     argument_label="time";
     non_default_arguments = ["past"; "all"];
     default_argument = "present"
    }

  let arguments = Array.of_list [v_mode_argument; v_time_argument]
  let modes = [`Nor; `NorNork]
  let times = [`Past; `Present]
  let nor_modes = [`Ni; `Hi; `Hura; `Gu; `Zu; `Zuek; `Haiek]
  let nork_modes = [`Nik; (`Hik `Male); (`Hik `Female);
                    `Hark; `Guk; `Zuk; `Zuek; `Haiek]

  let nori_modes = [`Niri; (`Hiri `Male); (`Hiri `Female);
                    `Hari; `Guri; `Zuri; `Zuei; `Haiei]

  let nor_to_string x =
    match x with
      | `Ni -> "ni"
      | `Hi -> "hi"
      | `Hura -> "hura"
      | `Gu -> "gu"
      | `Zu -> "zu"
      | `Zuek -> "zuek"
      | `Haiek -> "haiek"

  let nork_to_string x =
    match x with
      | `Nik -> "nik"
      | `Hik p -> begin
        match p with
          | `Male -> "hik (male)"
          | `Female -> "hik (female)"
      end
      | `Hark -> "hark"
      | `Guk -> "guk"
      | `Zuk -> "zuk"
      | `Zuek -> "zuek"
      | `Haiek -> "haiek"

  let nori_to_string x =
    match x with
      | `Niri -> "niri"
      | `Hiri p -> begin
        match p with
          | `Male -> "hiri (male)"
          | `Female -> "hiri (female)"
      end
      | `Hari -> "hari"
      | `Guri -> "guri"
      | `Zuri -> "zuri"
      | `Zuei -> "zuei"
      | `Haiei -> "haiei"

  let time_to_string x =
    match x with
      | `Present -> "present"
      | `Past -> "past"

  let v_mode_of_string v =
    match v with
      | "nor" -> `Nor
      | "nor/nork" -> `NorNork
      | "nor/nori" -> `NorNori
      | "all" -> `All
      | _ -> assert(false)

  let t_mode_of_string v =
    match v with
      | "present" -> `Present
      | "past" -> `Past
      | "all" -> `All
      | _ -> assert(false)

  let create arg =
    let v_mode, t_mode = arg.(0), arg.(1) in
    {
      v_mode = v_mode_of_string v_mode;
      t_mode = t_mode_of_string t_mode;
    }

  let generate_question t =
    let open Games in
    let valid_modes =
      match t.v_mode with
        | `Nor -> [`Nor]
        | `NorNork -> [`NorNork]
        | `NorNori -> [`NorNori]
        | `All -> modes
    in
    let valid_times =
      match t.t_mode with
        | `Present -> [`Present]
        | `Past -> [`Past]
        | `All -> times
    in
    let v_mode, time = random_element valid_modes, random_element valid_times in
    match v_mode with
      | `Nor -> (`Nor (random_element nor_modes), RandomVerbs.random_nor_verb (), time)
      | `NorNork -> (`NorNork (random_element nor_modes,
                               random_element nork_modes),
                     RandomVerbs.random_nor_nork_verb (),
                     time)
      | `NorNori -> (`NorNori (random_element nor_modes,
                               random_element nori_modes),
                     RandomVerbs.random_nor_nori_verb (),
                     time)

  let question_to_string current_question answero =
    let ps = Printf.sprintf in
    let answer =
      match answero with
        | None -> "..."
        | Some x -> x
    in
    match current_question with
      | (`Nor nor, v, time) -> ps "%s %s %s (%s)" (nor_to_string nor) v answer (time_to_string time)
      | (`NorNork (nor, nork), v, time) -> ps "%s %s %s %s (%s)" (nork_to_string nork) (nor_to_string nor) v answer (time_to_string time)
      | (`NorNori (nor, nori), v, time) -> ps "%s %s %s %s (%s)" (nor_to_string nor) (nori_to_string nori) v answer (time_to_string time)

  let question_answer current_question =
    let mode, _, time = current_question in
    Tables.conjugate mode time
end

}}


{client{
module IndicativePastPresentClient = struct
  include IndicativePastPresentShared

  module E = Eus_aditzak
  type help_t = E.animation option
  type helper = (help_t, question) Games._helper

  let start_animation t norNork =
    Lwt.async (fun () -> E.start_animation t norNork)

  let stop_animation animo =
    match animo with
      | None -> ()
      | Some t -> E.stop_animation t

  let create_help_button refocus_after_click t norNork =
    let play_help = Games.create_button `Info "Show me how it works" in
    let help_dom = Html5.To_dom.of_button play_help in
    let on_play_help _ _ =
      let () = refocus_after_click () in
      let () = start_animation t norNork in
      Lwt.return_unit
    in
    let open Lwt_js_events in
    let () = async (fun () ->
      clicks help_dom on_play_help) in
    play_help

  let get_animation refocus_after_click question =
    let mode, all_question, time = (question: question) in
    let help_no_available = pcdata "Sorry that question does not have help for now" in
    let not_available = None, [Html5.To_dom.of_element help_no_available] in
    match mode with
      | `Nor _ -> not_available
      | `NorNori _ -> not_available
      | `NorNork norNork -> match time with
          | `Past -> not_available
          | `Present ->
            let height, width = 300, 700 in
            let canvas = E.create_canvas_elt 300 700 in
            let t = E.create_animation height width canvas in
            let play_help = create_help_button refocus_after_click t norNork in
            let elts = [canvas; play_help] in
            let trs = List.map (fun elt -> tr [td [elt]]) elts in
            let anim_elt = tablex ~a:[a_class ["centered"]] ~thead:(thead []) [tbody trs] in
            (Some t, [Html5.To_dom.of_element anim_elt])

  let get_help refocus_after_click =
    Some {
      Games.get_help = (get_animation refocus_after_click);
      Games.stop_help = stop_animation;
    }
end

module IndicativeClient = Games.MakeClient(IndicativePastPresentClient)
}}

{server{
module IndicativePastPresentServer = struct
  include IndicativePastPresentShared
  let is_there_help = true
end
module IndicativeServer = Games.MakeServer(IndicativePastPresentServer)

let service unused unused_bis =
  let inputs = IndicativeServer.create_html_elements () in
  let _ = {unit{
    (* we cannot use functors in {unit{ }} sections,
       at least I got a syntax error
       so this work around was implemented *)
    let inputs = %inputs in
    let open Games.GameHtmlElements in
    let other_inputs = Array.of_list inputs.other_inputs in
    IndicativeClient.create_and_setup
      inputs.question_board
      inputs.answer_input
      inputs.answer_output
      inputs.start_game_div
      inputs.game_ongoing_div
      inputs.result_div
      inputs.nquestions_input
      inputs.start_game_button
      inputs.answer_button
      inputs.restart_game_button
      inputs.help_inputs
      other_inputs
  }}
  in
  IndicativeServer.return_page inputs

}}
