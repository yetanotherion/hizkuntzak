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
end =
struct
  let intransitive_verbs = ["izan"]
  let transitive_verbs = ["ukan"; "ikusi"; "jan"]
  let random_nor_verb () = Games.random_element intransitive_verbs
  let random_nor_nork_verb () = Games.random_element transitive_verbs
end

module NorNorNorkPastPresent = struct
  type genre = [ `Male | `Female ]
  type nork = [ `Nik | `Hik of genre | `Hark | `Guk | `Zuk | `Zuek | `Haiek ]
  type nor = [ `Ni | `Hi | `Hura | `Gu | `Zu | `Zuek | `Haiek ]
  type question = [ `Nor of nor |`NorNork of (nor * nork)] * string * [ `Past | `Present ]
  type t = {
    v_mode: [ `Nor | `NorNork | `All ];
    t_mode: [ `Past | `Present | `All ];
  }
  let title = "nor and nor/nork"
  let description = "The first game consists at conjugating verbs"
  let default_num_of_questions = 5
  let other_number_of_questions = [1; 10; 25; 50; 100]
  let correct_answer_message = "Oso ondo !"
  let bad_answer_prefix = "Zuzenketa: "
  let v_mode_argument =
    let open Games in
    {argument_description = "In which mode would you like to conjugate the verbs?";
     argument_label="mode";
     non_default_arguments = ["nor"; "nor/nork"];
     default_argument = "all"}

  let v_time_argument =
    let open Games in
    {argument_description = "In which time would you like to conjugate the verbs?";
     argument_label="time";
     non_default_arguments = ["past"; "present"];
     default_argument = "all"
    }

  let arguments = Array.of_list [v_mode_argument; v_time_argument]
  let modes = [`Nor; `NorNork]
  let times = [`Past; `Present]
  let nor_modes = [`Ni; `Hi; `Hura; `Gu; `Zu; `Zuek; `Haiek]
  let nork_modes = [`Nik; (`Hik `Male); (`Hik `Female);
                    `Hark; `Guk; `Zuk; `Zuek; `Haiek]

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

  let time_to_string x =
    match x with
      | `Present -> "present"
      | `Past -> "past"

  let v_mode_of_string v =
    match v with
      | "nor" -> `Nor
      | "nor/nork" -> `NorNork
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

  let question_answer current_question =
    let mode, _, time = current_question in
    Tables.conjugate mode time
end

}}

{client{
module NorNorkClient = Games.MakeClient(NorNorNorkPastPresent)

}}
{server{
module NorNorkServer = Games.MakeServer(NorNorNorkPastPresent)

let service unused unused_bis =
  let inputs = NorNorkServer.create_html_elements () in
  let _ = {unit{
    (* we cannot use functors in {unit{ }} sections,
       at least I got a syntax error
       so this work around was implemented *)
    let inputs = %inputs in
    let open Games.GameHtmlElements in
    let other_inputs = Array.of_list inputs.other_inputs in
    NorNorkClient.create_and_setup
      inputs.question_board
      inputs.answer_input
      inputs.answer_output
      inputs.start_game_div
      inputs.game_ongoing_div
      inputs.result_div
      inputs.nquestions_input
      inputs.start_game_button
      inputs.ok_game_button
      inputs.restart_game_button
      other_inputs
  }}
  in
  NorNorkServer.return_page inputs

}}
