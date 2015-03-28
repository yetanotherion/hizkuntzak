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
}}

{shared{

type game_argument = {
  argument_description: string;
  argument_label: string;
  non_default_arguments: string list;
  default_argument: string;
}

type arguments = game_argument array

module type GameConf =
  sig
    type t
    type question
    val title: string
    val description: string
    val default_num_of_questions: int
    val other_number_of_questions: int list
    val correct_answer_message: string
    val bad_answer_prefix: string
    val arguments: arguments
    (* the first argument of type string array
       is the list of arguments gathered from inputs
       in the same order as declared in arguments.
       these arguments can be used to limit the set
       of generated questions in the game *)
    val create: string array -> t
    val generate_question: t -> question
    (* the second argument of type string option
       is the optional answer of the question, already
       computed by by question_answer.
       This is used to compute either:
       - the question, or
       - the answer that will be displayed
         below the next question.
    *)
    val question_to_string: question -> string option -> string
    val question_answer: question -> string
  end

let random_element l = List.nth l (Random.int (List.length l))

module GameHtmlElements = struct
  type ('div, 'input, 'select, 'button) html_elements = {
    question_board: ([> Html5_types.div] as 'div) Eliom_content.Html5.D.elt;
    answer_input: ([> Html5_types.input ] as 'input) Eliom_content.Html5.D.elt;
    answer_output: ([> Html5_types.div ] as 'div) Eliom_content.Html5.D.elt;
    start_game_div: ([> Html5_types.div ] as 'div) Eliom_content.Html5.D.elt;
    game_ongoing_div: ([> Html5_types.div ] as 'div) Eliom_content.Html5.D.elt;
    result_div: ([> Html5_types.div ] as 'div) Eliom_content.Html5.D.elt;
    nquestions_input: ([> Html5_types.select ] as 'select) Eliom_content.Html5.D.elt;
    start_game_button: ([> Html5_types.button ] as 'button) Eliom_content.Html5.D.elt;
    ok_game_button: ([> Html5_types.button ] as 'button) Eliom_content.Html5.D.elt;
    restart_game_button: ([> Html5_types.button ] as 'button) Eliom_content.Html5.D.elt;
    other_inputs: ([> Html5_types.select ] as 'select) Eliom_content.Html5.D.elt list;
  }
end
}}

{client{

module Score = struct
  type t = {
    start_time: float;
    mutable nber_ok: int;
    mutable remaining_question: int;
    nber_of_questions: int;
  }

  let create nber_of_questions = {
    start_time = Unix.gettimeofday ();
    nber_ok = 0;
    remaining_question = nber_of_questions;
    nber_of_questions = nber_of_questions;
  }

  let good_answer t =
    t.remaining_question <- t.remaining_question - 1;
    t.nber_ok <- t.nber_ok + 1

  let bad_answer t =
    t.remaining_question <- t.remaining_question - 1

  let to_string t =
    let stop_date = Unix.gettimeofday () in
    let f = stop_date -. t.start_time in
    Printf.sprintf "%d/%d on %.2f seconds" t.nber_ok t.nber_of_questions f
  let finished t =  t.remaining_question = 0
end

module MakeClient (GC:GameConf) =
struct
  type other_inputs = Dom_html.selectElement Js.t array
  type inputs = {
    nb_questions_input: Dom_html.selectElement Js.t;
    start_game: Dom_html.buttonElement Js.t;
    ok_game: Dom_html.buttonElement Js.t;
    restart_game: Dom_html.buttonElement Js.t;
    other_inputs: other_inputs;
  }

  let create_inputs nb_questions_input start_button ok_button restart_button other_inputs = {
    nb_questions_input = nb_questions_input;
    start_game = start_button;
    ok_game = ok_button;
    restart_game = restart_button;
    other_inputs = other_inputs;
  }

  let get_input_params inputs =
    let nb_questions = int_of_string (Utils.get_input_text inputs.nb_questions_input) in
    nb_questions, Array.map (fun x -> Utils.get_input_text x) inputs.other_inputs

  let setup_inputs t on_start_game_clicks on_answer_input_changes on_restart_game_clicks =
    let open Lwt_js_events in
    async (fun () ->
      clicks t.start_game on_start_game_clicks);
    async (fun () ->
      clicks t.restart_game on_restart_game_clicks);
    async (fun () ->
      clicks t.ok_game on_answer_input_changes)

  type 'a t = {
    question_board: 'a Eliom_content.Html5.elt;
    answer_input: Dom_html.inputElement Js.t;
    answer_output: 'a Eliom_content.Html5.elt;
    result_div: 'a Eliom_content.Html5.elt;
    game_params: inputs;
    mutable current_game: GC.t option;
    mutable current_question: GC.question option;
    mutable current_score: Score.t option;
    start_game_div: Dom_html.divElement Js.t;
    game_ongoing_div: Dom_html.divElement Js.t;
  }

  let is_finished t =
    match t.current_score with
      | None -> false
      | Some x -> Score.finished x

  let current_question_to_str ?answer:(a=None) t =
    match t.current_question with
      | None -> ""
      | Some current_question ->
        GC.question_to_string current_question a

  let display_current_mode t =
    Html5.Manip.replaceChildren t.question_board [pcdata (current_question_to_str t)]

  let next_game t =
    let () =
      match t.current_game with
        | None -> ()
        | Some g -> begin
          let () = t.answer_input##focus() in
          t.current_question <- Some (GC.generate_question g)
        end
    in
    let () = t.answer_input##value <- Js.string "" in
    display_current_mode t

  let start_game t =
    let () = Utils.show_element t.game_params.ok_game in
    let nb_questions, others = get_input_params t.game_params in
    let () = t.current_game <- Some (GC.create others) in
    t.current_score <- Some (Score.create nb_questions)

  let display_result t score =
    let () = Html5.Manip.replaceChildren t.result_div [pcdata (Score.to_string score)] in
    let () = Utils.hidde_element t.answer_input in
    let () = Utils.hidde_element t.game_params.ok_game in
    Html5.Manip.replaceChildren t.question_board []

  let handle_answer t =
    let (question, score) =
      match (t.current_question, t.current_score) with
        | (Some x, Some y) -> (x, y)
        | _ -> assert(false)
    in
    let answer = GC.question_answer question in
    let message = Utils.get_input_text t.answer_input in
    let current_message =
      if message = answer then (Score.good_answer score; GC.correct_answer_message)
      else begin
        let () = Score.bad_answer score in
        GC.bad_answer_prefix ^ (current_question_to_str t ~answer:(Some answer))
      end
    in
    let output_message = [current_message] in
    let () = Html5.Manip.replaceChildren t.answer_output (List.map (fun x -> pcdata x) output_message) in
    if is_finished t then display_result t score
    else next_game t

  let on_answer_input_changes t _ _ =
    let message = Utils.get_input_text t.answer_input in
    let () =
      if message = "" then ()
      else
        if is_finished t then ()
        else begin
          match t.current_game with
            | None -> ()
            | Some game -> handle_answer t
        end
    in
    Lwt.return_unit

  let on_start_game_clicks t _ _ =
    let () = start_game t in
    let () = next_game t in
    let () = Utils.hidde_element t.start_game_div in
    let () = Utils.show_element t.game_ongoing_div in
    let () = Utils.show_element t.answer_input in
    let () = Html5.Manip.replaceChildren t.result_div [] in
    Lwt.return_unit

  let reset_game t =
    let () = t.current_game <- None in
    let () = Html5.Manip.replaceChildren t.answer_output [] in
    let () = t.answer_input##value <- Js.string "" in
    Html5.Manip.replaceChildren t.question_board []

  let on_restart_game_clicks t _ _ =
    let () = reset_game t in
    let () = Utils.hidde_element t.game_ongoing_div in
    let () = Utils.show_element t.start_game_div in
    Lwt.return_unit

  let create question answer_input answer_output game_params start_game_div game_ongoing_div result_div =
    let () = Random.self_init () in
    let res =
      {
        question_board = question;
        answer_input = Html5.To_dom.of_input answer_input;
        answer_output = answer_output;
        result_div = result_div;
        game_params = game_params;
        current_game = None;
        current_question = None;
        current_score = None;
        start_game_div = Html5.To_dom.of_div start_game_div;
        game_ongoing_div = Html5.To_dom.of_div game_ongoing_div;
      } in
    res

  let setup t =
    let open Lwt_js_events in
    async (fun () ->
      changes t.answer_input (on_answer_input_changes t));
    setup_inputs t.game_params (on_start_game_clicks t) (on_answer_input_changes t) (on_restart_game_clicks t)

  let create_and_setup
      qb
      answer_input
      answer_output
      start_game_div
      game_ongoing_div
      result_div
      nquestion_input
      start_game_button
      ok_game_button
      restart_game_button
      other_inputs =
    let game_mode = create_inputs
      (Html5.To_dom.of_select nquestion_input)
      (Html5.To_dom.of_button start_game_button)
      (Html5.To_dom.of_button ok_game_button)
      (Html5.To_dom.of_button restart_game_button)
      (Array.map Html5.To_dom.of_select other_inputs) in
    let t = create qb answer_input answer_output game_mode start_game_div game_ongoing_div result_div in
    setup t
end
}}


{server{

module MakeServer (GC:GameConf) = struct
  let list_to_select s l =
    let list = List.map (fun name -> Option ([], name, Some (pcdata name), true)) l in
    let head = Option ([], s, Some (pcdata s), true)  in
    head, list

  let game_mode_inputs () =
    let build_raw_select name default others =
      let h, l = list_to_select default others in
      raw_select ~a:[] ~required:(pcdata "") ~name:name h l
    in
    let its i = Printf.sprintf "%d" i in
    let n_inputs =
      "How many questions would you like in that game ?",
      build_raw_select "number of questions"
        (its GC.default_num_of_questions)
        (List.map its GC.other_number_of_questions)
    in
    let others =
      Array.map (fun x ->
        x.argument_description,
        build_raw_select x.argument_label x.default_argument x.non_default_arguments)
        GC.arguments
    in
    n_inputs, Array.to_list others

  let create_html_elements () =
    let question_board = div [] in
    let answer_input = string_input ~input_type:`Text () in
    let nquestions, others = game_mode_inputs () in
    let nquestions_input = Pervasives.snd nquestions in
    let other_inputs = List.map Pervasives.snd others in
    let start_game_button = button ~a:[a_class ["btn"; "btn-sm"; "btn-primary"]] ~button_type:`Button [pcdata "Start"] in
    let start_game_div = div [div [pcdata (GC.description ^ ":")];
                              ul (List.map (fun (x, input) -> li [pcdata x; input])
                                    (nquestions :: others));
                              div [start_game_button]] in
    let ok_game_button = button ~a:[a_class ["btn"; "btn-sm"; "btn-danger"]] ~button_type:`Button [pcdata "Answer"] in
    let restart_game_button = button ~a:[a_class ["btn"; "btn-sm"; "btn-success"]] ~button_type:`Button [pcdata "Restart"] in
    let answer_output = div [] in
    let result_div = div [] in
    let game_ongoing_div = div ~a:[a_class ["hidden"]] [question_board; answer_input; answer_output; result_div; ok_game_button; restart_game_button] in
    let open GameHtmlElements in
    {question_board = question_board;
     answer_input = answer_input;
     answer_output = answer_output;
     start_game_div = start_game_div;
     game_ongoing_div = game_ongoing_div;
     result_div = result_div;
     nquestions_input = nquestions_input;
     start_game_button = start_game_button;
     ok_game_button = ok_game_button;
     restart_game_button = restart_game_button;
     other_inputs = other_inputs}

  let return_page inputs =
    let open GameHtmlElements in
    let divs = div [inputs.start_game_div; inputs.game_ongoing_div;] in
    let otherh = Utils.create_bootstrap_head () in
    let b = Html5.F.(body [divs]) in
    let res = Eliom_tools.F.html ~title:GC.title ~css:[["css";"hizkuntzak.css"]]
      ~other_head:otherh b in
    Lwt.return res
end
}}
