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

type game_argument = {
  argument_description: string;
  argument_label: string;
  non_default_arguments: string list;
  default_argument: string;
}
type arguments = game_argument array

module GameHtmlElements = struct

  type ('div, 'button) help = {
    help_div: ([> Html5_types.div] as 'div) Eliom_content.Html5.D.elt;
    help_button: ([> Html5_types.button] as 'button) Eliom_content.Html5.D.elt;
    out_of_help_button: ([> Html5_types.button] as 'button) Eliom_content.Html5.D.elt;
  }

  type ('div, 'input, 'select, 'button) html_elements = {
    question_board: ([> Html5_types.div] as 'div) Eliom_content.Html5.D.elt;
    answer_input: ([> Html5_types.input ] as 'input) Eliom_content.Html5.D.elt;
    answer_output: ([> Html5_types.div ] as 'div) Eliom_content.Html5.D.elt;
    start_game_div: ([> Html5_types.div ] as 'div) Eliom_content.Html5.D.elt;
    game_ongoing_div: ([> Html5_types.div ] as 'div) Eliom_content.Html5.D.elt;
    result_div: ([> Html5_types.div ] as 'div) Eliom_content.Html5.D.elt;
    nquestions_input: ([> Html5_types.select ] as 'select) Eliom_content.Html5.D.elt;
    start_game_button: ([> Html5_types.button ] as 'button) Eliom_content.Html5.D.elt;
    answer_button: ([> Html5_types.button ] as 'button) Eliom_content.Html5.D.elt;
    restart_game_button: ([> Html5_types.button ] as 'button) Eliom_content.Html5.D.elt;
    help_inputs: ('div, 'button) help option;
    other_inputs: ([> Html5_types.select ] as 'select) Eliom_content.Html5.D.elt list;
  }
end

module type SharedGameConf =
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
    (* the type of the first argument to create t *)
    type create_arg
    (* the second argument of type string array
       is the list of arguments gathered from inputs
       in the same order as declared in arguments.
       these arguments can be used to limit the set
       of generated questions in the game *)
    val create: create_arg -> string array -> t
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

let create_button ?hidden:(h=false) bc btext =
  let bc_to_style bc =
    match bc with
      | `Primary -> "btn-primary"
      | `Danger -> "btn-danger"
      | `Success -> "btn-success"
      | `Info -> "btn-info"
  in
  let css_class = ["btn"; "btn-sm"; (bc_to_style bc)] in
  let css_class =
      if h then "hidden" :: css_class
      else css_class
  in
  button
    ~a:[a_class css_class]
    ~button_type:`Button [pcdata btext]
}}

(* XXX did not find a way to forbid the creation of a
   - a server Module with is_there_bool = True, and
   - a client Module with get_help () = None
*)

{server{
module type ServerGameConf =
sig
  include SharedGameConf
  val is_there_help: bool
end
}}

{client{

(* XXX did not manage to do that with class-es *)
type ('help_t, 'question) _helper = {
  get_help: 'question -> 'help_t * Dom_html.element Js.t list;
  stop_help: 'help_t -> unit;
}

module type ClientGameConf =
sig
  include SharedGameConf
  type help_t
  type helper = (help_t, question) _helper
  val get_help: (unit -> unit) -> helper option
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

module MakeClient (GC:ClientGameConf) =
struct
  type other_inputs = Dom_html.selectElement Js.t array

  type inputs = {
    nb_questions_input: Dom_html.selectElement Js.t;
    start_game: Dom_html.buttonElement Js.t;
    answer: Dom_html.buttonElement Js.t;
    restart_game: Dom_html.buttonElement Js.t;
    other_inputs: other_inputs;
  }

  let create_inputs nb_questions_input start_button ok_button restart_button other_inputs = {
    nb_questions_input = nb_questions_input;
    start_game = start_button;
    answer = ok_button;
    restart_game = restart_button;
    other_inputs = other_inputs;
  }

  let get_input_params inputs =
    let nb_questions = int_of_string (Utils.get_input_text inputs.nb_questions_input) in
    nb_questions, Array.map (fun x -> Utils.get_input_text x) inputs.other_inputs

  let setup_inputs t on_start_game_clicks on_answer_clicks on_restart_game_clicks =
    let open Lwt_js_events in
    async (fun () ->
      clicks t.start_game on_start_game_clicks);
    async (fun () ->
      clicks t.restart_game on_restart_game_clicks);
    async (fun () ->
      clicks t.answer on_answer_clicks)

  type help_inputs = {
    help_div: Dom_html.divElement Js.t;
    help_button: Dom_html.buttonElement Js.t;
    out_of_help_button: Dom_html.buttonElement Js.t;
    helper: GC.helper;
    mutable help_is_asked: bool;
    mutable curr_helper: GC.help_t option;
  }

  let create_help refocus ho =
    match ho with
      | None -> None
      | Some h ->
        Some {help_div = Html5.To_dom.of_div h.GameHtmlElements.help_div;
              help_button = Html5.To_dom.of_button h.GameHtmlElements.help_button;
              out_of_help_button = Html5.To_dom.of_button h.GameHtmlElements.out_of_help_button;
              help_is_asked = false;
              helper = Utils.must (GC.get_help refocus);
              curr_helper = None}

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
    help_inputs: help_inputs option;
    create_arg: GC.create_arg;
  }

  let focus_on_answer answer_input = answer_input##focus()

  let make_on_button_click t f ev arg =
    let () = focus_on_answer t.answer_input in
    f ev arg

  let reset_curr_helper h =
    let () = match h.curr_helper with
      | None -> ()
      | Some curr_h -> h.helper.stop_help curr_h
    in
    h.curr_helper <- None

  let do_hide_helper h =
    let () = reset_curr_helper h in
    let () = Utils.hidde_element h.out_of_help_button in
    let () = h.help_is_asked <- false in
    let () = Utils.show_element h.help_button in
    Html5.Manip.replaceChildren (Html5.Of_dom.of_div h.help_div) []

  let hide_helper h _ _ =
    let () = do_hide_helper h in
    Lwt.return_unit

  let update_help t =
    match t.help_inputs with
      | None -> ()
      | Some h ->
        let () = h.help_is_asked <- true in
        let () = reset_curr_helper h in
        let curr_helper, elements = h.helper.get_help (Utils.must t.current_question) in
        let elements = List.map
          (fun x -> Html5.Of_dom.of_element x)
          (elements)
        in
        let () = h.curr_helper <- Some curr_helper in
        Html5.Manip.replaceChildren (Html5.Of_dom.of_div h.help_div) elements

  let update_help_if_necessary t =
    match t.help_inputs with
      | None -> ()
      | Some h ->
        if h.help_is_asked then update_help t

  let on_help_click h t ev _ =
    let () = Utils.hidde_element h.help_button in
    let () = Utils.show_element h.out_of_help_button in
    let open Lwt_js_events in
    let () = async (fun () ->
      clicks h.out_of_help_button (make_on_button_click t (hide_helper h))) in
    let () = update_help t in
    Lwt.return_unit

  let setup_help t =
    match t.help_inputs with
      | None -> ()
      | Some h ->
        let open Lwt_js_events in
        async (fun () ->
          clicks h.help_button (make_on_button_click t (on_help_click h t)))

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
          let () = focus_on_answer t.answer_input in
          let () = t.current_question <- Some (GC.generate_question g) in
          update_help_if_necessary t
        end
    in
    let () = Utils.write_in_input t.answer_input "" in
    display_current_mode t

  let start_game t =
    let () = Utils.show_element t.game_params.answer in
    let () =
      match t.help_inputs with
        | None -> ()
        | Some h -> Utils.show_element h.help_button
    in
    let nb_questions, others = get_input_params t.game_params in
    let () = t.current_game <- Some (GC.create t.create_arg others) in
    t.current_score <- Some (Score.create nb_questions)

  let display_result t score =
    let () = Html5.Manip.replaceChildren t.result_div [pcdata (Score.to_string score)] in
    let () = Utils.hidde_element t.answer_input in
    let () = Utils.hidde_element t.game_params.answer in
    let () = Html5.Manip.replaceChildren t.question_board [] in
    match t.help_inputs with
      | None -> ()
      | Some h -> begin
        let () = do_hide_helper h in
        let () = Utils.hidde_element h.help_button in
        ()
      end
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

  let handle_answer_input_changes t =
    let message = Utils.get_input_text t.answer_input in
      if message = "" then ()
      else
        if is_finished t then ()
        else begin
          match t.current_game with
            | None -> ()
            | Some game -> handle_answer t
        end

  let on_start_game_clicks t _ _ =
    let () = start_game t in
    let () = next_game t in
    let () = Utils.hidde_element t.start_game_div in
    let () = Utils.show_element t.game_ongoing_div in
    let () = Utils.show_element t.answer_input in
    let () = focus_on_answer t.answer_input in
    let () = Html5.Manip.replaceChildren t.result_div [] in
    Lwt.return_unit

  let reset_game t =
    let () = t.current_game <- None in
    let () = Html5.Manip.replaceChildren t.answer_output [] in
    let () = Utils.write_in_input t.answer_input "" in
    Html5.Manip.replaceChildren t.question_board []

  let on_restart_game_clicks t _ _ =
    let () = reset_game t in
    let () = Utils.hidde_element t.game_ongoing_div in
    let () = Utils.show_element t.start_game_div in
    Lwt.return_unit

  let create question answer_input answer_output game_params start_game_div game_ongoing_div result_div h arg =
    let () = Random.self_init () in
    let answer_input = Html5.To_dom.of_input answer_input in
    let res =
      {
        question_board = question;
        answer_input = answer_input;
        answer_output = answer_output;
        result_div = result_div;
        game_params = game_params;
        current_game = None;
        current_question = None;
        current_score = None;
        start_game_div = Html5.To_dom.of_div start_game_div;
        game_ongoing_div = Html5.To_dom.of_div game_ongoing_div;
        help_inputs = create_help (fun () ->
                                   focus_on_answer answer_input) h;
        create_arg = arg;
      } in
    res

  let on_keyups t ev _ =
    let () =
      if ev##keyCode == 13 then
        handle_answer_input_changes t
      else ()
    in
    Lwt.return_unit

  let setup t =
    let open Lwt_js_events in
    let click_on_answer_handler _ _ =
      let () = handle_answer_input_changes t in
      Lwt.return_unit
    in
    async (fun () ->
      keyups t.answer_input (on_keyups t));
    setup_inputs t.game_params (on_start_game_clicks t) click_on_answer_handler (on_restart_game_clicks t);
    setup_help t

  let create_and_setup
      qb
      answer_input
      answer_output
      start_game_div
      game_ongoing_div
      result_div
      nquestion_input
      start_game_button
      answer_button
      restart_game_button
      help_inputs
      other_inputs
      create_arg =
    let game_mode = create_inputs
      (Html5.To_dom.of_select nquestion_input)
      (Html5.To_dom.of_button start_game_button)
      (Html5.To_dom.of_button answer_button)
      (Html5.To_dom.of_button restart_game_button)
      (Array.map Html5.To_dom.of_select other_inputs) in
    let t = create qb answer_input answer_output game_mode start_game_div game_ongoing_div result_div help_inputs create_arg in
    setup t
end
}}


{server{

module MakeServer (GC:ServerGameConf) = struct
  let game_mode_inputs () =
    let open Utils in
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
    let start_game_button = create_button `Primary "Start" in
    let start_game_div = div [div [pcdata (GC.description ^ ":")];
                              ul (List.map (fun (x, input) -> li [pcdata x; input])
                                    (nquestions :: others));
                              div [start_game_button]] in
    let answer_button = create_button `Danger "Answer" in
    let restart_game_button = create_button `Success "Restart" in
    let answer_output = div [] in
    let result_div = div [] in
    let game_ongoing_l = [question_board; answer_input; answer_output; result_div; answer_button; restart_game_button] in
    let open GameHtmlElements in
    let create_help d b ob = {help_div = d;
                              help_button = b;
                              out_of_help_button = ob}
    in
    let game_ongoing_l, h =
      match GC.is_there_help with
        | false -> game_ongoing_l, None
        | true ->
          let help_div = div ~a:[a_class ["centered"]] [] in
          let help_button = create_button `Primary "Help" in
          let out_of_help_button = create_button ~hidden:true `Primary "I don't need help anymore" in
          (game_ongoing_l @ [help_button; out_of_help_button; help_div],
           Some (create_help help_div help_button out_of_help_button))
    in
    let game_ongoing_div = div ~a:[a_class ["hidden"; "centered"]] game_ongoing_l in

    {question_board = question_board;
     answer_input = answer_input;
     answer_output = answer_output;
     start_game_div = start_game_div;
     game_ongoing_div = game_ongoing_div;
     result_div = result_div;
     nquestions_input = nquestions_input;
     start_game_button = start_game_button;
     answer_button = answer_button;
     restart_game_button = restart_game_button;
     help_inputs = h;
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
