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
type level = [`Hard | `Normal]

let level_to_str x =
  match x with
    | `Hard -> "hard"
    | `Normal -> "normal"

let str_to_level x =
  match x with
  | "hard" -> `Hard
  | "normal" -> `Normal
  | _ -> assert(false)
}}

{client{
open Eliom_lib
open Eliom_content
open Tyxml_js

let resetChildren dom_element =
  List.iter (fun n -> ignore (dom_element##removeChild((n:> Dom.node Js.t))))
            (Dom.list_of_nodeList (dom_element##childNodes))

let replaceChildren dom_element dom_elements =
  let () = resetChildren dom_element in
  let rec _replaceChildren dom_element dom_elements =
    match dom_elements with
    | hd :: tl -> begin
        let () = Dom.appendChild dom_element hd in
        _replaceChildren dom_element tl
      end
    | [] -> ()
  in
  _replaceChildren dom_element dom_elements

type game_argument = {
  argument_description: string;
  argument_label: string;
  non_default_arguments: string list;
  default_argument: string;
}
type arguments = game_argument array

module GameHtmlElements = struct

  type ('div, 'button) help = {
    help_div: ([> Html5_types.div] as 'div) Html5.elt;
    help_button: ([> Html5_types.button] as 'button) Html5.elt;
    out_of_help_button: ([> Html5_types.button] as 'button) Html5.elt;
  }

  type ('div, 'input, 'select, 'button) html_elements = {
    question_board: ([> Html5_types.div] as 'div) Html5.elt;
    answer_board: ([> Html5_types.div] as 'div) Html5.elt;
    answer_output: ([> Html5_types.div ] as 'div) Html5.elt;
    start_game_div: ([> Html5_types.div ] as 'div) Html5.elt;
    game_ongoing_div: ([> Html5_types.div ] as 'div) Html5.elt;
    result_div: ([> Html5_types.div ] as 'div) Html5.elt;
    nquestions_input: ([> Html5_types.select ] as 'select) Html5.elt;
    level_input: ([> Html5_types.select ] as 'select) Html5.elt;
    start_game_button: ([> Html5_types.button ] as 'button) Html5.elt;
    restart_game_button: ([> Html5_types.button ] as 'button) Html5.elt;
    help_inputs: ('div, 'button) help option;
    other_inputs: ([> Html5_types.select ] as 'select) Html5.elt list;
  }
end

module type GameConf =
  sig
    type t
    type question
    val description: string
    val default_num_of_questions: int
    val other_number_of_questions: int list
    val correct_answer_message: string
    val bad_answer_prefix: string
    val arguments: arguments
    val supported_levels: level list
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
       computed by question_answer.
       This is used to compute either:
       - the question, or
       - the answer that will be displayed
         below the next question.
    *)
    val question_to_string: question -> string option -> string
    val question_answer: question -> string
    val question_additional_answers: t -> question -> string list
    type help_t
    val get_help: (unit -> unit) ->
                  question -> help_t * Dom_html.element Js.t list
    val stop_help: help_t -> unit
    val is_there_help: bool
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
  Html5.(button
           ~a:[a_class css_class] [pcdata btext])

module HardAnswers = struct
    open Html5
    type t = {
        a_input: Dom_html.inputElement Js.t;
        a_button: Dom_html.buttonElement Js.t;
        mutable setup_done: bool;
      }

    let focus_on_answer t = t.a_input ## focus()

    let create () =
      let a_input = To_dom.of_input (input ~a:[a_input_type `Text;
                                               a_autofocus `Autofocus] ()) in
      let a_button = To_dom.of_button (create_button `Danger "Answer") in
      let setup_done = false in
      {a_input; a_button; setup_done}

    let reset_input t =
      Utils.write_in_input t.a_input ""

    let handle_answer_input t f =
      let proposed_answer = Utils.get_input_text t.a_input in
      let () = f proposed_answer in
      Lwt.return_unit

    let on_keyups t f ev _ =
      if ev##keyCode == 13 then
        handle_answer_input t f
      else Lwt.return_unit

    let on_answer_clicks t f _ _ =
      handle_answer_input t f

    let get_ui_elements t =
      (* we coerce all elements to Htm5.elt *)
      List.map To_dom.of_element
               [Of_dom.of_input t.a_input;
                Of_dom.of_button t.a_button]

    let setup t f =
      if not t.setup_done then
        let open Lwt_js_events in
        async (fun () ->
               keyups t.a_input (on_keyups t f));
        async (fun () ->
               clicks t.a_button (on_answer_clicks t f));
        t.setup_done <- true;

  end

module NormalAnswers = struct
    open Html5
    type t = (Dom_html.buttonElement Js.t list) ref

    let create () = ref []

    let focus_on_answer _ = ()
    let reset_input _ = ()

    let get_ui_elements t =
      (* we coerce all elements to Htm5.elt *)
      List.map To_dom.of_element
               (List.map
                  Of_dom.of_button
                  !t)

    let on_button_click f x _ _ =
      let () = f x in
      Lwt.return_unit

    let setup t f answer additional_answers =
      let buttons = List.map
                      (fun x -> x,
                                (To_dom.of_button
                                   (create_button `Primary x)))
                      (Utils.reorder_list (answer :: additional_answers))
      in
      let open Lwt_js_events in
      let () =
        List.iter
          (fun (x, b) ->
           async (fun () ->
                  clicks b (on_button_click f x)))
          buttons
      in
      t := List.map Pervasives.snd buttons

  end

type answer_handler = [`Normal of NormalAnswers.t
                      | `Hard of HardAnswers.t]


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

module Make (GC:GameConf) =
struct
  type other_inputs = Dom_html.selectElement Js.t array

  type inputs = {
    nb_questions_input: Dom_html.selectElement Js.t;
    level_input: Dom_html.selectElement Js.t;
    start_game: Dom_html.buttonElement Js.t;
    restart_game: Dom_html.buttonElement Js.t;
    other_inputs: other_inputs;
  }

  let create_inputs nb_questions_input level_input start_button
                    restart_button other_inputs = {
    nb_questions_input = nb_questions_input;
    level_input = level_input;
    start_game = start_button;
    restart_game = restart_button;
    other_inputs = other_inputs;
  }

  let get_input_params inputs =
    let nb_questions = int_of_string (Utils.get_input_text
                                        inputs.nb_questions_input) in
    let level = (Utils.get_input_text
                   inputs.level_input) in
    nb_questions,
    (str_to_level level),
    Array.map (fun x ->
               Utils.get_input_text x) inputs.other_inputs

  let setup_inputs t on_start_game_clicks
                   on_restart_game_clicks =
    let open Lwt_js_events in
    async (fun () ->
      clicks t.start_game on_start_game_clicks);
    async (fun () ->
      clicks t.restart_game on_restart_game_clicks);

  type help_inputs = {
    help_div: Dom_html.divElement Js.t;
    help_button: Dom_html.buttonElement Js.t;
    out_of_help_button: Dom_html.buttonElement Js.t;
    mutable help_is_asked: bool;
    mutable curr_helper: GC.help_t option;
  }

  let create_help ho =
    match ho with
      | None -> None
      | Some h ->
         Some {help_div = To_dom.of_div h.GameHtmlElements.help_div;
               help_button = To_dom.of_button h.GameHtmlElements.help_button;
               out_of_help_button = To_dom.of_button
                                      h.GameHtmlElements.out_of_help_button;
               help_is_asked = false;
               curr_helper = None}

  type 'a t = {
    question_board: 'a Html5.elt;
    answer_board: 'a Html5.elt;
    answer_output: 'a Html5.elt;
    result_div: 'a Html5.elt;
    game_params: inputs;
    mutable current_game: GC.t option;
    mutable current_question: GC.question option;
    mutable current_score: Score.t option;
    mutable answer_handler: answer_handler option;
    start_game_div: Dom_html.divElement Js.t;
    game_ongoing_div: Dom_html.divElement Js.t;
    help_inputs: help_inputs option;
    create_arg: GC.create_arg;
  }

  let focus_on_answer t =
    match t.answer_handler with
    | None -> ()
    | Some x ->
       match x with
       | `Hard ah -> HardAnswers.focus_on_answer ah
       | `Normal ah -> NormalAnswers.focus_on_answer ah

  let reset_answer t =
    let () =
      match t.answer_handler with
      | None -> ()
      | Some x ->
         match x with
         | `Hard ah -> HardAnswers.reset_input ah
         | `Normal ah -> NormalAnswers.reset_input ah
    in
    focus_on_answer t

  let make_on_button_click t f ev arg =
    let () = focus_on_answer t in
    f ev arg


  let reset_curr_helper h =
    let () = match h.curr_helper with
      | None -> ()
      | Some curr_h -> GC.stop_help curr_h
    in
    h.curr_helper <- None

  let do_hide_helper h =
    let () = reset_curr_helper h in
    let () = Utils.hidde_element h.out_of_help_button in
    let () = h.help_is_asked <- false in
    let () = Utils.show_element h.help_button in
    resetChildren h.help_div

  let hide_helper h _ _ =
    let () = do_hide_helper h in
    Lwt.return_unit

  let update_help t =
    match t.help_inputs with
      | None -> ()
      | Some h ->
        let () = h.help_is_asked <- true in
        let () = reset_curr_helper h in
        let curr_helper, elements = GC.get_help
                                      (fun () -> focus_on_answer  t)
                                      (Utils.must t.current_question) in
        let () = h.curr_helper <- Some curr_helper in
        replaceChildren h.help_div elements

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
    replaceChildren (To_dom.of_div t.question_board)
                    [To_dom.of_pcdata
                       (Html5.pcdata (current_question_to_str t))]

  let display_result t score =
    let () = replaceChildren (To_dom.of_div t.result_div)
                             [To_dom.of_pcdata
                                (Html5.pcdata (Score.to_string score))] in
    let () = resetChildren (To_dom.of_div t.question_board) in
    let () = resetChildren (To_dom.of_div t.answer_board) in
    match t.help_inputs with
      | None -> ()
      | Some h -> begin
          let () = do_hide_helper h in
          let () = Utils.hidde_element h.help_button in
          ()
        end

  (* XXX the need for mutually reccursive functions
     might disappear using react *)
  let rec
      next_game t =
        let () =
          match t.current_game with
            | None -> ()
            | Some g -> begin
              let current_question = GC.generate_question g in
              let additional_answers = GC.question_additional_answers
                                         g
                                         current_question in
              let () =
                t.current_question <- Some current_question in
              let answer = GC.question_answer
                                     current_question in
              let () = setup_question t
                                      answer
                                      additional_answers
              in
              update_help_if_necessary t
            end
        in
        let () = reset_answer t in
        display_current_mode t
    and
      setup_question t answer additional_answers =
        match t.answer_handler with
        | None -> ()
        | Some aht ->
           let ui_elements =
             match aht with
             | `Hard ah -> HardAnswers.(
                 let () = setup ah (handle_answer t) in
                 get_ui_elements ah)
             | `Normal ah ->
                NormalAnswers.(
                 let () = setup ah (handle_answer t)
                                answer
                                additional_answers
                 in
                 get_ui_elements ah)
           in
           replaceChildren (To_dom.of_div t.answer_board)
                           ui_elements

    and
      handle_answer t proposed_answer =
        let (question, score) =
          match (t.current_question, t.current_score) with
            | (Some x, Some y) -> (x, y)
            | _ -> assert(false)
        in
        let answer = GC.question_answer question in
        let current_message =
          if proposed_answer = answer then (
            Score.good_answer score; GC.correct_answer_message)
          else begin
            let () = Score.bad_answer score in
            GC.bad_answer_prefix ^
              (current_question_to_str t ~answer:(Some answer))
          end
        in
        let output_message = [current_message] in
        let () = replaceChildren (To_dom.of_div t.answer_output)
                                 (List.map (fun x ->
                                            To_dom.of_pcdata
                                              (Html5.pcdata x))
                                           output_message) in
        if is_finished t then display_result t score
        else next_game t

  let start_game t =
    let () =
      match t.help_inputs with
        | None -> ()
        | Some h -> Utils.show_element h.help_button
    in
    let nb_questions, level, others = get_input_params t.game_params in
    let () = t.current_game <- Some (GC.create t.create_arg others) in
    let () = t.current_score <- Some (Score.create nb_questions) in
    let answer_handler =
      match level with
      | `Hard -> `Hard (HardAnswers.create ())
      | `Normal -> `Normal (NormalAnswers.create ())
    in
    t.answer_handler <- Some answer_handler


  let on_start_game_clicks t _ _ =
    let () = start_game t in
    let () = next_game t in
    let () = Utils.hidde_element t.start_game_div in
    let () = Utils.show_element t.game_ongoing_div in
    let () = resetChildren (To_dom.of_div t.result_div) in
    Lwt.return_unit

  let reset_game t =
    let () = t.current_game <- None in
    let () = resetChildren (To_dom.of_div t.answer_output) in
    let () = resetChildren (To_dom.of_div t.question_board) in
    resetChildren (To_dom.of_div t.answer_board)

  let on_restart_game_clicks t _ _ =
    let () = reset_game t in
    let () = Utils.hidde_element t.game_ongoing_div in
    let () = Utils.show_element t.start_game_div in
    Lwt.return_unit

  let create question answer_board answer_output game_params
             start_game_div game_ongoing_div result_div h arg =
    let () = Random.self_init () in
    let res =
      {
        question_board = question;
        answer_board = answer_board;
        answer_output = answer_output;
        result_div = result_div;
        game_params = game_params;
        current_game = None;
        current_question = None;
        current_score = None;
        answer_handler = None;
        start_game_div = To_dom.of_div start_game_div;
        game_ongoing_div = To_dom.of_div game_ongoing_div;
        help_inputs = create_help h;
        create_arg = arg;
      } in
    res


  let setup t =
    let open Lwt_js_events in
    setup_inputs t.game_params
                 (on_start_game_clicks t)
                 (on_restart_game_clicks t);
    setup_help t

  let game_mode_inputs () =
    let its i_arg = Printf.sprintf "%d" i_arg in
    let select_args = (its GC.default_num_of_questions) ::
                        (List.map its GC.other_number_of_questions) in
    let n_inputs =
      "How many questions would you like in that game ?",
      Utils.create_select select_args
    in
    let levels = List.map level_to_str GC.supported_levels in
    let game_level =
      "On which level would you like to play ?",
      Utils.create_select levels
    in
    let others =
      Array.map (fun x ->
                 let arguments = x.default_argument ::
                                   x.non_default_arguments in
                 x.argument_description,
                 Utils.create_select arguments)
                GC.arguments
    in
    n_inputs, game_level, Array.to_list others

  let create_html_elements () =
    let open Tyxml_js in
    let open Html5 in
    let question_board = div [] in
    let answer_board = div [] in
    let nquestions, level, others = game_mode_inputs () in
    let nquestions_input = Pervasives.snd nquestions in
    let level_input = Pervasives.snd level in
    let other_inputs = List.map Pervasives.snd others in
    let start_game_button = create_button `Primary "Start" in
    let start_game_div = div [div [pcdata (GC.description ^ ":")];
                              ul (List.map
                                    (fun (x, input) -> li [pcdata x; input])
                                           (nquestions :: (level :: others)));
                              div [start_game_button]] in
    let restart_game_button = create_button `Success "Restart" in
    let answer_output = div [] in
    let result_div = div [] in
    let game_ongoing_l = [question_board;
                          answer_board;
                          answer_output;
                          result_div;
                          restart_game_button] in
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
          let out_of_help_button = create_button ~hidden:true
                                                 `Primary
                                                 "I don't need help anymore" in
          (game_ongoing_l @ [help_button; out_of_help_button; help_div],
           Some (create_help help_div help_button out_of_help_button))
    in
    let game_ongoing_div = div ~a:[a_class ["hidden";
                                            "centered"]] game_ongoing_l in

    {question_board = question_board;
     answer_board = answer_board;
     answer_output = answer_output;
     start_game_div = start_game_div;
     game_ongoing_div = game_ongoing_div;
     result_div = result_div;
     nquestions_input = nquestions_input;
     level_input = level_input;
     start_game_button = start_game_button;
     restart_game_button = restart_game_button;
     help_inputs = h;
     other_inputs = other_inputs}

  let _create_and_setup
      qb
      answer_board
      answer_output
      start_game_div
      game_ongoing_div
      result_div
      nquestion_input
      level_input
      start_game_button
      restart_game_button
      help_inputs
      other_inputs
      create_arg =
    let game_mode = create_inputs
                      (To_dom.of_select nquestion_input)
                      (To_dom.of_select level_input)
                      (To_dom.of_button start_game_button)
                      (To_dom.of_button restart_game_button)
                      (Array.map To_dom.of_select other_inputs) in
    let t = create qb answer_board answer_output game_mode
                   start_game_div
                   game_ongoing_div
                   result_div
                   help_inputs
                   create_arg in
    setup t

  let create_and_setup elt arg =
    let inputs = create_html_elements () in
    let open GameHtmlElements in
    let other_inputs = Array.of_list inputs.other_inputs in
    let () = _create_and_setup
               inputs.question_board
               inputs.answer_board
               inputs.answer_output
               inputs.start_game_div
               inputs.game_ongoing_div
               inputs.result_div
               inputs.nquestions_input
               inputs.level_input
               inputs.start_game_button
               inputs.restart_game_button
               inputs.help_inputs
               other_inputs
               arg in
    replaceChildren elt (List.map To_dom.of_div [inputs.start_game_div;
                                                 inputs.game_ongoing_div])
end
}}


{server{

  let return_page title =
    let div_elt = Eliom_content.Html5.F.(div ~a:[a_id "main"] []) in
    let otherh = Utils.create_bootstrap_head () in
    let b = Eliom_content.Html5.F.(body [div_elt]) in
    let res = Eliom_tools.F.html
                ~title:title
                ~css:[["css";"hizkuntzak.css"]]
                ~other_head:otherh
                b in
    Lwt.return res
}}
