{shared{
open Eliom_lib
open Eliom_content
open Html5.D
}}

{client{


let random_element l = List.nth l (Random.int (List.length l))

module RandomVerbs: sig
  val random_nor_verb: unit -> string
  val random_nor_nork_verb: unit -> string
end =
struct
  let intransitive_verbs = ["izan"]
  let transitive_verbs = ["ukan"; "ikusi"; "jan"]
  let random_nor_verb () = random_element intransitive_verbs
  let random_nor_nork_verb () = random_element transitive_verbs
end

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
end

module GenerateQuestions = struct
  type genre = [ `Male | `Female]
  type nork = [`Nik | `Hik of genre | `Hark | `Guk | `Zuk | `Zuek | `Haiek]
  type nor = [`Ni | `Hi | `Hura | `Gu | `Zu | `Zuek | `Haiek]
  type question = [ `Nor of nor |`NorNork of (nor * nork)] * string * [ `Past | `Present]

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

  type mode = {
    nb_of_questions: int;
    v_mode: [`Nor | `NorNork | `All];
    t_mode: [`Past | `Present | `All];
  }

  type t = {
    mode: mode;
    current_score: Score.t;
  }

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

  let create nb_of_questions v_mode t_mode = {
    mode = {
      nb_of_questions = nb_of_questions;
      v_mode = v_mode_of_string v_mode;
      t_mode = t_mode_of_string t_mode;
    };
    current_score = Score.create nb_of_questions;
  }

  let generate_question t =
    let valid_modes =
      match t.mode.v_mode with
        | `Nor -> [`Nor]
        | `NorNork -> [`NorNork]
        | `All -> modes
    in
    let valid_times =
      match t.mode.t_mode with
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
end

module Game = struct
  type game_params = {
    nb_questions_input: Dom_html.selectElement Js.t;
    v_mode_input: Dom_html.selectElement Js.t;
    t_mode_input: Dom_html.selectElement Js.t;
    start_game: Dom_html.buttonElement Js.t;
    restart_game: Dom_html.buttonElement Js.t;
  }

  let create_game_params nb_questions_input v_mode_input t_mode_input start_button restart_button = {
    nb_questions_input = Html5.To_dom.of_select nb_questions_input;
    v_mode_input = Html5.To_dom.of_select v_mode_input;
    t_mode_input = Html5.To_dom.of_select t_mode_input;
    start_game = Html5.To_dom.of_button start_button;
    restart_game = Html5.To_dom.of_button restart_button;
  }

  type 'a t = {
    question_board: 'a Eliom_content.Html5.elt;
    answer_input: Dom_html.inputElement Js.t;
    answer_output: 'a Eliom_content.Html5.elt;
    result_div: 'a Eliom_content.Html5.elt;
    game_params: game_params;
    mutable current_game: GenerateQuestions.t option;
    mutable current_question: GenerateQuestions.question option;
    mutable answered: bool;
    start_game_div: Dom_html.divElement Js.t;
    game_ongoing_div: Dom_html.divElement Js.t;
  }

  let is_finished t =
    match t.current_game with
      | None -> false
      | Some x -> x.GenerateQuestions.current_score.Score.remaining_question = 0

  let current_question_to_str ?answer:(a=None) t =
    match t.current_question with
      | None -> ""
      | Some current_question ->
        let ps = Printf.sprintf in
        let answer =
          match a with
            | None -> "..."
            | Some x -> x
        in
        let open GenerateQuestions in
        match current_question with
          | (`Nor nor, v, time) -> ps "%s %s %s (%s)" (nor_to_string nor) v answer (time_to_string time)
          | (`NorNork (nor, nork), v, time) -> ps "%s %s %s %s (%s)" (nork_to_string nork) (nor_to_string nor) v answer (time_to_string time)

  let display_current_mode t =
    Html5.Manip.replaceChildren t.question_board [pcdata (current_question_to_str t)]

  let next_game t =
    let () =
      match t.current_game with
        | None -> ()
        | Some g -> t.current_question <- Some (GenerateQuestions.generate_question g)
    in
    let () = t.answer_input##value <- Js.string "" in
    let () = t.answered <- false in
    display_current_mode t

  let start_game t =
    let nb_questions = int_of_string (Utils.get_input_text t.game_params.nb_questions_input) in
    let v_mode = Utils.get_input_text t.game_params.v_mode_input in
    let t_mode = Utils.get_input_text t.game_params.t_mode_input in
    t.current_game <- Some (GenerateQuestions.create nb_questions v_mode t_mode)

  let display_result t score =
    let () = Html5.Manip.replaceChildren t.result_div [pcdata (Score.to_string score)] in
    let () = Utils.hidde_element t.answer_input in
    Html5.Manip.replaceChildren t.question_board []

  let handle_answer t score =
    let () = t.answered <- true in
    match t.current_question with
      | None -> assert(false)
      | Some (mode, v, time) ->
        let answer = Tables.conjugate mode time in
        let message = Utils.get_input_text t.answer_input in
        let current_message =
          if message = answer then (Score.good_answer score; "Oso ondo !")
          else begin
            let () = Score.bad_answer score in
            "Zuzenketa: " ^ (current_question_to_str t ~answer:(Some answer))
          end
        in
        let output_message = [current_message] in
        let () = Html5.Manip.replaceChildren t.answer_output (List.map (fun x -> pcdata x) output_message) in
        if is_finished t then display_result t score
        else next_game t


  let on_answer_input_changes t _ _ =
    let () =
      if is_finished t then ()
      else begin
        match t.current_game with
          | None -> ()
          | Some game -> handle_answer t game.GenerateQuestions.current_score
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
        answered = false;
        start_game_div = Html5.To_dom.of_div start_game_div;
        game_ongoing_div = Html5.To_dom.of_div game_ongoing_div;
      } in
    res

  let setup t =
    let open Lwt_js_events in
        async (fun () ->
          changes t.answer_input (on_answer_input_changes t));
        async (fun () ->
          clicks t.game_params.start_game (on_start_game_clicks t));
        async (fun () ->
          clicks t.game_params.restart_game (on_restart_game_clicks t))
end

}}

{server{

let list_to_select s l =
  let list = List.map (fun name -> Option ([], name, Some (pcdata name), true)) l in
  let head = Option ([], s, Some (pcdata s), true)  in
  head, list

let service =
    Eliom_service.App.service ~path:["game"] ~get_params:Eliom_parameter.unit ()

let game_mode_inputs () =
  let build_raw_select name default others =
    let h, l = list_to_select default others in
    raw_select ~a:[] ~required:(pcdata "") ~name:name h l
  in
  build_raw_select "number of questions" "5" ["1"; "10"; "25"; "50"; "100"],
  build_raw_select "verb mode" "all" ["nor"; "nor/nork"],
  build_raw_select "time mode" "all" ["present"; "past"]

let f =
  (fun () () ->
    let question_board = div [] in
    let answer_input = string_input ~input_type:`Text () in
    let nquestions_input, v_mode_input, t_mode_input = game_mode_inputs () in
    let start_game_button = button ~a:[a_class ["btn"; "btn-sm"; "btn-primary"]] ~button_type:`Button [pcdata "Start"] in
    let start_game_div = div [div [pcdata "how many questions would you like in that game ?"; nquestions_input];
                              div [pcdata "in which mode would you like to conjugate the verbs?"; v_mode_input];
                              div [pcdata "in which time would you like to conjugate the verbs?"; t_mode_input];
                              div [start_game_button]] in
    let restart_game_button = button ~a:[a_class ["btn"; "btn-sm"; "btn-success"]] ~button_type:`Button [pcdata "Restart"] in
    let answer_output = div [] in
    let result_div = div [] in
    let game_ongoing_div = div ~a:[a_class ["hidden"]] [question_board; answer_input; answer_output; result_div; restart_game_button] in
    let _ = {unit{
      let open Game in
      let game_mode = create_game_params %nquestions_input %v_mode_input %t_mode_input %start_game_button %restart_game_button  in
      let t = create %question_board %answer_input %answer_output game_mode %start_game_div %game_ongoing_div %result_div in
      setup t
    }} in
    let divs = div [start_game_div; game_ongoing_div;] in
    let otherh = Utils.create_bootstrap_head () in
    let b = Html5.F.(body [divs]) in
    let res = Eliom_tools.F.html ~title:"game" ~css:[["css";"hizkuntzak.css"]]
      ~other_head:otherh b in
    Lwt.return res)

}}
