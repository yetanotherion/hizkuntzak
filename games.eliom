{shared{
open Eliom_lib
open Eliom_content
open Html5.D
}}

{client{
let modes = [`Nor; `NorNork]
let times = [`Past; `Present]
let nor_modes = [`Ni; `Hi; `Hura; `Gu; `Zu; `Zuek; `Haiek]
let nork_modes = [`Nik; (`Hik `Male); (`Hik `Female);
                  `Hark; `Guk; `Zuk; `Zuek; `Haiek]

let random_element l = List.nth l (Random.int (List.length l))

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

let choose_mode () =
  let mode, time = random_element modes, random_element times in
  match mode with
    | `Nor -> (`Nor (random_element nor_modes), RandomVerbs.random_nor_verb (), time)
    | `NorNork -> (`NorNork (random_element nor_modes,
                             random_element nork_modes),
                   RandomVerbs.random_nor_nork_verb (),
                   time)
type genre = [ `Male | `Female]
type nork = [`Nik | `Hik of genre | `Hark | `Guk | `Zuk | `Zuek | `Haiek]
type nor = [`Ni | `Hi | `Hura | `Gu | `Zu | `Zuek | `Haiek]

type question = [ |`Nor of nor |`NorNork of (nor * nork)] * string * [ `Past | `Present]

type score = {
  start_time: float;
  mutable nber_ok: int;
  mutable remaining_question: int;
  nber_of_questions: int;
}

let new_score nber_of_questions = {
  start_time = Unix.gettimeofday ();
  nber_ok = 0;
  remaining_question = nber_of_questions;
  nber_of_questions = nber_of_questions;
}

let good_answer s =
  s.remaining_question <- s.remaining_question - 1;
  s.nber_ok <- s.nber_ok + 1

let bad_answer s =
  s.remaining_question <- s.remaining_question - 1

let score_to_string score =
  let stop_date = Unix.gettimeofday () in
  let f = stop_date -. score.start_time in
  Printf.sprintf "%d/%d on %.2f seconds" score.nber_ok score.nber_of_questions f

type game = {
  mode: int;
  current_score: score;
}

let create_game nber_questions = {
  mode = nber_questions;
  current_score = new_score nber_questions;
}

type game_params = {
  game_mode_input: Dom_html.selectElement Js.t;
  start_game: Dom_html.buttonElement Js.t;
  restart_game: Dom_html.buttonElement Js.t;
}

let create_game_params mode start_button restart_button = {
  game_mode_input = Html5.To_dom.of_select mode;
  start_game = Html5.To_dom.of_button start_button;
  restart_game = Html5.To_dom.of_button restart_button;

}

type 'a t = {
  question_board: 'a Eliom_content.Html5.elt;
  answer_input: Dom_html.inputElement Js.t;
  answer_output: 'a Eliom_content.Html5.elt;
  result_div: 'a Eliom_content.Html5.elt;
  game_params: game_params;
  mutable current_game: game option;
  mutable curr_question: question;
  mutable answered: bool;
  start_game_div: Dom_html.divElement Js.t;
  game_ongoing_div: Dom_html.divElement Js.t;
}

let is_finished t =
  match t.current_game with
    | None -> false
    | Some x -> x.current_score.remaining_question = 0


let curr_question_to_str ?answer:(a=None) t =
  let ps = Printf.sprintf in
  let answer =
    match a with
      | None -> "..."
      | Some x -> x
  in
  match t.curr_question with
    | (`Nor nor, v, time) -> ps "%s %s %s (%s)" (nor_to_string nor) v answer (time_to_string time)
    | (`NorNork (nor, nork), v, time) -> ps "%s %s %s %s (%s)" (nork_to_string nork) (nor_to_string nor) v answer (time_to_string time)

let display_current_mode t =
  Html5.Manip.replaceChildren t.question_board [pcdata (curr_question_to_str t)]


let next_game t =
  let () = t.curr_question <- choose_mode () in
  let () = t.answer_input##value <- Js.string "" in
  let () = t.answered <- false in
  display_current_mode t

let start_game t =
  let mode = int_of_string (Utils.get_input_text t.game_params.game_mode_input) in
  t.current_game <- Some (create_game mode)

let display_result t score =
  let () = Html5.Manip.replaceChildren t.result_div [pcdata (score_to_string score)] in
  let () = Utils.hidde_element t.answer_input in
  Html5.Manip.replaceChildren t.question_board []

let handle_answer t score =
  let () = t.answered <- true in
  let mode, v, time = t.curr_question in
  let answer = Tables.conjugate mode time in
  let message = Utils.get_input_text t.answer_input in
  let current_message =
    if message = answer then (good_answer score; "Oso ondo !")
    else begin
      let () = bad_answer score in
      "Zuzenketa: " ^ (curr_question_to_str t ~answer:(Some answer))
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
        | Some game -> handle_answer t game.current_score
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
      curr_question = choose_mode ();
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

}}

{server{

let list_to_select s l =
  let list = List.map (fun name -> Option ([], name, Some (pcdata name), true)) l in
  let head = Option ([], s, Some (pcdata s), true)  in
  head, list

let service =
    Eliom_service.App.service ~path:["game"] ~get_params:Eliom_parameter.unit ()

let f =
  (fun () () ->
    let question_board = div [] in
    let answer_input = string_input ~input_type:`Text () in
    let h, l = list_to_select "5" ["1"; "10"; "25"; "50"; "100"] in
    let game_mode_input = raw_select
      ~a:[]
      ~required:(pcdata "")
      ~name:"number of questions"
      h l
    in
    let start_game_button = button ~a:[a_class ["btn"; "btn-sm"; "btn-primary"]] ~button_type:`Button [pcdata "Start"] in
    let start_game_div = div [div [pcdata "how many questions would you like in that game ?"; game_mode_input];
                              div [start_game_button]] in
    let restart_game_button = button ~a:[a_class ["btn"; "btn-sm"; "btn-success"]] ~button_type:`Button [pcdata "Restart"] in
    let answer_output = div [] in
    let result_div = div [] in
    let game_ongoing_div = div ~a:[a_class ["hidden"]] [question_board; answer_input; answer_output; result_div; restart_game_button] in
    let _ = {unit{
      let game_mode = create_game_params %game_mode_input %start_game_button %restart_game_button  in
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
