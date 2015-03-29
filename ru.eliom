{shared{
module Dictionary = struct
  let title = "dictionary"
  let description = "Improve your dictionary"
  let default_num_of_questions = 5
  let other_number_of_questions = [1; 10; 25; 50; 100]
  let correct_answer_message = "хорошо !"
  let bad_answer_prefix = "правильный ответ: "
  let arguments = Array.of_list []
  let create _ = Random.self_init ()
  let questions = [`Hello; `House; `Beer; `Wine; `VeryGood]
  type question = [ `Hello | `House | `Beer | `Wine | `VeryGood ]
  type t = unit
  let generate_question () = Games.random_element questions

  let current_question_to_str x =
    match x with
      | `Hello -> "hello"
      | `House -> "house"
      | `Beer -> "beer"
      | `Wine -> "wine"
      | `VeryGood -> "very good"

  let question_to_string q answero =
    let ps = Printf.sprintf in
    let current_q_str = current_question_to_str q in
    match answero with
      | None -> ps "%s: ... ?" current_q_str
      | Some x -> ps "%s: %s" current_q_str x

  let question_answer q =
    match q with
      | `Hello -> "привет"
      | `House -> "дом"
      | `Beer -> "пиво"
      | `Wine -> "вино"
      | `VeryGood -> "хорошо"
end
}}

{client{
module RuDictClient = Games.MakeClient(Dictionary)
}}

{server{
module RuDictServer = Games.MakeServer(Dictionary)

let service u u_bis =
  let inputs = RuDictServer.create_html_elements () in
  let _ = {unit{
    let inputs = %inputs in
    let open Games.GameHtmlElements in
    let other_inputs = Array.of_list inputs.other_inputs in
    RuDictClient.create_and_setup
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
  RuDictServer.return_page inputs
}}
