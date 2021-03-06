{client{

module Dictionary = struct
  let title = "hiztegia"
  let description = "Hiztegia hobetu"
  let default_num_of_questions = 5
  let other_number_of_questions = [1; 10; 25; 50; 100]
  let correct_answer_message = "Oso ondo !"
  let bad_answer_prefix = "Erantzun zuzena: "
  let arguments = Array.of_list []
  let supported_levels = [`Normal; `Hard]
  type question = string * string
  type create_arg = question list

  type t = {
      mutable questions: question list;
      mutable taken_questions: question list;
    }

  let create arg _ =
    {questions = arg;
     taken_questions = []}

  let not_taken_questions t =
    List.filter
      (fun x ->
       not (List.exists (fun y -> x == y) t.taken_questions))
      t.questions

  let generate_question t =
    let sample =
      match not_taken_questions t with
      | [] -> t.questions
      | l -> l
    in
    let res = Games.random_element sample in
    let taken_questions =
      if not (List.exists (fun x -> x == res) t.taken_questions) then
        res :: t.taken_questions
      else t.taken_questions
    in
    t.taken_questions <- taken_questions;
    res

  let current_question_to_str q = Pervasives.fst q

  let question_to_string q answero =
    let ps = Printf.sprintf in
    let current_q_str = current_question_to_str q in
    match answero with
      | None -> ps "%s: ... ?" current_q_str
      | Some x -> ps "%s: %s" current_q_str x

  let question_answer q = Pervasives.snd q
  let nber_additional_answers = 4
  let question_additional_answers t question =
    let number_of_other_questions = min ((List.length t.questions) - 1)
                                        nber_additional_answers in
    let other_questions = List.filter
                            (fun x -> x <> question)
                            t.questions in
    let chosen = Utils.(first_n
                          (reorder_list other_questions)
                          number_of_other_questions) in
    List.map Pervasives.snd chosen

  let is_there_help = false
  type help_t = unit
  let get_help _ _ = (), []
  let stop_help () = ()

  end

module Client = Games.Make(Dictionary)
}}
