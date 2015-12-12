{server{

 module Genre = struct

    let alphabet = "йцукенгшщзхъфываролджэёячсмитьбюп"
    let consonants = "цкнгшщзхъфврлджэёчсмтб"

    let is_consonant letter = Text.contains consonants letter
    let last_char word = Text.get word (-1)
    let eq word1 word2 = Text.compare word1 word2 == 0

    let is_masculine word =
      let last = last_char word in
      (is_consonant last) || eq last "й"

    let is_feminine word =
      let last = last_char word in
      eq last "а" || eq last "я"

    let is_neutral word =
      let last = last_char word in
      eq last "о" || eq last "е"

    let get word =
      if is_masculine word then `Masculine
      else if is_feminine word then `Feminine
      else if is_neutral word then `Neutral
      else
        let res = Printf.sprintf "Unknown word %s" word in
        failwith(res)
   end
}}

{shared{

 module Moi = struct
     let choices = ["мой"; "моя"; "моё"]
   end

 module Tvoi = struct
     let choices = ["твой"; "твоя"; "твоё"]
   end

 module Baj = struct
     let choices = ["ваш"; "вашa"; "ваше"]
   end

 module Naj = struct
     let choices = ["наш"; "нашa"; "наше"]
   end

 module MakeP (F: sig val choices : string list end) = struct
     let get genre =
       let idx =
         match genre with
         | `Masculine -> 0
         | `Feminine -> 1
         | `Neutral -> 2
       in
       List.nth F.choices idx
   end

 module MMoi = MakeP(Moi)
 module MTvoi = MakeP(Tvoi)
 module MBaj = MakeP(Baj)
 module MNaj = MakeP(Naj)

 module Yevo = struct
     let get is_plural genre =
       if is_plural then "их"
       else
         match genre with
         | `Neutral | `Masculine -> "его"
         | `Feminine -> "её"
   end

 let names = [("Iban", `Masculine); ("Igor", `Masculine);
              ("Irina", `Feminine); ("Otxanda", `Feminine); ("пoле", `Neutral)]

 module GenreIo = struct
     let to_string g =
       match g with
       | `Masculine -> "maskulinoa"
       | `Feminine -> "femininoa"
       | `Neutral -> "neutroa"

     type t = [`Masculine | `Feminine | `Neutral]
   end
}}


{client{
 module GenreParam = struct
  let title = "feminino maskulino ala neutro"
  let description = "Errusoz feminino maskulino ala neutroa landu"
  let default_num_of_questions = 5
  let other_number_of_questions = [1; 10; 25; 50; 100]
  let correct_answer_message = "Oso ondo !"
  let bad_answer_prefix = "Erantzun zuzena: "
  let supported_levels = [`Normal; `Hard]
  type question = string * GenreIo.t
  type t = question list
  let create x _ = x
  let arguments = Array.of_list []
  type create_arg = t
  type help_t = unit
  let get_help _ _ = (), []
  let stop_help _ = ()
  let is_there_help = false

  let generate_question (t:t) =
    Games.random_element t

  let question_to_string current_question answero =
    let ps = Printf.sprintf in
    let answer =
      match answero with
        | None -> ""
        | Some x -> x
    in
    let curr_q, _ = current_question in
    ps "%s ? %s" curr_q answer

  let question_answer (question: question) =
    let _, answer = question in
    GenreIo.to_string answer

  let question_additional_answers _ question =
    let _, genre = question in
    let others =
      match genre with
      | `Masculine -> [`Feminine; `Neutral]
      | `Feminine -> [`Masculine; `Neutral]
      | `Neutral -> [`Masculine; `Feminine]
    in
    List.map GenreIo.to_string others

   end

 module PronounParam = struct
  let title = "Izenordeak"
  let description = "Izenorde errusoak landu"
  let default_num_of_questions = 5
  let other_number_of_questions = [1; 10; 25; 50; 100]
  let correct_answer_message = "Oso ondo !"
  let bad_answer_prefix = "Erantzun zuzena: "
  let supported_levels = [`Normal]
  let pronouns = [`Moi; `Tvoi; `Baj; `Naj ]
  type pronoun = [`Moi | `Tvoi | `Baj | `Naj]

  type question = string * GenreIo.t * pronoun
  type t = (string * GenreIo.t) list
  let create x _ = x
  let arguments = Array.of_list []
  type create_arg = t
  type help_t = unit
  let get_help _ _ = (), []
  let stop_help _ = ()
  let is_there_help = false

  let generate_question (t:t) =
    let q, genre = Games.random_element t in
    let pronoun = Games.random_element pronouns in
    q, genre, pronoun

  let question_to_string current_question answero =
    let ps = Printf.sprintf in
    let answer =
      match answero with
        | None -> ""
        | Some x -> x
    in
    let curr_q, _, _  = current_question in
    ps "%s ? %s" curr_q answer

  let apply_pronoun pronoun genre =
    match pronoun with
    | `Moi -> MMoi.get genre
    | `Tvoi -> MTvoi.get genre
    | `Baj -> MBaj.get genre
    | `Naj -> MNaj.get genre

  let question_answer (question: question) =
    let _, answer, pronoun = question in
    apply_pronoun pronoun answer

  let question_additional_answers _ question =
    let _, genre, pronoun = question in
    let others =
      match genre with
      | `Masculine -> [`Feminine; `Neutral]
      | `Feminine -> [`Masculine; `Neutral]
      | `Neutral -> [`Masculine; `Feminine]
    in
    List.map (apply_pronoun pronoun) others

   end

 module GenreGame = Games.Make(GenreParam)
 module PronounGame = Games.Make(PronounParam)
 module Text = struct
     type t = string
 end
}}

{server{
open Eliom_content
open Html5.D

let get_words_and_genres () =
  lwt words = Db.Translation.get_all_nouns_in_language "ru" in
  let res = List.map (fun x ->
                      try Some (x, Genre.get x)
                      with _ -> None) words in
  let only_correct = List.filter (fun x ->
                                  match x with
                                  | None -> false
                                  | Some _ -> true) res in
  let must xo =
    match xo with
    | None -> assert(false)
    | Some x -> x in

  Lwt.return (List.map must only_correct)

let rpc_get_genres =
  server_function Json.t<unit>
    (get_words_and_genres: unit -> (string * GenreIo.t) list Lwt.t)


let genre_service unused unused_bis =
  let _ = {_{
    let doc = Dom_html.document in
    let parent =
      Js.Opt.get (doc##getElementById(Js.string "main"))
        (fun () -> assert false)
    in
    lwt words_and_genres = %rpc_get_genres () in
    let () = GenreGame.create_and_setup parent words_and_genres in
    Lwt.return_unit
  }}
  in
  Games.return_page "genre"

let pronoun_service unused unused_bis =
  let _ = {_{
    let doc = Dom_html.document in
    let parent =
      Js.Opt.get (doc##getElementById(Js.string "main"))
        (fun () -> assert false)
    in
    lwt words_and_genres = %rpc_get_genres () in
    let () = PronounGame.create_and_setup parent words_and_genres in
    Lwt.return_unit
  }}
  in
  Games.return_page "pronouns"


}}
