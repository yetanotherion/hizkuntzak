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
     let get word =
       let idx =
         match Genre.get word with
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
     let get is_plural word =
       if is_plural then "их"
       else
         match Genre.get word with
         | `Neutral | `Masculine -> "его"
         | `Feminine -> "её"
   end

 let names = [("Iban", `Masculine); ("Igor", `Masculine);
              ("Irina", `Feminine); ("Otxanda", `Feminine); ("пoле", `Neutral)]
}}

{shared{
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

 module GenreGame = Games.Make(GenreParam)
 module Text = struct
     type t = string
 end
}}

{server{
open Eliom_content
open Html5.D

let get_genres words =
  let res = List.map (fun x -> (x, Genre.get x)) words in
  Lwt.return res

let rpc_get_genres =
  server_function Json.t<string list>
    (get_genres: string list -> (string * GenreIo.t) list Lwt.t)


let genre_service unused unused_bis =
  let _ = {_{
    let doc = Dom_html.document in
    let parent =
      Js.Opt.get (doc##getElementById(Js.string "main"))
        (fun () -> assert false)
    in
    let words = ["aвaрия"; "aвтобус"; "вaгaж"; "письмо"; "пoле"] in
    lwt words_and_genres = %rpc_get_genres words in
    let () = GenreGame.create_and_setup parent words_and_genres in
    Lwt.return_unit
  }}
  in
  Games.return_page "genre"


}}
