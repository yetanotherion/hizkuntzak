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

{client{

module Greeting = struct
  type t = [ `Hello
           | `GoodAdverb
           | `GoodAdjective
           ]

  let values = [`Hello; `GoodAdverb; `GoodAdjective]

  let to_english x =
    match x with
      | `Hello -> "hello"
      | `GoodAdverb -> "very good (adverb)"
      | `GoodAdjective -> "very good (adjective)"

  let to_russian x =
    match x with
      | `Hello -> "привет"
      | `GoodAdverb -> "хорошо"
      | `GoodAdjective -> "хороший"
end

module Geography = struct
  type t = [ `France
           | `Crimea
           | `Moscow
           | `Russia]

  let values = [`France; `Crimea; `Moscow; `Russia]

  let to_english x =
    match x with
      | `France -> "France"
      | `Crimea -> "Crimea"
      | `Moscow -> "Moscow"
      | `Russia -> "Russia"

  let to_russian x =
    match x with
      | `France -> "Франция"
      | `Crimea -> "Крым"
      | `Moscow -> "Москва"
      | `Russia -> "Россия"

end

module Location = struct
  type t = [ `Corner | `Floor ]
  let values = [`Corner; `Floor]

  let to_english x =
    match x with
      | `Corner -> "corner"
      | `Floor -> "floor"

  let to_russian x =
    match x with
      | `Corner -> "угол"
      | `Floor -> "пол"
end

module FoodAndDrink = struct
  type t = [`Beer
           | `Wine
           | `Kitchen
           | `CoffeeHouse
           | `Rice]
  let values = [`Beer; `Wine; `Kitchen; `CoffeeHouse; `Rice]

  let to_english x =
    match x with
      | `Beer -> "beer"
      | `Wine -> "wine"
      | `Kitchen -> "kitchen"
      | `CoffeeHouse -> "coffee house"
      | `Rice -> "rice"

  let to_russian x =
    match x with
      | `Beer -> "пиво"
      | `Wine -> "вино"
      | `Kitchen -> "кухня"
      | `CoffeeHouse -> "кафе"
      | `Rice -> "рис"
end

module Transport = struct
  type t = [`Tram
           | `Car
           | `Taxi]
  let values = [`Tram; `Car; `Taxi]

  let to_english x =
    match x with
      | `Tram -> "tram"
      | `Car -> "car"
      | `Taxi -> "taxi"

  let to_russian x =
    match x with
      | `Tram -> "трамвай"
      | `Car -> "автомобиль"
      | `Taxi -> "такси"
end

module Building = struct
  type t = [ `Airport
           | `Building
           | `University ]
  let values = [`Airport; `Building; `University]

  let to_english x =
    match x with
      | `Airport -> "airport"
      | `Building -> "building"
      | `University -> "university"

  let to_russian x =
    match x with
      | `Airport -> "аэропорт"
      | `Building -> "здание"
      | `University -> "университет"
end

module HouseAndFurniture = struct
  type t = [`Closet
           | `Desk
           | `Notebook
           | `Letter
           | `House]
  let values = [`Closet; `Desk; `Notebook; `Letter; `House]
  let to_english x =
    match x with
      | `Closet -> "closet"
      | `Desk -> "desk"
      | `Notebook -> "notebook"
      | `Letter -> "letter"
      | `House -> "house"

  let to_russian x =
    match x with
      | `Closet -> "шкаф"
      | `Desk -> "бюро"
      | `Notebook -> "тетрадь"
      | `Letter -> "письмо"
      | `House -> "дом"

end

module Nature = struct
  type t = [`Garden | `Sea ]
  let values = [ `Garden; `Sea]

  let to_english x =
    match x with
      | `Garden -> "garden"
      | `Sea -> "sea"

  let to_russian x =
    match x with
      | `Garden -> "сад"
      | `Sea -> "море"

end

module PeopleFunction = struct
  type t = [`Tourist]
  let values = [ `Tourist]

  let to_english x =
    match x with
      | `Tourist -> "tourist"

  let to_russian x =
    match x with
      | `Tourist -> "турист"

end

module Verb = struct
  type t = [`Hope | `Cook | `See | `Play | `Want]
  let values = [`Hope; `Cook; `See; `Play; `Want]
  let to_english x =
    let res =
      match x with
      | `Hope -> "hope"
      | `Cook -> "cook"
      | `See -> "see"
      | `Play -> "play"
      | `Want -> "want"
    in
    "to " ^ res

  let to_russian x =
    match x with
    | `Hope -> "надеяться"
    | `Cook -> "готовить"
    | `See -> "видеть"
    | `Want -> "хотеть"
    | `Play -> "играть"
end

module Time = struct
  type t = [`Today | `Afternoon ]
  let values = [ `Today; `Afternoon ]
  let to_english x =
    match x with
    | `Today -> "today"
    | `Afternoon -> "afternoon"
  let to_russian x =
    match x with
    | `Today -> "сегодня"
    | `Afternoon -> "после обеда"
end

module Numbers = struct
  type t = [`Quarter]
  let values = [`Quarter]
  let to_english x =
    match x with
    | `Quarter -> "quarter"
  let to_russian x =
    match x with
    | `Quarter -> "четверть"
end

module Dictionary = struct
  let description = "Improve your dictionary"
  let default_num_of_questions = 5
  let supported_levels = [`Hard]
  let other_number_of_questions = [1; 10; 25; 50; 100]
  let correct_answer_message = "хорошо !"
  let bad_answer_prefix = "правильный ответ: "
  let arguments = Array.of_list []
  let is_there_help = false
  type create_arg = unit
  let create _ _ = Random.self_init ()

  type question = [ `Greeting of Greeting.t
                  | `Geography of Geography.t
                  | `Location of Location.t
                  | `FoodDrink of FoodAndDrink.t
                  | `Transport of Transport.t
                  | `Building of Building.t
                  | `HouseFurniture of HouseAndFurniture.t
                  | `Nature of Nature.t
                  | `PeopleFunction of PeopleFunction.t
                  | `Verb of Verb.t
                  | `Time of Time.t
                  | `Numbers of Numbers.t
                  ]
  let questions =
    (List.map (fun x -> `Greeting x) Greeting.values) @
      (List.map (fun x -> `Geography x) Geography.values) @
      (List.map (fun x -> `Location x) Location.values) @
      (List.map (fun x -> `FoodDrink x) FoodAndDrink.values) @
      (List.map (fun x -> `Transport x) Transport.values) @
      (List.map (fun x -> `Building x) Building.values) @
      (List.map (fun x -> `HouseFurniture x) HouseAndFurniture.values) @
      (List.map (fun x -> `Nature x) Nature.values) @
      (List.map (fun x -> `PeopleFunction x) PeopleFunction.values) @
      (List.map (fun x -> `Verb x) Verb.values) @
      (List.map (fun x -> `Time x) Time.values) @
      (List.map (fun x -> `Numbers x) Numbers.values)


  type t = unit
  let generate_question () = Games.random_element questions

  let current_question_to_str q =
    match q with
      | `Greeting g -> Greeting.to_english g
      | `Geography g -> Geography.to_english g
      | `Location l -> Location.to_english l
      | `FoodDrink f -> FoodAndDrink.to_english f
      | `Transport t -> Transport.to_english t
      | `Building b -> Building.to_english b
      | `HouseFurniture h -> HouseAndFurniture.to_english h
      | `Nature n -> Nature.to_english n
      | `PeopleFunction p -> PeopleFunction.to_english p
      | `Verb v -> Verb.to_english v
      | `Time t -> Time.to_english t
      | `Numbers n -> Numbers.to_english n


  let question_to_string q answero =
    let ps = Printf.sprintf in
    let current_q_str = current_question_to_str q in
    match answero with
      | None -> ps "%s: ... ?" current_q_str
      | Some x -> ps "%s: %s" current_q_str x

  let question_answer q =
    match q with
      | `Greeting g -> Greeting.to_russian g
      | `Geography g -> Geography.to_russian g
      | `Location l -> Location.to_russian l
      | `FoodDrink f -> FoodAndDrink.to_russian f
      | `Transport t -> Transport.to_russian t
      | `Building b -> Building.to_russian b
      | `HouseFurniture h -> HouseAndFurniture.to_russian h
      | `Nature n -> Nature.to_russian n
      | `PeopleFunction p -> PeopleFunction.to_russian p
      | `Verb v -> Verb.to_russian v
      | `Time t -> Time.to_russian t
      | `Numbers n -> Numbers.to_russian n

  type help_t = unit
  let get_help _ _ = (), []
  let stop_help () = ()
end

module RuDictClient = Games.Make(Dictionary)

}}

{server{

let service u u_bis =
  let _ = {unit{
    let doc = Dom_html.document in
    let parent =
      Js.Opt.get (doc##getElementById(Js.string "main"))
        (fun () -> assert false)
    in
    RuDictClient.create_and_setup parent ()
  }}
  in
  Games.return_page "dictionary"

}}
