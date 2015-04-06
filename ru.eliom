{shared{

module Greeting = struct
  type t = [ `Hello
           | `VeryGood ]

  let values = [`Hello; `VeryGood]

  let to_english x =
    match x with
      | `Hello -> "hello"
      | `VeryGood -> "very good"

  let to_russian x =
    match x with
      | `Hello -> "привет"
      | `VeryGood -> "хорошо"
end

module Geography = struct
  type t = [ `France
           | `Crimea
           | `Moscow
           | `Russia]

  let values = [`France; `Crimea; `Moscow; `Russia]

  let to_english x =
    match x with
      | `France -> "france"
      | `Crimea -> "crimea"
      | `Moscow -> "moscow"
      | `Russia -> "russia"

  let to_russian x =
    match x with
      | `France -> "франция"
      | `Crimea -> "крым"
      | `Moscow -> "москва"
      | `Russia -> "россии"

end

module Location = struct
  type t = [ `Corner ]
  let values = [`Corner]

  let to_english x =
    match x with
      | `Corner -> "corner"

  let to_russian x =
    match x with
      | `Corner -> "угол"
end

module FoodAndDrink = struct
  type t = [`Beer
           | `Wine
           | `Kitchen
           | `CoffeeHouse]
  let values = [`Beer; `Wine; `Kitchen; `CoffeeHouse]

  let to_english x =
    match x with
      | `Beer -> "beer"
      | `Wine -> "wine"
      | `Kitchen -> "kitchen"
      | `CoffeeHouse -> "coffee house"

  let to_russian x =
    match x with
      | `Beer -> "пиво"
      | `Wine -> "вино"
      | `Kitchen -> "кухня"
      | `CoffeeHouse -> "кафе"
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
      | `Car -> "автомобилъ"
      | `Taxi -> "такси"
end

module Building = struct
  type t = [`University]
  let values = [`University]

  let to_english x =
    match x with
      | `University -> "university"

  let to_russian x =
    match x with
      | `University -> "университет"
end

module HouseAndFurniture = struct
  type t = [`Cabinet
           | `Desk
           | `House]
  let values = [`Cabinet; `Desk; `House]
  let to_english x =
    match x with
      | `Cabinet -> "cabinet"
      | `Desk -> "desk"
      | `House -> "house"

  let to_russian x =
    match x with
      | `Cabinet -> "шкаф"
      | `Desk -> "бюро"
      | `House -> "дом"

end

module Dictionary = struct
  let title = "dictionary"
  let description = "Improve your dictionary"
  let default_num_of_questions = 5
  let other_number_of_questions = [1; 10; 25; 50; 100]
  let correct_answer_message = "хорошо !"
  let bad_answer_prefix = "правильный ответ: "
  let arguments = Array.of_list []
  let create _ = Random.self_init ()

  type question = [ `Greeting of Greeting.t
                  | `Geography of Geography.t
                  | `Location of Location.t
                  | `FoodDrink of FoodAndDrink.t
                  | `Transport of Transport.t
                  | `Building of Building.t
                  | `HouseFurniture of HouseAndFurniture.t]

  let questions =
    (List.map (fun x -> `Greeting x) Greeting.values) @
      (List.map (fun x -> `Geography x) Geography.values) @
      (List.map (fun x -> `Location x) Location.values) @
      (List.map (fun x -> `FoodDrink x) FoodAndDrink.values) @
      (List.map (fun x -> `Transport x) Transport.values) @
      (List.map (fun x -> `Building x) Building.values) @
      (List.map (fun x -> `HouseFurniture x) HouseAndFurniture.values)

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