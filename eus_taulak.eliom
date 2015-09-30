{shared{
open Eliom_lib
open Eliom_content
open Tyxml_js
}}

{client{
   module E = Eus_aditzak

   type current_mode_params = [`NorNork of (Dom_html.selectElement Js.t * Dom_html.selectElement Js.t * E.animation)
                              |`NorNori of (Dom_html.selectElement Js.t * Dom_html.selectElement Js.t * E.animation)]


   type ('select, 'div) t = {
       verb_mode:([> Html5_types.select ] as 'select) Html5.elt;
       div:([> Html5_types.div ] as 'div) Html5.elt;
       mutable curr_mode: current_mode_params;
     }

   let create_input label default_value other_values =
     let r = Html5.(select (List.map (fun x -> (option (pcdata x))) (default_value :: other_values))) in
     (Html5.pcdata label, r)

   let create_nor () = create_input "Nor" "ni" ["hi";
                                                "hura";
                                                "gu";
                                                "zu";
                                                "zuek";
                                                "haiek"]

   let create_nork () = create_input "Nork" "nik" ["hik (male)";
                                                   "hik (female)";
                                                   "hark";
                                                   "guk";
                                                   "zuk";
                                                   "zuek";
                                                   "haiek"]

   let create_nori () = create_input "Nori" "niri" ["hiri (male)";
                                                    "hiri (female)";
                                                    "hari";
                                                    "guri";
                                                    "zuri";
                                                    "zuei";
                                                    "haiei"]

   let verb_mode_of_string x =
     match x with
     | "nor/nork" -> `NorNork
     | "nor/nori" -> `NorNori
     | _ -> assert(false)


   let display_verbs_params_out_of_inputs v =
     let verb_mode = Utils.get_input_text (To_dom.of_select v) in
     let height, width = 300, 700 in
     let mycanvas = E.create_canvas_elt height width in
     let create_two_dimensional_table first second f_animation =
       let animation = f_animation height width mycanvas in
       let trs = Html5.([tr [td first];
                         tr [td second];
                         tr [td [mycanvas]]]) in
       Html5.(tablex ~a:[a_class ["centered"]] ~thead:(thead []) [tbody trs]),
       animation
     in

     match (verb_mode_of_string verb_mode) with
       | `NorNork -> begin
           let nork_label, nork_select = create_nork () in
           let nor_label, nor_select = create_nor () in
           let t, a = create_two_dimensional_table [nork_label; nork_select]
                                                   [nor_label; nor_select] E.NorNorkAnimation.create_animation in
           (t,
            `NorNork (To_dom.of_select nork_select, To_dom.of_select nor_select, a))
         end
       | `NorNori -> begin
           let nor_label, nor_select = create_nor () in
           let nori_label, nori_select = create_nori () in
           let t, a = create_two_dimensional_table [nor_label; nor_select]
                                                   [nori_label; nori_select] E.NorNoriAnimation.create_animation in
           (t,
            `NorNori (To_dom.of_select nor_select, To_dom.of_select nori_select, a))
         end

   let nor_of_string x =
     match x with
     | "ni" -> `Ni
     | "hi" -> `Hi
     | "hura" -> `Hura
     | "gu" -> `Gu
     | "zu" -> `Zu
     | "zuek" -> `Zuek
     | "haiek" -> `Haiek
     | _ -> assert(false)

   let nork_of_string x =
     match x with
     | "nik" -> `Nik
     | "hik (male)" -> `Hik `Male
     | "hik (female)" -> `Hik `Female
     | "hark" -> `Hark
     | "guk" -> `Guk
     | "zuk" -> `Zuk
     | "zuek" -> `Zuek
     | "haiek" -> `Haiek
     | _ -> assert(false)

   let nori_of_string x =
     match x with
     | "niri" -> `Niri
     | "hiri (male)" -> `Hiri `Male
     | "hiri (female)" -> `Hiri `Female
     | "hari" -> `Hari
     | "guri" -> `Guri
     | "zuri" -> `Zuri
     | "zuei" -> `Zuei
     | "haiei" -> `Haiei
     | _ -> assert(false)

   let create_nor_nork_param nork nor =
     let nork = nork_of_string (Utils.get_input_text nork) in
     let nor = nor_of_string (Utils.get_input_text nor) in
     (nor, nork)

   let create_nor_nori_param nori nor =
     let nori = nori_of_string (Utils.get_input_text nori) in
     let nor = nor_of_string (Utils.get_input_text nor) in
     (nor, nori)

   let setup_animation t =
     match t.curr_mode with
     | `NorNork (nork, nor, t) ->
        let norNork = create_nor_nork_param nork nor in
        E.NorNorkAnimation.start_animation t norNork
     | `NorNori (nori, nor, t) ->
        let norNori = create_nor_nori_param nor nori in
        E.NorNoriAnimation.start_animation t norNori

   let stop_animation t =
     match t.curr_mode with
     | `NorNork (_, _, t) ->
        E.NorNorkAnimation.stop_animation t
     | `NorNori (_, _, t) ->
        E.NorNoriAnimation.stop_animation t


   let rec on_any_event ?(create_all=false) t _ _ =
     let setup_select_elt t elt =
       let open Lwt_js_events in
       async (fun () ->
              inputs elt (on_any_event t))
     in
     let setup_curr_mode t =
       match t.curr_mode with
       | `NorNork (nork, nor, _) -> begin
           let () = setup_select_elt t nork in
           setup_select_elt t nor
         end
       | `NorNori (nori, nor, _) -> begin
           let () = setup_select_elt t nori in
           setup_select_elt t nor
         end
     in
     let () = stop_animation t in
     let () =
       if create_all then
         let new_div, curr_mode = display_verbs_params_out_of_inputs t.verb_mode in
         let () = Games.replaceChildren (To_dom.of_div t.div) [To_dom.of_table new_div] in
         let () = t.curr_mode <- curr_mode in
         setup_curr_mode t
     in
     let _ = setup_animation t in
     Lwt.return_unit

   let setup t =
     let open Lwt_js_events in
     let v_mode = To_dom.of_select t.verb_mode in
     let () = async (fun () ->
                     inputs v_mode (on_any_event ~create_all:true t)) in
     (* XXX this is copy/pasted from above
        a cleaner solution would be to
        trigger an event during the setup *)
     let setup_select_elt t elt =
       let open Lwt_js_events in
       async (fun () ->
              inputs elt (on_any_event t))
     in
     let setup_curr_mode t =
       match t.curr_mode with
       | `NorNork (nork, nor, _) -> begin
           let () = setup_select_elt t nork in
           setup_select_elt t nor
         end
       | `NorNori (nori, nor, _) -> begin
           let () = setup_select_elt t nori in
           setup_select_elt t nor
         end
     in
     let _ = setup_curr_mode t in
     let _ = setup_animation t in
     ()

   let create v div =
     let new_div, curr_mode = display_verbs_params_out_of_inputs v in
     let () = Games.replaceChildren (To_dom.of_div div) [(To_dom.of_table new_div)] in
     {verb_mode=v;
      div=div;
      curr_mode=curr_mode}

}}

{server{

let service unused unused_bis =
  let all_div = Eliom_content.Html5.F.(div ~a:[a_class ["centered"]; a_id "main"] []) in
  let _ = {unit{
    let verb_mode = Tyxml_js.Html5.(select (List.map (fun x -> option (pcdata x)) ["nor/nork"; "nor/nori"])) in
    let verb_div = Html5.div [verb_mode] in
    let param_div = Html5.div [] in
    let t = create verb_mode param_div in
    let () = setup t in
    let doc = Dom_html.document in
    let parent =
      Js.Opt.get (doc##getElementById(Js.string "main"))
        (fun () -> assert false)
    in
    Games.replaceChildren parent [To_dom.of_div verb_div; To_dom.of_div param_div]
  }} in
  let otherh = Utils.create_bootstrap_head () in
  let b = Eliom_content.Html5.F.(body [all_div]) in
  let res = Eliom_tools.F.html ~title:"taulak" ~css:[["css";"hizkuntzak.css"]]
      ~other_head:otherh b in
  Lwt.return res

 }}
