module Lwt_thread = struct
  include Lwt
  include Lwt_chan
end
module Lwt_PGOCaml = PGOCaml_generic.Make(Lwt_thread)
module Lwt_Query = Query.Make_with_Db(Lwt_thread)(Lwt_PGOCaml)

let must x =
  match x with
  | None -> assert(false)
  | Some x -> x



module LangDb:
(sig
    val full_transaction_block:
      ((string, bool) Hashtbl.t Lwt_PGOCaml.t -> 'a Lwt.t) -> 'a Lwt.t
    val use_db :
      ((string, bool) Hashtbl.t Lwt_PGOCaml.t -> 'a Lwt.t) -> 'a Lwt.t
    val find: string -> int32 Lwt.t
    val find_lang: int32 -> string Lwt.t
    val get_supported_languages: unit -> string list Lwt.t
 end) = struct

 (* a new language can only be added out of the application.
    The application must be rebooted so that it is taken
    into account *)

    module StringHash = struct
        type t = string
        let equal i j = i = j
        let hash = Hashtbl.hash
      end

    module IntHash = struct
        type t = int32
        let equal i j = i = j
        let hash = Hashtbl.hash
      end

    module StringHashtbl = Hashtbl.Make (StringHash)
    module IntHashtbl = Hashtbl.Make (IntHash)

    type t = {
        lang_to_id: Int32.t StringHashtbl.t;
        id_to_lang: string IntHashtbl.t;
        pool: (string, bool) Hashtbl.t Lwt_PGOCaml.t Lwt_pool.t;
      }

    let table = <:table< languages (
                 id integer NOT NULL,
                 lang text NOT NULL) >>

    let load_table dbh =
      lwt res = Lwt_Query.query dbh <:select< row | row in $table$>> in
      let length = List.length res in
      let h1, h2 = StringHashtbl.create length, IntHashtbl.create length in
      let () = List.iter (fun x -> StringHashtbl.add h1 x#!lang x#!id) res in
      let () = List.iter (fun x -> IntHashtbl.add h2 x#!id x#!lang) res in
      Lwt.return (h1, h2)

    let connect_to_db () =
      try_lwt
        Lwt_PGOCaml.connect ~database:"hizkuntzak" ()
      with _ ->
        Lwt_PGOCaml.connect ~host:"127.0.0.1"
                            ~user:"postgres"
                            ~password:"postgres"
                            ~port:5432
                            ~database:"hizkuntzak" ()

    (* from eliom-base-app *)
    let validate db =
     try_lwt
       lwt () = Lwt_PGOCaml.ping db in
       Lwt.return true
     with _ ->
       Lwt.return false

    let transaction_block db f =
      lwt () = Lwt_PGOCaml.begin_work db in
      try_lwt
        lwt r = f () in
        lwt () = Lwt_PGOCaml.commit db in
        Lwt.return r
      with e ->
        lwt () = Lwt_PGOCaml.rollback db in
        Lwt.fail e

    let create () =
      let pool = Lwt_pool.create 16 ~validate connect_to_db in
      lwt lang_to_id, id_to_lang = Lwt_pool.use pool load_table in
      Lwt.return {pool=pool;
                  lang_to_id=lang_to_id;
                  id_to_lang=id_to_lang}

    let tref = ref None

    let get () =
      match !tref with
      | None -> begin
         lwt t = create () in
         let () = tref := Some t in
         Lwt.return t
        end
      | Some t -> Lwt.return t

    let full_transaction_block f =
      lwt t = get () in
      Lwt_pool.use t.pool (fun db -> transaction_block db (fun () -> f db))

    let use_db f =
      lwt t = get () in
      Lwt_pool.use t.pool f

    let do_get_supported_languages t =
      StringHashtbl.fold (fun l _ accum -> l :: accum) t.lang_to_id []

    let get_supported_languages () =
      lwt t = get () in
      Lwt.return (do_get_supported_languages t)

    let do_find_id t lang =
      if StringHashtbl.mem t.lang_to_id lang then
        (StringHashtbl.find t.lang_to_id lang)
      else begin
        let supported = do_get_supported_languages t in
        let msg = Printf.sprintf "unknown language: %s (supported: %s)" lang (String.concat "," supported) in
        raise (Failure msg)
        end

    let do_find_lang t id = IntHashtbl.find t.id_to_lang id

    let find lang =
      lwt t = get () in
      try
        let res = do_find_id t lang in
        Lwt.return res
      with e -> Lwt.fail e

    let find_lang lang =
      lwt t = get () in
      try
        let res = do_find_lang t lang in
        Lwt.return res
      with e -> Lwt.fail e

  end



module Word = struct
    let id = <:sequence< serial "words_id" >>
    let table = <:table< words (
                 id integer NOT NULL DEFAULT(nextval $id$),
                 word text NOT NULL,
                 lang integer NOT NULL) >>

    let words_in_language language =
      lwt language_id = LangDb.find language in
      Lwt.return << row |
                    row in $table$;
                    row.lang = $int32:language_id$ >>

    let get_words word_ids =
      <:select< {row.word} |
                 row in $table$;
                 word in $word_ids$;
                 row.id = word.id >>

    let get_word_id dbh word language_id =
      match_lwt (Lwt_Query.query dbh <:select< row |
                                               row in $table$;
                                               row.word = $string:word$;
                                               row.lang = $int32:language_id$>>)
      with
      | [] -> Lwt.return None
      | hd :: _ -> Lwt.return (Some hd#!id)

    let _insert_word dbh word lang =
      Lwt_Query.query dbh
       <:insert< $table$ := {word = $string:word$;
                             lang = $int32:lang$;
        id = table?id}>>

    let do_insert_word dbh word lang =
      lwt language_id = LangDb.find lang in
      match_lwt (get_word_id dbh word language_id) with
      | Some hd -> Lwt.return hd
      | None -> begin
          lwt () = _insert_word dbh word language_id in
          lwt word_id = get_word_id dbh word language_id in
          Lwt.return (must word_id)
        end


    let insert_word word lang =
      LangDb.full_transaction_block
        (fun dbh -> do_insert_word dbh word lang)

    type t = {
        id: Int32.t;
        word: string;
        language: string;
      }

    let get dbh word lang =
      (* get the id of the word "word" associated to a language "lang".
         Note that this method inserts the word associated to language
         "lang" if it does not exist *)
      lwt id = do_insert_word dbh word lang in
      Lwt.return {id=id;
                  word=word;
                  language=lang}

    let find word_id = << {id = row.id} |
                          row in $table$;
                          row.id = $int32:word_id$ >>

  end

let get_current_date () = CalendarLib.Date.from_unixfloat (Unix.time ())

module User = struct
    exception User_not_found
    exception User_exists

    let id = <:sequence< serial "users_id" >>
    let table = <:table< users (
                 id integer NOT NULL DEFAULT(nextval $id$),
                 username text NOT NULL,
                 password text NOT NULL,
                 preferred_lang_src integer NOT NULL,
                 preferred_lang_dst integer NOT NULL,
                 last_visit_date date NOT NULL) >>

    let hash_password password = Bcrypt.string_of_hash (Bcrypt.hash password)
    let verify_password p1 p2 = Bcrypt.verify p1 (Bcrypt.hash_of_string p2)
    let default_src_lang = "eus"
    let default_dst_lang = "en"

    let do_insert dbh username password =
      lwt preferred_lang_src = LangDb.find default_src_lang in
      lwt preferred_lang_dst = LangDb.find default_dst_lang in
      let date =  get_current_date () in
      Lwt_Query.query dbh
       <:insert< $table$ := {username = $string:username$;
                             password = $string:hash_password password$;
                             id = table?id;
                             preferred_lang_src = $int32:preferred_lang_src$;
                             preferred_lang_dst = $int32:preferred_lang_dst$;
                             last_visit_date = $date:date$;
                            } >>

    let update_last_visit_date dbh id =
      let date = get_current_date () in
      Lwt_Query.query dbh
       <:update< row in $table$ := {last_visit_date = $date:date$} |
                 row.id = $int32:id$ >>

    let do_update_preferred_lang_src dbh id lang =
      lwt preferred_lang = LangDb.find lang in
      Lwt_Query.query dbh
       <:update< row in $table$ :=
                 {preferred_lang_src = $int32:preferred_lang$} |
                 row.id = $int32:id$ >>

    let do_update_preferred_lang_dst dbh id lang =
      lwt preferred_lang = LangDb.find lang in
      Lwt_Query.query dbh
       <:update< row in $table$ :=
                 {preferred_lang_dst = $int32:preferred_lang$} |
                 row.id = $int32:id$ >>

    let update_preferred_lang_src id lang =
      LangDb.full_transaction_block
        (fun dbh -> do_update_preferred_lang_src dbh id lang)

    let update_preferred_lang_dst id lang =
      LangDb.full_transaction_block
        (fun dbh -> do_update_preferred_lang_dst dbh id lang)

    let do_delete dbh user_id =
      Lwt_Query.query dbh
       <:delete< row in $table$ |
                 row.id = $int32:user_id$ >>

    let get dbh username =
      lwt res = Lwt_Query.query dbh
                                <:select< row |
                                          row in $table$;
                                          row.username = $string:username$ >> in
      match res with
       | [] -> Lwt.return None
       | hd :: _ -> Lwt.return (Some hd)

    let get_by_id dbh userid =
      lwt res = Lwt_Query.query dbh
                                <:select< row |
                                          row in $table$;
                                          row.id = $int32:userid$ >> in
      match res with
       | [] -> Lwt.return None
       | hd :: _ -> Lwt.return (Some hd)

    let insert username password =
      LangDb.full_transaction_block
        (fun dbh ->
           match_lwt (get dbh username) with
           | Some _ -> raise User_exists
           | None -> do_insert dbh username password)


    let get_existing_id dbh username =
      lwt res = get dbh username in
      match res with
         | None -> Lwt.fail User_not_found
         | Some u -> Lwt.return u#!id

    let check_password username password =
      LangDb.use_db
        (fun dbh ->
         match_lwt (get dbh username) with
         | Some u when verify_password password u#!password ->
            Lwt.return (Some u#!id)
         | _ -> Lwt.return None)

    let do_get_existing_user_from_id dbh id =
        lwt res = Lwt_Query.query dbh <:select< row |
                                                row in $table$;
                                                row.id = $int32:id$ >> in
      match res with
       | [] -> assert(false)
       | hd :: _ ->
          lwt lang_src = LangDb.find_lang hd#!preferred_lang_src in
          lwt lang_dst = LangDb.find_lang hd#!preferred_lang_dst in
          Lwt.return (hd#!username, hd#!password, hd#!id, lang_src, lang_dst)

    let get_existing_user_from_id id =
      LangDb.full_transaction_block
        (fun dbh ->
         lwt () = update_last_visit_date dbh id in
         do_get_existing_user_from_id dbh id)

end

module Translation = struct
    let id = <:sequence< serial "translations_id" >>
    (*  We use the correction_link and correction_state attributes
        to implement that link.

        We don't have enums in macaque, thus, we implement the following
        mapping
        constraint correction_link = -1 <-> correction_state=0.
        correction_link <> -1 ->
        -original: correction_state == 0
        -correction: correction_state == 1 -> correction_not_validated
                     correction_state == 2 -> correction_validated
        Table = [A=(id=0, correction_link=10, correction_state=0, ...),
                 ...
                 ACorrection=(id=10, correction_link=0, correction_state=2, ..),
                 ...] *)

    let table = <:table< translations (
                 id integer NOT NULL DEFAULT(nextval $id$),
                 l_word integer  NOT NULL,
                 r_word integer NOT NULL,
                 user_id integer NOT NULL,
                 description text NOT NULL,
                 correction_state integer NOT NULL,
                 correction_link integer NOT NULL) >>

    let get_id_and_description ?correction_state:(co_st=0l)
                               dbh lword_id rword_id user_id =
      let s = <:select< row |
                        row in $table$;
                        row.l_word = $int32:lword_id$;
                        row.r_word = $int32:rword_id$;
                        row.user_id = $int32:user_id$;
                        row.correction_state = $int32:co_st$; >> in
      match_lwt (Lwt_Query.query dbh s) with
        | [] -> Lwt.return None
        | hd :: _ -> Lwt.return (Some (hd#!id, hd#!description))

    let insert ?correction_state:(co_st=Int32.zero)
               ?link_to_correction:(co_link=(Int32.minus_one))
               dbh lword_id rword_id user_id description  =
      Lwt_Query.query dbh
       <:insert< $table$ := {description = $string:description$;
                             l_word = $int32:lword_id$;
                             r_word = $int32:rword_id$;
                             user_id = $int32:user_id$;
                             id = table?id;
                             correction_state = $int32:co_st$;
                             correction_link = $int32:co_link$} >>

    let update_description dbh synonym_id description =
      Lwt_Query.query dbh
       <:update< row in $table$ := {description = $string:description$} |
                 row.id = $int32:synonym_id$ >>

    let set ?description:(descr="") l_word r_word l_lang r_lang user_id =
      LangDb.full_transaction_block (fun dbh ->
        lwt l = Word.get dbh l_word l_lang in
        lwt r = Word.get dbh r_word r_lang in
        match_lwt (get_id_and_description dbh l.Word.id r.Word.id user_id) with
          | None -> begin
             lwt () = insert dbh l.Word.id r.Word.id user_id descr in
             (* xxx get the id without querying the db again *)
             match_lwt (get_id_and_description dbh l.Word.id r.Word.id user_id)
             with
             | None -> Lwt.return `Nok
             | Some (id, description) -> Lwt.return (`Ok (id, description))
            end
          | Some (id, description) ->
             if String.compare description descr == 0 then Lwt.return `Nok
             else begin
               lwt () = update_description dbh id descr in
               Lwt.return (`Ok (id, descr))
               end)

    let delete_linked_corrections dbh l_word r_word user_id =
      match_lwt (get_id_and_description
                 dbh
                 l_word.Word.id
                 r_word.Word.id
                 user_id) with
      | None -> Lwt.return_unit
      | Some (id, _) ->
         let query = <:delete< row in $table$ |
                               row.correction_link = $int32:id$ >> in
         Lwt_Query.query dbh query

    let unset ?original:(orig=true) l_word r_word l_lang r_lang user_id =
      LangDb.full_transaction_block (fun dbh ->
        lwt l_word = Word.get dbh l_word l_lang in
        lwt r_word = Word.get dbh r_word r_lang in
        lwt () = if orig then
                   delete_linked_corrections dbh l_word r_word user_id
                 else Lwt.return_unit in
        let query = <:delete< row in $table$ |
                              row.l_word = $int32:l_word.Word.id$;
                              row.r_word = $int32:r_word.Word.id$;
                              row.user_id = $int32:user_id$ >> in
        Lwt_Query.query dbh query)

    let get_element_by_id dbh id =
      let query = <:select< row |
                            row in $table$;
                            row.id = $int32:id$ >> in
      match_lwt (Lwt_Query.query dbh query) with
       | [] -> Lwt.fail (Failure "element does not exist anymore")
       | h :: _ -> Lwt.return h

    let ask_correction original_id corrector_id =
      LangDb.full_transaction_block (fun dbh ->
         lwt elt = get_element_by_id dbh original_id in
         if elt#!correction_state = 0l then
             match_lwt (User.get_by_id dbh corrector_id) with
           | None -> Lwt.fail (Failure "Corrector does not exist anymore")
           | Some _ ->
              lwt () = insert ~correction_state:1l
                              ~link_to_correction:original_id
                              dbh elt#!l_word elt#!r_word
                              corrector_id
                              elt#!description in
              match_lwt (get_id_and_description ~correction_state:1l
                                             dbh
                                             elt#!l_word
                                             elt#!r_word
                                             corrector_id) with
              | None -> Lwt.fail (Failure "could not insert the correction")
              | Some (id, _) ->
                 Lwt_Query.query dbh
                   <:update< row in $table$ := {correction_link = $int32:id$} |
                             row.id = $int32:original_id$ >>
         else Lwt.fail (Failure "A correction was created reload the page"))

    let validate_correction correction_id =
      LangDb.full_transaction_block (fun dbh ->
        Lwt_Query.query dbh
          <:update< row in $table$ := {correction_state = $int32:2l$} |
                    row.id = $int32:correction_id$ >>)

    let acknowledge_validated_correction ~implement_correction
                                         original_id
                                         correction_id =
      LangDb.full_transaction_block (fun dbh ->
        lwt original = get_element_by_id dbh original_id in
        lwt correction = get_element_by_id dbh correction_id in
        lwt () = Lwt_Query.query dbh
                   <:delete< row in $table$ |
                             row.id = $int32:correction_id$ >> in
        let descr = correction#!description in
        let query = if implement_correction then
         <:update< row in $table$ := {correction_link = $int32:Int32.minus_one$;
                                      correction_state = $int32:0l$;
                                      l_word = $int32:correction#!l_word$;
                                      r_word = $int32:correction#!r_word$;
                                      description = $string:descr$} |
                   row.id = $int32:original_id$>>
                    else
         <:update< row in $table$ := {correction_link = $int32:Int32.minus_one$;
                                      correction_state = $int32:0l$ } |
                   row.id = $int32:original_id$ >> in
        Lwt_Query.query dbh query)

    let get_translations user_id l_lang r_lang =
      LangDb.full_transaction_block (fun dbh ->
        lwt words_in_r_lang = Word.words_in_language r_lang in
        lwt words_in_l_lang = Word.words_in_language l_lang in
        (* let's find all the translations asked by the current user *)
        let translations = << synonym |
                              synonym in $table$;
                              synonym.user_id = $int32:user_id$;
                              synonym.correction_state = $int32:0l$ >> in
        (* all the corrections asked by the current user *)
        let linked = << synonym |
                        synonym in $table$;
                        chosen_translation in $translations$;
                        synonym.correction_state <> $int32:0l$;
                        synonym.correction_link = chosen_translation.id >> in
        (* all the corrections asked by another user to the current user *)
        let corrections = << synonym |
                             synonym in $table$;
                             synonym.user_id = $int32:user_id$;
                             synonym.correction_state <> $int32:0l$ >> in
        let corrections_links = << synonym |
                                   synonym in $table$;
                                   link in $corrections$;
                                   synonym.id = link.correction_link >> in
        let overall = << union $translations$ $linked$ >> in
        let overall = << union $corrections$ $overall$ >> in
        let overall = << union $corrections_links$ $overall$ >> in
        let query =
               <:select< {descr = t.description;
                          l_word = lw.word; r_word = rw.word;
                          correction_state = t.correction_state;
                          correction_link = t.correction_link;
                          id = t.id;
                          user_id = t.user_id } |
                          t in $overall$;
                          lw in $words_in_l_lang$;
                          rw in $words_in_r_lang$;
                          t.r_word = rw.id;
                          t.l_word = lw.id >>
        in
        lwt res = Lwt_Query.query dbh query in
        let open Utils.Translation in
        let to_data x =
            {id=x#!id;
             source=x#!l_word;
             dest=x#!r_word;
             description=x#!descr;
             owner=x#!user_id}
        in
        let to_correction x =
          let () = assert(x#!correction_state = 1l
                          || x#!correction_state = 2l) in
          let data = to_data x in
          {correction=data;
           corrected_id=x#!correction_link;
           validated=x#!correction_state = 2l}
        in
        let originals, corrections = List.partition
                                       (fun x ->
                                        x#!correction_state = Int32.zero)
                                       res in
        let originals = List.map to_data originals in
        let corrections = List.map to_correction corrections in
        let h = Hashtbl.create (List.length originals) in
        let () = List.iter (fun x ->
                            let new_x = {content=x;
                                         correction=None} in
                            Hashtbl.add h x.id new_x) originals
        in
        let () = List.iter (fun x ->
                            if Hashtbl.mem h x.corrected_id then
                              let previous = Hashtbl.find h x.corrected_id in
                              let new_x = {previous with correction = Some x} in
                              Hashtbl.replace h x.corrected_id new_x
                            else ()) corrections
        in
        let res = Hashtbl.fold (fun key value res -> value :: res) h [] in
        let () = Hashtbl.reset h in
        Lwt.return res)

    let do_delete_all_user_ids_translations dbh user_id =
      let query = <:delete< row in $table$ |
                   row.user_id = $int32:user_id$ >> in
      Lwt_Query.query dbh query

    let delete_user user_id =
      LangDb.full_transaction_block (fun dbh ->
        lwt () = do_delete_all_user_ids_translations dbh user_id in
        User.do_delete dbh user_id)

    let get_all_nouns_in_language lang =
      LangDb.full_transaction_block (fun dbh ->
        lwt words_in_lang = Word.words_in_language lang in
        let noun = "noun" in
        let nouns_in_lang = << {id = t.l_word} |
                                t in $table$;
                                t.description = $string:noun$;
                                lw in $words_in_lang$;
                                t.l_word = lw.id >> in
        let noun_names = Word.get_words nouns_in_lang in
        lwt res = Lwt_Query.query dbh noun_names in
        Lwt.return (List.map (fun x -> x#!word) res))
end
