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



module LangDb: (sig
                   val full_transaction_block: ((string, bool) Hashtbl.t Lwt_PGOCaml.t -> 'a Lwt.t) -> 'a Lwt.t
                   val use_db : ((string, bool) Hashtbl.t Lwt_PGOCaml.t -> 'a Lwt.t) -> 'a Lwt.t
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
       <:update< row in $table$ := {preferred_lang_src = $int32:preferred_lang$} |
                 row.id = $int32:id$ >>

    let do_update_preferred_lang_dst dbh id lang =
      lwt preferred_lang = LangDb.find lang in
      Lwt_Query.query dbh
       <:update< row in $table$ := {preferred_lang_dst = $int32:preferred_lang$} |
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
      lwt res = Lwt_Query.query dbh <:select< row |
                                              row in $table$;
                                              row.username = $string:username$ >> in
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
    let table = <:table< translations (
                 id integer NOT NULL DEFAULT(nextval $id$),
                 l_word integer  NOT NULL,
                 r_word integer NOT NULL,
                 user_id integer NOT NULL,
                 description text NOT NULL) >>

    type t = {
        id: Int32.t;
        l_word: Word.t;
        r_word: Word.t;
        description: string;
      }

    let get_id_and_description dbh lword_id rword_id user_id =
      let s = <:select< row |
                        row in $table$;
                        row.l_word = $int32:lword_id$;
                        row.r_word = $int32:rword_id$;
                        row.user_id = $int32:user_id$>> in
      match_lwt (Lwt_Query.query dbh s) with
        | [] -> Lwt.return None
        | hd :: _ -> Lwt.return (Some (hd#!id, hd#!description))

    let insert dbh lword_id rword_id user_id description =
      Lwt_Query.query dbh
       <:insert< $table$ := {description = $string:description$;
                             l_word = $int32:lword_id$;
                             r_word = $int32:rword_id$;
                             user_id = $int32:user_id$;
                             id = table?id}>>

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
             Lwt.return true
            end
          | Some (id, description) ->
             if String.compare description descr == 0 then Lwt.return false
             else begin
               lwt () = update_description dbh id descr in
               Lwt.return true
               end)

    let unset l_word r_word l_lang r_lang user_id =
      LangDb.full_transaction_block (fun dbh ->
        lwt l_word = Word.get dbh l_word l_lang in
        lwt r_word = Word.get dbh r_word r_lang in
        let query = <:delete< row in $table$ |
                              row.l_word = $int32:l_word.Word.id$;
                              row.r_word = $int32:r_word.Word.id$;
                              row.user_id = $int32:user_id$ >> in
        Lwt_Query.query dbh query)

    let get_translations ?l_word:(l_word=None) user_id l_lang r_lang =
      LangDb.full_transaction_block (fun dbh ->
        lwt words_in_r_lang = Word.words_in_language r_lang in
        lwt words_in_l_lang = Word.words_in_language l_lang in
        lwt query = match l_word with
          | Some word -> begin
             lwt l_word = Word.get dbh word l_lang in
             let translations = << synonym |
                                   synonym in $table$;
                                   synonym.l_word = $int32:l_word.Word.id$;
                                   synonym.user_id = $int32:user_id$ >> in
             Lwt.return
               <:select< {descr = t.description;
                          l_word = lw.word; r_word = rw.word} |
                          t in $translations$;
                          lw in $words_in_l_lang$;
                          rw in $words_in_r_lang$;
                          t.r_word = rw.id;
                          t.l_word = lw.id >>
            end
          | None -> begin
             let translations = << synonym |
                                   synonym in $table$;
                                   synonym.user_id = $int32:user_id$ >> in
             Lwt.return
               <:select< {descr = t.description;
                          l_word = lw.word; r_word = rw.word} |
                          t in $translations$;
                          lw in $words_in_l_lang$;
                          rw in $words_in_r_lang$;
                          t.r_word = rw.id;
                          t.l_word = lw.id >>
            end
        in
        lwt res = Lwt_Query.query dbh query in
        let make_res x = Utils.Translation.({source=x#!l_word;
                                             dest=x#!r_word;
                                             description=x#!descr}) in
        Lwt.return (List.map make_res res))

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
