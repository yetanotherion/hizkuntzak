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
                 end) = struct

 (* a new language can only be added out of the application.
    The application must be rebooted so that it is taken
    into account *)

    module StringHash = struct
        type t = string
        let equal i j = i = j
        let hash = Hashtbl.hash
      end

    module StringHashtbl = Hashtbl.Make (StringHash)

    type t = {
        lang: Int32.t StringHashtbl.t;
        pool: (string, bool) Hashtbl.t Lwt_PGOCaml.t Lwt_pool.t;
      }

    let table = <:table< languages (
                 id integer NOT NULL,
                 lang text NOT NULL) >>

    let load_table dbh =
      lwt res = Lwt_Query.query dbh <:select< row | row in $table$>> in
      let h = StringHashtbl.create (List.length res) in
      let () = List.iter (fun x -> StringHashtbl.add h x#!lang x#!id) res in
      Lwt.return h

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
      lwt lang = Lwt_pool.use pool load_table in
      Lwt.return {pool=pool;
                  lang=lang}

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

    let find lang =
      lwt t = get () in
      if StringHashtbl.mem t.lang lang then
        Lwt.return (StringHashtbl.find t.lang lang)
      else begin
        let supported = StringHashtbl.fold (fun l _ accum -> l :: accum) t.lang [] in
        let msg = Printf.sprintf "unknown language: %s (supported: %s)" lang (String.concat "," supported) in
        Lwt.fail (Failure msg)
        end

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

module User = struct
    exception User_not_found
    exception User_exists

    let id = <:sequence< serial "users_id" >>
    let table = <:table< users (
                 id integer NOT NULL DEFAULT(nextval $id$),
                 username text NOT NULL,
                 password text NOT NULL) >>

    let do_insert dbh username password =
      Lwt_Query.query dbh
       <:insert< $table$ := {username = $string:username$;
                             password = $string:password$;
                             id = table?id}>>
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

    let get_id dbh lword_id rword_id user_id =
      let s = <:select< row |
                        row in $table$;
                        row.l_word = $int32:lword_id$;
                        row.r_word = $int32:rword_id$;
                        row.user_id = $int32:user_id$>> in
      match_lwt (Lwt_Query.query dbh s) with
        | [] -> Lwt.return None
        | hd :: _ -> Lwt.return (Some hd#!id)

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

    let set ?description:(descr="") l_word l_lang r_word r_lang username =
      LangDb.full_transaction_block (fun dbh ->
        lwt user_id = User.get_existing_id dbh username in
        lwt l = Word.get dbh l_word l_lang in
        lwt r = Word.get dbh r_word r_lang in
        match_lwt (get_id dbh l.Word.id r.Word.id user_id) with
          | None -> insert dbh l.Word.id r.Word.id user_id descr
          | Some id -> update_description dbh id descr)

    let unset l_word l_lang r_word r_lang username =
      LangDb.full_transaction_block (fun dbh ->
        lwt user_id = User.get_existing_id dbh username in
        lwt l_word = Word.get dbh l_word l_lang in
        lwt r_word = Word.get dbh r_word r_lang in
        let query = <:delete< row in $table$ |
                              row.l_word = $int32:l_word.Word.id$;
                              row.r_word = $int32:r_word.Word.id$;
                              row.user_id = $int32:user_id$ >> in
        Lwt_Query.query dbh query)


    type translation_res = {
        translation: string;
        description: string;
      }

    let get_translations l_word l_lang r_lang username =
      LangDb.full_transaction_block (fun dbh ->
        lwt user_id = User.get_existing_id dbh username in
        lwt l_word = Word.get dbh l_word l_lang in
        lwt words_in_lang = Word.words_in_language r_lang in
        let translations = << synonym |
                              synonym in $table$;
                              synonym.l_word = $int32:l_word.Word.id$;
                              synonym.user_id = $int32:user_id$ >> in
        let query = <:select< {descr = t.description; word = w.word} |
                               t in $translations$;
                               w in $words_in_lang$;
                               t.r_word = w.id >> in
        lwt res = Lwt_Query.query dbh query in
        Lwt.return (List.map (fun x -> {translation=x#!word;
                                        description=x#!descr}) res))

    let do_delete_all_user_ids_translations dbh user_id =
      let query = <:delete< row in $table$ |
                   row.user_id = $int32:user_id$ >> in
      Lwt_Query.query dbh query

    let delete_user username =
      LangDb.full_transaction_block (fun dbh ->
        lwt user_id = User.get_existing_id dbh username in
        lwt () = do_delete_all_user_ids_translations dbh user_id in
        User.do_delete dbh user_id)
  end
