module Lwt_thread = struct
  include Lwt
  include Lwt_chan
end
module Lwt_PGOCaml = PGOCaml_generic.Make(Lwt_thread)
module Lwt_Query = Query.Make_with_Db(Lwt_thread)(Lwt_PGOCaml)

let join delim l =
  let res = List.fold_left (fun accum res -> accum ^ res ^ delim) "" l in
  String.sub res 0 (String.length res - 1)

let must x =
  match x with
  | None -> assert(false)
  | Some x -> x



module LangDb = struct

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
        db: unit Lwt_PGOCaml.t;
      }

    let table = <:table< languages (
                 id integer NOT NULL,
                 lang text NOT NULL) >>

    let load_table dbh =
      lwt res = Lwt_Query.query dbh <:select< row | row in $table$>> in
      let h = StringHashtbl.create (List.length res) in
      let () = List.iter (fun x -> StringHashtbl.add h x#!lang x#!id) res in
      Lwt.return h

    let handler = ref None

    let connect_to_db () =
      try_lwt
        Lwt_PGOCaml.connect ~database:"hizkuntzak" ()
      with _ ->
        Lwt_PGOCaml.connect ~host:"127.0.0.1"
                            ~user:"postgres"
                            ~password:"postgres"
                            ~port:5432
                            ~database:"hizkuntzak" ()

    let create () =
      lwt db = connect_to_db () in
      lwt table = load_table db in
      Lwt.return {db=db;
                  lang=table}

    let get () =
      match !handler with
      | Some h -> Lwt.return h
      | None -> begin
          lwt t = create () in
          handler := Some t;
          Lwt.return t
        end

    let find lang =
      lwt t = get () in
      let h = t.lang in
      if StringHashtbl.mem h lang then Lwt.return (StringHashtbl.find h lang)
      else begin
        let supported = StringHashtbl.fold (fun l _ accum -> l :: accum) h [] in
        let msg = Printf.sprintf "unknown language: %s (supported: %s)" lang (join "," supported) in
        Lwt.fail (Failure msg)
        end

    let get_db () =
      lwt res = get () in
      Lwt.return res.db

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

    let get_word_id word language_id =
      lwt dbh = LangDb.get_db () in
      match_lwt (Lwt_Query.query dbh <:select< row |
                                               row in $table$;
                                               row.word = $string:word$;
                                               row.lang = $int32:language_id$>>)
      with
      | [] -> Lwt.return None
      | hd :: _ -> Lwt.return (Some hd#!id)

    let _insert_word word lang =
      lwt dbh = LangDb.get_db () in
      Lwt_Query.query dbh
       <:insert< $table$ := {word = $string:word$;
                             lang = $int32:lang$;
                             id = table?id}>>
    let insert_word word lang =
      lwt language_id = LangDb.find lang in
      match_lwt (get_word_id word language_id) with
        | Some hd -> Lwt.return hd
        | None -> begin
            lwt () = _insert_word word language_id in
            lwt word_id = get_word_id word language_id in
            Lwt.return (must word_id)
          end

    type t = {
        id: Int32.t;
        word: string;
        language: string;
      }

    let get word lang =
      (* get the id of the word "word" associated to a language "lang".
         Note that this method inserts the word associated to language
         "lang" if it does not exist *)
      lwt id = insert_word word lang in
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

    let do_insert username password =
      lwt dbh = LangDb.get_db () in
      Lwt_Query.query dbh
       <:insert< $table$ := {username = $string:username$;
                             password = $string:password$;
                             id = table?id}>>
    let get username =
      lwt dbh = LangDb.get_db () in
      lwt res = Lwt_Query.query dbh <:select< row |
                                              row in $table$;
                                              row.username = $string:username$ >> in
      match res with
       | [] -> Lwt.return None
       | hd :: _ -> Lwt.return (Some hd)

    let insert username password =
      match_lwt (get username) with
      | Some _ -> raise User_exists
      | None -> do_insert username password

    let get_existing_id username =
      lwt res = get username in
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

    let get_id lword_id rword_id user_id =
      lwt dbh = LangDb.get_db () in
      let s = <:select< row |
                        row in $table$;
                        row.l_word = $int32:lword_id$;
                        row.r_word = $int32:rword_id$;
                        row.user_id = $int32:user_id$>> in
      match_lwt (Lwt_Query.query dbh s) with
        | [] -> Lwt.return None
        | hd :: _ -> Lwt.return (Some hd#!id)

    let insert lword_id rword_id user_id description =
      lwt dbh = LangDb.get_db () in
      Lwt_Query.query dbh
       <:insert< $table$ := {description = $string:description$;
                             l_word = $int32:lword_id$;
                             r_word = $int32:rword_id$;
                             user_id = $int32:user_id$;
                             id = table?id}>>

    let update_description synonym_id description =
      lwt dbh = LangDb.get_db () in
      Lwt_Query.query dbh
       <:update< row in $table$ := {description = $string:description$} |
                 row.id = $int32:synonym_id$ >>

    let set ?description:(descr="") l_word l_lang r_word r_lang username =
      lwt user_id = User.get_existing_id username in
      lwt l = Word.get l_word l_lang in
      lwt r = Word.get r_word r_lang in
      match_lwt (get_id l.Word.id r.Word.id user_id) with
        | None -> insert l.Word.id r.Word.id user_id descr
        | Some id -> update_description id descr

    let unset l_word l_lang r_word r_lang username =
      lwt user_id = User.get_existing_id username in
      lwt dbh = LangDb.get_db () in
      lwt l_word = Word.get l_word l_lang in
      lwt r_word = Word.get r_word r_lang in
      let query = <:delete< row in $table$ |
                            row.l_word = $int32:l_word.Word.id$;
                            row.r_word = $int32:r_word.Word.id$;
                            row.user_id = $int32:user_id$ >> in
      Lwt_Query.query dbh query


    type translation_res = {
        translation: string;
        description: string;
      }

    let get_translations l_word l_lang r_lang username =
      lwt user_id = User.get_existing_id username in
      lwt dbh = LangDb.get_db () in
      lwt l_word = Word.get l_word l_lang in
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
                                      description=x#!descr}) res)

  end
