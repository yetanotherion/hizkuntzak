{shared{
     open Eliom_content
     open Eliom_parameter
}}

let get_current_user_id () =
  let uids = Eliom_state.get_volatile_data_session_group () in
  let get_uid uid =
    try Eliom_lib.Option.map Int32.of_string uid
    with Failure _ -> None
  in
  match get_uid uids with
    | None -> begin
      lwt uid = Eliom_state.get_persistent_data_session_group () in
      match get_uid uid  with
       | Some uid ->
         (* A persistent session exists, but the volatile session has gone.
            It may be due to a timeout or may be the server has been
            relaunched.
            We restart the volatile session silently *)
          let () = Eliom_state.set_volatile_data_session_group
                     ~scope:Eliom_common.default_session_scope (Int32.to_string uid) in

         Lwt.return (Some uid)
       | None -> Lwt.return None
      end
    | Some uid -> Lwt.return (Some uid)

let get_user_of_id id =
  lwt (u, p, id, src_lang, dst_lang) = Db.User.get_existing_user_from_id id in
  Lwt.return (Current_user.create u p id src_lang dst_lang)

let get_current_user () =
  match_lwt get_current_user_id () with
  | None -> Lwt.return None
  | Some id ->
     lwt (u, p, id, src_lang, dst_lang) = Db.User.get_existing_user_from_id id in
     Lwt.return (Some (Current_user.create u p id src_lang dst_lang))

let connect uid =
  lwt () = Eliom_state.set_persistent_data_session_group
    ~scope:Eliom_common.default_session_scope uid in
  let () = Eliom_state.set_volatile_data_session_group
    ~scope:Eliom_common.default_session_scope uid in
  Lwt.return_unit

let disconnect () =
  Eliom_state.discard ~scope:Eliom_common.default_session_scope ()

{shared{
  exception User_exists
}}

let rpc_connect =
  server_function Json.t<string> connect

let rpc_disconnect =
  server_function Json.t<unit> disconnect

let rpc_get_current_user =
  server_function Json.t<unit> get_current_user

let rpc_get_user_of_id =
  server_function Json.t<int32> get_user_of_id

let rpc_check_password =
  let check_password (username, password) =
    Db.User.check_password username password
  in
  server_function Json.t<string * string> check_password

let rpc_create_account =
  let create_account (username, password) =
    try_lwt
      lwt () = Db.User.insert username password in
      Lwt.return true
    with Db.User.User_exists -> Lwt.return false
  in
  server_function Json.t<string * string> create_account

{client{

let get_current_user = %rpc_get_current_user

let login f u p =
   lwt newm =
     match_lwt %rpc_check_password (u, p) with
     | None -> Lwt.return (`Login (`Error "Invalid login"))
     | Some id -> begin
         lwt () = %rpc_connect (Int32.to_string id) in
         lwt user = %rpc_get_user_of_id id in
         Lwt.return (`Logged user)
       end
   in
   let () = f newm in
   Lwt.return_unit

let create_account f u p r =
  lwt newm =
    if String.compare p r <> 0 then (
      let () = Utils.log (Printf.sprintf "%s != %s" p r) in
      Lwt.return (`CreateAccount (`Error ("passwords don't match", Some u)))
    )
    else begin
      match_lwt %rpc_create_account (u, p) with
      | true -> Lwt.return (`CreateAccount (`AccountCreated (Printf.sprintf "account %s created" u)))
      | false -> Lwt.return (`CreateAccount (`Error (Printf.sprintf "User %s already exists" u, None)))
      end in
  let () = f newm in
  Lwt.return_unit

let logout f =
  lwt () = %rpc_disconnect () in
  let () = f (`Login `Unit) in
  Lwt.return_unit

let goto_login f =
  let () = f (`Login `Unit) in
  Lwt.return_unit

let goto_create_account f =
  let () = f (`CreateAccount `Unit) in
  Lwt.return_unit
}}
