{shared{
     open Eliom_content
     open Eliom_parameter
     open Html5.D
}}

let get_current_user () =
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

  type state = [`Login | `Create_account ]
  type 'a div_type = 'a constraint [< Html5_types.div ] = 'a

  type 'a t = {
      div: 'a div_type Html5.elt;
      mutable state: state;
    }
  let create div =
    {div=div;
     state=`Login}

  let input_value i = Js.to_string (Html5.To_dom.of_input i) ## value

  let on_login_click login_ok write_msg username password =
    let u, p = input_value username, input_value password in
    match_lwt (%rpc_check_password (u,p)) with
    | None -> write_msg "Invalid login"
    | Some id -> lwt () = %rpc_connect (Int32.to_string id) in
                 login_ok ()

  let on_create_account_click write_msg username password =
    let u, p = input_value username, input_value password in
    match_lwt %rpc_create_account (u,p) with
    | true -> write_msg (Printf.sprintf "Account %s created" u)
    | false -> write_msg (Printf.sprintf "User %s already exists" u)

  let on_logout_click reload =
    lwt () = %rpc_disconnect () in
    reload ()

  let on_go_to_create_account_click t reload =
    let () = t.state <- `Create_account in
    reload ()

  let on_go_to_login_click t reload =
    let () = t.state <- `Login in
    reload ()

  let rec reload t =
    let div = t.div in
    let r_fun () = reload t in
    match_lwt (%rpc_get_current_user ()) with
    | None -> begin
       match t.state with
       | `Login -> begin
          let ui = Auth_view.login_elements
                        (on_login_click r_fun)
                        (fun () -> on_go_to_create_account_click t r_fun) in
          let () = Html5.Manip.replaceChildren div ui in
          Lwt.return_unit
         end
       | `Create_account -> begin
           let ui = Auth_view.create_account_elements
                        (on_create_account_click)
                        (fun () -> on_go_to_login_click t r_fun) in
          let () = Html5.Manip.replaceChildren div ui in
          Lwt.return_unit
         end
      end
    | Some uid ->
       let ui = Auth_view.connected_elements (fun () -> on_logout_click r_fun) uid in
       let () = Html5.Manip.replaceChildren div ui in
       Lwt.return_unit

  let rec setup div =
    Lwt_js_events.async (fun () -> reload (create div))
}}

{server{
let ui_box () =
  let div_element = div [] in
  let _ = {unit{ setup %div_element }} in
  div_element
}}
