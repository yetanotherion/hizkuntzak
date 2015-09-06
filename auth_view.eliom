{client{
  open Eliom_content
  open Html5.D

  let username_password_button_error onclick button =
    let username = string_input ~input_type:`Text () in
    let password = string_input ~input_type:`Text () in
    let text_holder = div [] in
    let reset_input i = (Html5.To_dom.of_input i) ## value <- Js.string "" in
    let write_msg msg =
      let () = Html5.Manip.replaceChildren text_holder
                                           [pcdata msg] in
      Lwt.return_unit
    in
    let onclick _ _ =
      let () = Html5.Manip.replaceChildren text_holder [] in
      lwt res = onclick write_msg username password in
      let () = reset_input username in
      let () = reset_input password in
      Lwt.return_unit
      in
    let open Lwt_js_events in
    async (fun () -> clicks
                       (Html5.To_dom.of_button button)
                       onclick);
    username, password, text_holder

  let ui_elements on_action_click on_go_to_click action_button_name go_to_button_name =
    let action_button = button ~a:[a_class ["btn"; "btn-lg"; "btn-primary"]] ~button_type:`Button
                               [pcdata action_button_name] in
    let go_to_button = button ~a:[a_class ["btn"; "btn-lg"; "btn-warning"]] ~button_type:`Button
                              [pcdata go_to_button_name] in
    let username, password, text_holder = username_password_button_error on_action_click action_button in
    let open Lwt_js_events in
    let () = async (fun () -> clicks
                             (Html5.To_dom.of_button go_to_button)
                             (fun _ _ -> on_go_to_click ())) in
    [username; password;
     action_button; go_to_button;
     text_holder]

  let login_elements on_login_click on_go_to_create_click =
    ui_elements on_login_click on_go_to_create_click "Login" "Go to create account"

  let create_account_elements on_create_account_click on_go_to_login_click =
    ui_elements on_create_account_click on_go_to_login_click "Create account" "Go to login"

  let connected_elements on_logout_click uid =
    let logout = button ~a:[a_class ["btn"; "btn-lg"; "btn-primary"]] ~button_type:`Button
                        [pcdata "Logout"] in
    let open Lwt_js_events in
    let () = async (fun () -> clicks
                             (Html5.To_dom.of_button logout)
                             (fun _ _ -> on_logout_click ())) in
    [pcdata (Printf.sprintf "Welcome %ld" uid); logout]
}}
