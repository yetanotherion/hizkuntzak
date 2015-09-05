
let do_as_user user =
  let print_trans () =
    lwt res = Db.Translation.get_translations "etxea" "eus" "en" user in
    let () = List.iter (fun x -> Printf.printf "en: %s, descr: %s\n" x.Db.Translation.translation x.Db.Translation.description) res in
    Lwt.return_unit
  in
  lwt () = Db.Translation.set "etxea" "eus" "house" "en" user in
  lwt () = Db.Translation.set "etxea" "eus" "flat" "en" ~description:"this is an appartment" user in
  lwt () = Db.Translation.set "etxea" "eus" "erroneous" "en" ~description:"this should be deleted" user in
  lwt () = Db.Translation.set "etxea" "eus" "maison" "fr" user in
  let () = Printf.printf "before delete\n" in
  lwt () = print_trans () in
  let () = Printf.printf "after delete\n" in
  lwt () = Db.Translation.unset "etxea" "eus" "erroneous" "en" user in
  lwt () = print_trans () in
  Lwt.return_unit

lwt () =
  lwt id = Db.LangDb.find "eus" in
  let () = Printf.printf "%ld\n" id in
  lwt () = Db.Translation.delete_user "bonnie" in
  lwt () = Db.Translation.delete_user "clyde" in
  lwt () = Db.User.insert "bonnie" "p1" in
  lwt () = Db.User.insert "clyde" "p2" in
  lwt () = do_as_user "bonnie" in
  lwt res = Db.Translation.get_translations "etxea" "eus" "en" "clyde" in
  let () = assert (List.length res = 0) in
  Lwt.return_unit
