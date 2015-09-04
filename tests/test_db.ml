lwt () =
  let print_trans () =
    lwt res = Db.Translation.get_translations "etxea" "eus" "en" in
    let () = List.iter (fun x -> Printf.printf "en: %s, descr: %s\n" x.Db.Translation.translation x.Db.Translation.description) res in
    Lwt.return_unit
  in
  lwt id = Db.LangDb.find "eus" in
  let () = Printf.printf "%ld\n" id in
  lwt () = Db.Translation.set "etxea" "eus" "house" "en" in
  lwt () = Db.Translation.set "etxea" "eus" "flat" "en" ~description:"this is an appartment" in
  lwt () = Db.Translation.set "etxea" "eus" "erroneous" "en" ~description:"this should be deleted" in
  lwt () = Db.Translation.set "etxea" "eus" "maison" "fr" in
  let () = Printf.printf "before delete\n" in
  lwt () = print_trans () in
  let () = Printf.printf "after delete\n" in
  lwt () = Db.Translation.unset "etxea" "eus" "erroneous" "en" in
  lwt () = print_trans () in
  Lwt.return_unit
