{shared{

type t = {
    username: string;
    password: string;
    id: Int32.t;
    preferred_lang_src: string;
    preferred_lang_dst: string;
}

let create username password id preferred_lang_src preferred_lang_dst = {
    username; password; id; preferred_lang_src; preferred_lang_dst
  }

let get_user_id t = t.id
let get_preferred_lang_src t = t.preferred_lang_src
let get_preferred_lang_dst t = t.preferred_lang_dst
}}
