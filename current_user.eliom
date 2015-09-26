{shared{

type t = {
    username: string;
    password: string;
    id: Int32.t;
    preferred_lang: string;
}

let create username password id preferred_lang = {
    username; password; id; preferred_lang
  }

let get_user_id t = t.id
let get_preferred_lang t = t.preferred_lang
}}
