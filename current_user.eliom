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
}}
