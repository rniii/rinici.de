open Tyxml
open Lwt.Syntax

let layout ?title:(title_="~rini") ?head:(head_=[]) contents =
  Html.(html
    (head
      (title (txt title_))
      (head_ |> List.append [
        meta ~a:[ a_charset "utf-8" ] ();
        meta ~a:[ a_name "viewport"; a_content "width=device-width,initial-scale=1" ] ();
        meta ~a:[ a_name "theme-color"; a_content "#d895ee" ] ();
      ])
    )
    (body contents))

let home req =
  layout Html.[
    main [
      section [
        h1 [ txt "Welcome!" ];

        section [
          h2 [ txt "intra relay cat" ];
        ];
      ];
      nav [
        h2 [ txt "Links" ];
        p [ txt "Find me here!" ];
        ul ([
          ("https://codeberg.org/rini",   [ txt "codeberg.org/rini" ]);
          ("https://github.com/rniii",    [ txt "github.com/rniii" ]);
          ("https://ko-fi.com/rniii",     [ txt "ko-fi.com/rniii" ]);
          ("https://wetdry.world/@rini",  [ txt "@rini@wetdry.world" ]);
          ("mailto:rini%40rinici.de",     [ txt "rini"; span [ txt "@" ]; txt "rinici.de" ]);
        ] |> List.map (fun (href, c) -> li [ a ~a:[a_href href] c ]))
      ]
    ]
  ]

let () =
  Dream.run @@ Dream.logger @@ Dream.cookie_sessions
  @@ Dream.router
       [
         Dream.get "/" (fun req -> Template.home req |> Dream.html);
         Dream.get "/chat" (fun _ ->
             Dream.stream
               ~headers:[ ("Content-Type", Dream.text_html) ]
               Template.chat);
         Dream.post "/chat" (fun req ->
             let* form = Dream.form req in
             match form with
             | `Ok [ ("text", text) ] ->
                 Server.send_message (Some text);
                 Dream.empty `OK
             | _ -> Dream.empty `Bad_Request);
         Dream.get "/**" @@ Dream.static "public";
       ]
