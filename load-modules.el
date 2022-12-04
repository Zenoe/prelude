;; -*- lexical-binding: t; -*-

(let ( (zo-modules '(
                     "zo-lib"
                     "zo-ui"
                     "zo-edit"
                     "zo-tool"
                     "prelude-programming" ;; must load before other programming language
                     "prelude-company"
                     "prelude-org"
                     "prelude-c"
                     "prelude-css"
                     "prelude-lisp"
                     "prelude-perl"
                     "prelude-shell"
                     "prelude-web"
                     "prelude-xml"
                     "prelude-yaml"
                     "zo-evil"
                     "zo-project"
                     "zo-company"
                     "zo-persp-mode"
                     "prelude-erc"
                     "completion/config"
                     "workspaces/config"
                     "lookup/config"
                     "zo-keybinding"
                     )
                   )
       )
  (zo/load zo-modules prelude-modules-dir)

  )
