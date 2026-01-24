(lang dune 3.14)

(name parenvault)

(generate_opam_files true)

(source
 (github glenn/parenvault))

(authors "Glenn")

(maintainers "Glenn")

(license proprietary)

(documentation https://github.com/glenn/parenvault)

(package
 (name parenvault)
 (synopsis "Personal Knowledge Management TUI")
 (description "Offline-first PKM system with PostgreSQL sync over Tailscale and aerc integration")
 (depends
  (ocaml (>= 5.1))
  ; TUI
  (notty (>= 0.2))
  (notty-lwt (>= 0.2))
  ; Database
  (caqti (>= 2.1))
  (caqti-driver-postgresql (>= 2.1))
  (caqti-driver-sqlite3 (>= 2.1))
  (caqti-lwt (>= 2.1))
  ; Async
  (lwt (>= 5.7))
  (lwt_ppx (>= 2.1))
  ; Markdown parsing
  (omd (>= 2.0.0~alpha4))
  ; JSON
  (yojson (>= 2.1))
  (ppx_yojson_conv (>= 0.16))
  ; Date/Time
  (ptime (>= 1.1))
  (timedesc (>= 3.0))
  ; CLI
  (cmdliner (>= 1.2))
  ; Config
  (toml (>= 7.1))
  ; UUID
  (uuidm (>= 0.9))
  ; File watching (for aerc maildir)
  (inotify (>= 2.6))
  ; Email parsing
  (mrmime (>= 0.6))
  ; Crypto (for sync)
  (digestif (>= 1.1))
  ; Testing
  (alcotest :with-test)
  (alcotest-lwt :with-test))
 (tags
  (pkm tui postgresql sync aerc)))
