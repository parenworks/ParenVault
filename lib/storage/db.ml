(** Database connection management for offline-first architecture.
    
    Uses SQLite for local storage and PostgreSQL for remote sync.
*)

open Lwt.Syntax

(** Connection pool type - using Caqti_lwt_unix for pool management *)
type pool = (Caqti_lwt.connection, Caqti_error.t) Caqti_lwt_unix.Pool.t

(** Database configuration *)
type config = {
  local_path: string;           (** Path to local SQLite database *)
  remote_uri: string option;    (** PostgreSQL URI over Tailscale *)
}

(** Create a connection pool *)
let create_pool uri =
  let pool_config = Caqti_pool_config.create ~max_size:10 () in
  Caqti_lwt_unix.connect_pool ~pool_config (Uri.of_string uri)

(** Create local SQLite pool *)
let create_local_pool ~path =
  let uri = Printf.sprintf "sqlite3://%s" path in
  create_pool uri

(** Create remote PostgreSQL pool *)
let create_remote_pool ~uri =
  create_pool uri

(** Execute a query with a pool *)
let with_pool pool f =
  Caqti_lwt_unix.Pool.use f pool

(** Check if remote database is reachable *)
let check_remote_connection pool =
  let query = Caqti_request.Infix.(Caqti_type.unit ->! Caqti_type.int) "SELECT 1" in
  let* result = with_pool pool (fun (module C : Caqti_lwt.CONNECTION) ->
    C.find query ()
  ) in
  match result with
  | Ok 1 -> Lwt.return true
  | Ok _ -> Lwt.return true
  | Error _ -> Lwt.return false

(** Database context holding both local and remote connections *)
type context = {
  local: pool;
  remote: pool option;
  mutable remote_available: bool;
}

(** Initialize database context *)
let init ~local_path ?remote_uri () =
  match create_local_pool ~path:local_path with
  | Error err -> Lwt.return (Error (Caqti_error.show err))
  | Ok local ->
    let* remote, remote_available = 
      match remote_uri with
      | None -> Lwt.return (None, false)
      | Some uri ->
        match create_remote_pool ~uri with
        | Error _ -> Lwt.return (None, false)
        | Ok pool ->
          let* available = check_remote_connection pool in
          Lwt.return (Some pool, available)
    in
    Lwt.return (Ok { local; remote; remote_available })

(** Get the local pool *)
let local ctx = ctx.local

(** Get the remote pool if available *)
let remote ctx = 
  if ctx.remote_available then ctx.remote else None

(** Refresh remote connection status *)
let refresh_remote_status ctx =
  match ctx.remote with
  | None -> Lwt.return false
  | Some pool ->
    let* available = check_remote_connection pool in
    ctx.remote_available <- available;
    Lwt.return available
