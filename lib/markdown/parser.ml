(** Markdown parsing with task metadata extraction.
    
    Supports extended syntax for tasks:
    - [ ] Task title @due(2026-01-25) @priority(high) @tag(work)
*)

open Domain.Types

(** Parsed task from markdown *)
type parsed_task = {
  title: string;
  completed: bool;
  priority: priority option;
  due_date: string option;
  tags: string list;
  description: string option;
}

(** Attribute patterns *)
let due_pattern = Str.regexp "@due(\\([^)]+\\))"
let priority_pattern = Str.regexp "@priority(\\([^)]+\\))"
let tag_pattern = Str.regexp "@tag(\\([^)]+\\))"

(** Extract all matches of a pattern *)
let extract_all pattern text =
  let rec find acc start =
    try
      let _ = Str.search_forward pattern text start in
      let matched = Str.matched_group 1 text in
      find (matched :: acc) (Str.match_end ())
    with Not_found -> List.rev acc
  in
  find [] 0

(** Remove all attribute annotations from text *)
let strip_attributes text =
  let text = Str.global_replace due_pattern "" text in
  let text = Str.global_replace priority_pattern "" text in
  let text = Str.global_replace tag_pattern "" text in
  String.trim text

(** Parse a single task line *)
let parse_task_line line =
  (* Check for checkbox pattern *)
  let checkbox_pattern = Str.regexp "^\\(- \\)?\\[\\([ xX]\\)\\] \\(.*\\)$" in
  if Str.string_match checkbox_pattern line 0 then
    let completed = 
      let mark = Str.matched_group 2 line in
      mark = "x" || mark = "X"
    in
    let rest = Str.matched_group 3 line in
    (* Extract attributes *)
    let due_date = 
      match extract_all due_pattern rest with
      | d :: _ -> Some d
      | [] -> None
    in
    let priority =
      match extract_all priority_pattern rest with
      | p :: _ -> priority_of_string p
      | [] -> None
    in
    let tags = extract_all tag_pattern rest in
    let title = strip_attributes rest in
    Some { title; completed; priority; due_date; tags; description = None }
  else
    None

(** Parse markdown content and extract tasks *)
let extract_tasks content =
  let lines = String.split_on_char '\n' content in
  List.filter_map parse_task_line lines

(** Convert a task to markdown checkbox format *)
let task_to_markdown (task : Domain.Types.task) =
  let checkbox = if task.status = Domain.Types.Done then "[x]" else "[ ]" in
  let priority_attr = 
    Printf.sprintf "@priority(%s)" (priority_to_string task.priority)
  in
  let due_attr = match task.due_date with
    | Some d -> 
      let (y, m, day) = Ptime.to_date d.time in
      Printf.sprintf " @due(%04d-%02d-%02d)" y m day
    | None -> ""
  in
  let tag_attrs = 
    task.tags 
    |> List.map (Printf.sprintf "@tag(%s)")
    |> String.concat " "
  in
  Printf.sprintf "- %s %s %s%s %s" 
    checkbox task.title priority_attr due_attr tag_attrs
  |> String.trim

(** Parse a full markdown document *)
let parse_document content =
  Omd.of_string content

(** Render markdown AST back to string *)
let render_document doc =
  Omd.to_sexp doc  (* omd 2.x uses different API - this is placeholder *)

(** Extract title from markdown (first H1) *)
let extract_title content =
  let doc = parse_document content in
  let rec find_h1 = function
    | [] -> None
    | Omd.Heading (_, 1, Omd.Text (_, text)) :: _ -> Some text
    | _ :: rest -> find_h1 rest
  in
  find_h1 doc

(** Create a note document with frontmatter-style metadata *)
let create_note_document ~title ~tags ~content =
  let tag_line = if tags = [] then "" else
    Printf.sprintf "Tags: %s\n" (String.concat ", " (List.map (fun t -> "#" ^ t) tags))
  in
  Printf.sprintf "# %s\n\n%s\n%s" title tag_line content
