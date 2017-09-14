(* compile with ocamlbuild -pkgs pcre,batteries pastrhymes.native *)

let fold = Mu.fold
let map = Mu.map
let cons = Mu.cons
let revh tl h = fold cons tl h
let rev l = revh [] l
let drop n l = Mu.rec_n n List.tl l
let hd = List.hd and tl = List.tl 
let identity x = x

module M = MuMapP

let to_set lyst = fold (fun el mp -> M.add el true mp) M.empty lyst
let set_union a b = fold (fun (el, _) mp -> M.add el true mp) b (M.bindings a)
let set_mem k a = try M.find k a ; true with Not_found -> false 
let set_add k a = M.add k true a
let set_list a = map fst (M.bindings a)
let invert_map a = M.fold (fun k v m -> M.addf v (cons k) [] m) a M.empty
let mset_add k a = M.addf k ((+) 1) 0 a
let print_mset a = String.concat "," 
  (map (fun (s,n) -> Printf.sprintf "%s(%d)" s n) (M.bindings a))

let rec fold_lines kons knil inch = 
  let this_inp = try Some (input_line inch) with End_of_file -> None in
  match this_inp with
      Some inp -> fold_lines kons (kons inp knil) inch
    | None -> knil

let () = Printf.printf "Computing rhyme schemes...\n%!"

let parse_tsv_raw path =
  let inch = open_in path in
  let rec parse_lines lines linenum ncols =
    let this_line = try Some (input_line inch) with End_of_file -> None in
    match this_line with
        Some this_line ->
          let vals = Mu.splittab this_line in
          let vlen = List.length vals in
          if vlen <> ncols then failwith
            (Printf.sprintf "%s:%d: %d cols expected, found %d"
              path linenum ncols vlen) else
          parse_lines (vals :: lines) (linenum + 1) ncols
      | None -> lines in
  let first_line = try input_line inch
    with End_of_file -> failwith
      (Printf.sprintf "%s contains no header" path) in
  let header = Mu.splittab first_line in
  rev (parse_lines [header] 2 (List.length header))

let cmudict = map hd (parse_tsv_raw "local/data/cmudict.dict")
let polyverbs = parse_tsv_raw "rhymes.tsv"

let rhyme_schemes = (* map from (lemmare, vvdre) to canonical "lemma/verb" *)
  fold (fun pvrow schemes -> 
      let find_or_create vv0 vvd vv0re vvdre sch = 
        M.addf (vv0re, vvdre) identity (Printf.sprintf "%s/%s" vv0 vvd) sch in
      match pvrow with 
          lemma :: reg :: irreg :: lemmare :: regre :: irregre :: [] ->
            find_or_create lemma reg lemmare regre
              (find_or_create lemma irreg lemmare irregre schemes)
        | _ -> failwith "malformed rhymes.tsv")
    M.empty (drop 1 polyverbs)

let () = Printf.printf "Finding rhymes in cmudict...\n%!"

let cmu_rhymes = (* map from re to set of rhyming words *)
  let rex_lib =
    let all_rexps = List.concat (map (drop 3) (drop 1 polyverbs)) in
    let compile rexp lib =
      try ignore (M.find rexp lib) ; lib
      with Not_found -> M.add rexp (Pcre.regexp rexp) lib in
    fold compile M.empty all_rexps in
  let pmatch rexstr str = Pcre.pmatch ~rex:(M.find rexstr rex_lib) str in
  let cmudict_matches rexstr = 
    let matches = pmatch rexstr in
    let codare = Pcre.regexp "[( ].*$" in
    to_set 
      (map (fun str -> Pcre.substitute ~rex:codare ~subst:(fun _ -> "") str)
        (List.filter matches cmudict)) in
  M.fold (fun re _ db -> M.add re (cmudict_matches re) db) rex_lib M.empty

(* clean up the database, removing "live" *)
let cmu_rhymes = M.map (M.filter (fun k v -> 
  k <> "live" && (* live_vv0 doesn't rhyme with dive *)
  k <> "read" && (* read_vv0 doesn't rhyme with wed *)
  k <> "moped" && (* not vv *)
  k <> "dred" && (* not a word, most often occurs as hun- dred *)
  k <> "ned" && (* same as above, not a word *)
  k <> "aforesaid" && (* not vv *)
  k <> "sled" (* not vv *))) cmu_rhymes

let rhymes_with word re = set_mem word (M.find re cmu_rhymes)

let match_rhyme_scheme lemma vvd = (* canonicalize rhyme scheme or None *)
  let rec match_ = function 
      ((lre, dre), scheme) :: tl ->
        if rhymes_with lemma lre && rhymes_with vvd dre then Some scheme
        else match_ tl
    | [] -> None in
  match_ (M.bindings rhyme_schemes)

let () = Printf.printf "Finding matching lemma/vvd pairs in vvd-COHA...\n%!"

(* (map from lemma to set of vvd tokens, map from (year, lemma) to count) *)
let (coha_lemmas, lemma_ts) =
  let inch = open_in "local/data/vvd-COHA" in
  ignore (input_line inch) ; (* ignore header *)
  let badline ll = failwith (Printf.sprintf "Bad line in vvd-COHA: %s" ll) in
  let res = fold_lines (fun line (lemtot, lem_byyear) ->
      match Mu.splittab line with 
          genre :: yearstr :: id ::token :: lemma :: [] -> (try 
            (M.addf lemma (fun m -> mset_add token m) M.empty lemtot,
              mset_add (int_of_string yearstr, lemma) lem_byyear) 
            with _ -> badline line )
        | _ -> badline line )
    (M.empty, M.empty) inch in
  close_in inch ; res

let cmu_rhyming_verbs =
  let coha_verbs = M.fold (fun k1 v1 m1 -> 
      M.add k1 true (M.fold (fun k2 v2 m2 -> M.add k2 true m2) v1 m1))
    coha_lemmas M.empty in
  M.map (fun v1 -> M.filter (fun k2 v2 -> set_mem k2 coha_verbs) v1) 
    cmu_rhymes

(* Select monomorphic lemmas whose only vvd fits a lemma/vvd rhyme scheme *)
let canon_rhyme_scheme = (* map from lemma to canonical rhyming "lemma/verb" *)
  M.fold (fun lemma tokens acc ->
      let tokens = M.filter (fun _ v -> v >= 50) tokens in
      if M.cardinal tokens <> 1 then acc else
      match match_rhyme_scheme lemma (fst (M.choose tokens)) with
          Some scheme -> M.add lemma scheme acc
        | None -> acc)
    coha_lemmas M.empty

let rhyming_lemmas = invert_map canon_rhyme_scheme
let lookup_lemma_scheme scheme = 
  try M.find scheme rhyming_lemmas with Not_found -> []

(* map from (year, lemma/verb) to (count, lemmas) *)
let vvd_coha = M.fold (fun (year, lemma) count rhyme_ts ->
    match try Some (M.find lemma canon_rhyme_scheme) with Not_found->None with
        Some scheme -> M.addf (year, scheme)
          (fun (ct, mset) -> (ct + count, M.addf lemma ((+) count) 0 mset))
          (0, M.empty) rhyme_ts
      | None -> rhyme_ts (* only add lemmas that belong to a rhyme scheme *))
  lemma_ts M.empty

let () =
  let ouch = open_out "local/out/rhyme.schemes.tsv" in
  Printf.fprintf ouch "lemma\tregsch\tirregsch\treglemmas\tirreglemmas\n%!" ;
  let print row = match row with
      lemma :: reg :: irreg :: lemmare :: regre :: irregre :: [] ->
        let regsch = M.find (lemmare, regre) rhyme_schemes in
        let irregsch = M.find (lemmare, irregre) rhyme_schemes in
        Printf.fprintf ouch "%s\t%s\t%s\t" lemma regsch irregsch ;
        Printf.fprintf ouch "%s\t%s\n%!"
          (String.concat "," (lookup_lemma_scheme regsch))
          (String.concat "," (lookup_lemma_scheme irregsch))
    | _ -> failwith "malformed rhymes.tsv" in
  Mu.iter print (drop 1 polyverbs) ;
  close_out ouch

let fprint_coha fn coha =
  let ouch = open_out fn  in
  Printf.fprintf ouch "year\tscheme\tcount\tlemmas\n%!" ;
  let print (year, scheme) (count, lemmas) = Printf.fprintf ouch 
    "%d\t%s\t%d\t%s\n%!" year scheme count (print_mset lemmas) in
  M.iter print coha ;
  close_out ouch

let () = fprint_coha "local/out/rhyme.timeseries.vvd.tsv" vvd_coha

(* read COHA *)
let read_file (genre, year, id, n) inch acc =
  Printf.printf "Reading document %s %d %d (%d)...\n%!" genre year id n ;
  let split line = Pcre.split ~rex:(Pcre.regexp "\t") line in
  let rec read_line acc = match try Some (split (input_line inch))
                            with End_of_file -> None with
      Some (token :: lemma :: pos :: tl) ->
        (* Printf.fprintf logf "%s, %s, %s\n%!" token lemma pos ; *)
        let hde = function [] ->
            (Printf.printf "Erroneous line in %s_%d_%d.txt:%s\n%!"
              genre year id (String.concat "\\t" (token::lemma::pos::tl)) ;
              "")
          | hd :: tl -> hd in
        let prim_pos = hde (Pcre.split ~rex:(Pcre.regexp "[%_@\r\n]+") pos) in
        if not (prim_pos = "vvd" || prim_pos = "vvz" || prim_pos = "vvg" ||
          prim_pos = "vvi" || prim_pos = "vv0" || prim_pos = "vvn")
        then read_line acc else 
        let scho = try Some (M.find lemma canon_rhyme_scheme)
                     with Not_found -> None in
        (match scho with
            Some scheme ->
              (* Printf.fprintf logf "RECORDED %s, %s\n%!" prim_pos lemma ; *)
              let newacc = M.addf (year, scheme)
                (fun (count, mset) -> (count + 1, mset_add lemma mset))
                (0, M.empty) acc in
              read_line newacc
          | None -> read_line acc)
    | Some line ->
        Printf.printf "Erroneous line in %s_%d_%d.txt:\n%s\n%!"
          genre year id (String.concat "_" line) ;
        read_line acc
    | None -> acc in
  read_line acc

(* read_file ("mag", 1968, 491444) (open_in "data/COHA/Word_lemma_PoS/mag_1968_491444.txt") M.empty ;; *)

let coha = (* map from (year, lemma/verb) to (count, lemmas) *)
  let datadir = "data/COHA/Word_lemma_PoS" in
  let dir = Unix.opendir datadir in
  let rec read_dir read_file (nn, acc) = (* all the unix bs *)
    let split fn = (Printf.sprintf "%s/%s" datadir fn, 
      Pcre.split ~rex:(Pcre.regexp "[_.]") fn) in
    match try Some (split (Unix.readdir dir)) with End_of_file -> None with
        Some (fn, (genre :: year :: id :: "txt" :: [])) ->
          let inch = open_in fn in
          let newacc = read_file 
            (genre, int_of_string year, int_of_string id, nn) inch acc in
          close_in inch ; 
          read_dir read_file (nn + 1, newacc)
      | Some _ -> read_dir read_file (nn, acc)
      | None -> acc in
  read_dir read_file (1, M.empty)

let () = fprint_coha "local/out/rhyme.timeseries.tsv" coha
