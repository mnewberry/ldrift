(** Mitchell's utilities *)
(** This file Copyright (c) 2011 Mitchell Johnson. *)
(* *)
(* This software is free software; you can redistribute it and/or *)
(* modify it under the terms of the GNU Library General Public *)
(* License version 2, with the special exception on linking *)
(* described in file LICENSE. *)
(* *)
(* This software is distributed in the hope that it will be useful, *)
(* but WITHOUT ANY WARRANTY; without even the implied warranty of *)
(* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. *)


(** Alternative versions of standard functions that may fix some defects and
introduce others.  Items labeled [kons] are intended to be fold's cons-like
argument.  Cf.  SRFI-1 *)

let assoc k l = try Some (List.assoc k l) with _ -> None
let assocmem = List.mem_assoc

(* option combinators *)
let opt_none = function Some _ -> false | None -> true
let opt_some x = not (opt_none x)
let opt_def d = function Some x -> x | None -> d
let opt_def_f f = function Some x -> x | None -> f ()
let opt_req = function
    Some x -> x
  | None -> failwith "Some required but None present"
let opt_bind f = function
    Some x -> f x
  | None -> None
let opt_fmap f = function
    Some x -> Some (f x)
  | None -> None
let opt_of_list = function
    [] -> None
  | [x] -> Some x
  | _ -> failwith "Could not convert pleural list to option"
let list_of_opt = function
    Some x -> [x]
  | None -> []
let list_of_list_opt l = List.filter opt_some l
let the = function [x] -> x | _ -> failwith "Multiple items when one expected"
let opt_norm = function
    Some (Some x) -> x
  | _ -> None
let exc_to_opt f = try Some (f ()) with _ -> None

(** {7 Misc } *)

let min_max_kons x (min, max) =
  ((if x < min then x else min), (if x > max then x else max)) ;;
let min_kons x min = if x < min then x else min ;;
let max_kons x max = if x > max then x else max ;;
let min_max_kons_by f x ((min, min_obj), (max, max_obj)) =
  ((if f x < min then f x, x else min, min_obj), 
   (if f x > max then f x, x else max, max_obj)) ;;
let min_kons_by f x (min, min_obj) = 
  if f x < min then f x, x else (min, min_obj) ;;
let max_kons_by f x (max, max_obj) = 
  if f x > max then f x, x else (max, max_obj) ;;

let identity x = x ;;
let constant k = (fun x -> k) ;;

let cons a b = a :: b ;;

let hypot a b = sqrt (float (a * a + b * b))
let hypot2 (ax, ay) (bx, by) = let sq x = x * x in 
  sqrt (float (sq (ax - bx) + sq (ay - by)))
let hypot3 (ax, ay, az) (bx, by, bz) = let sq x = x * x in 
  sqrt (float (sq (ax - bx) + sq (ay - by) + sq (az - bz)))

let default d = function
    None -> d
  | Some v -> v

let sgn n = compare n 0 ;;
let divrnd a b = let q = (2 * a) / b in (q + sgn q) / 2 ;;
let sigmod a n = a - n * (divrnd a n)
let ( % ) a b = if a < 0 then a mod b + b else a mod b ;;
  (** The mod operator with a range of [0] through [b] *)

(** {7 Fold } *)

(** [fold kons knil lyst], a pidgin version of the classic list iterator *)
let rec fold kons knil = function 
    [] -> knil
  | h :: t -> fold kons (kons h knil) t ;;

let rec forall p = function [] -> true | h :: t -> p h && forall p t
let rec some p = function [] -> false | h :: t -> p h || some p t

let rec fold2 kons knil la lb = match (la, lb) with ([], []) -> knil
  | ((ha :: ta), (hb :: tb)) -> fold2 kons (kons ha hb knil) ta tb ;
  | _ -> invalid_arg "unequal lengths" ;;

let rec fold3 kons knil la lb lc = match (la, lb, lc) with ([], [], []) -> knil
  | ((ha :: ta), (hb :: tb), (hc :: tc)) -> 
    fold3 kons (kons ha hb hc knil) ta tb tc ;
  | _ -> invalid_arg "unequal lengths" ;;

let rec fold4 kons knil la lb lc ld = match (la, lb, lc, ld) with 
    ([], [], [], []) -> knil
  | ((ha :: ta), (hb :: tb), (hc :: tc), (hd :: td)) -> 
    fold4 kons (kons ha hb hc hd knil) ta tb tc td ;
  | _ -> invalid_arg "unequal lengths" ;;

(** Cartesian folds and maps *)
let cfold2 kons knil la lb =
  fold (fun a k -> 
    fold (fun b k -> kons a b k) k lb) knil la
let cmap2 f = cfold2 (fun a b k -> f a b :: k) []

let cfold3 kons knil la lb lc =
  fold (fun a k -> 
    fold (fun b k -> 
      fold (fun c k -> kons a b c k) k lc) k lb) knil la
let cmap3 f = cfold3 (fun a b c k -> f a b c :: k) []

let cfold4 kons knil la lb lc ld =
  fold (fun a k -> 
    fold (fun b k -> 
      fold (fun c k -> 
        fold (fun d k -> kons a b c d k) k ld) k lc) k lb) knil la
let cmap4 f = cfold4 (fun a b c d k -> f a b c d :: k) []

(** pidgin unfold *)
let rec unfold is_knull kar kdr value tail = if is_knull value = true then tail
  else unfold is_knull kar kdr (kdr value) ((kar value) :: tail) ;;

(** This 'unified' unfold allows the caller to compute [(kar, kdr)] of [value]
at the same time, which may save redundant computation in some cases. *)
let rec unfold_u is_knil unkons seed tail = if is_knil seed = true then tail
  else let (kar, kdr) = unkons seed in 
       unfold_u is_knil unkons kdr (kar :: tail) ;;

(** {7 Common uses of [fold]} *)

(** Any specialization of fold will necessarily be tail-recursive. *)

let min = function 
  (hx :: tx) -> fold min_kons hx tx 
  | _ -> invalid_arg "min_max is not defined on empty lists" ;;
let max = function 
  (hx :: tx) -> fold max_kons hx tx 
  | _ -> invalid_arg "min_max is not defined on empty lists" ;;
let min_max = function
  (hx :: tx) -> fold min_max_kons (hx, hx) tx
  | _ -> invalid_arg "min_max is not defined on empty lists" ;;
let min_by f = function 
  (hx :: tx) -> snd (fold (min_kons_by f) (f hx, hx) tx) 
  | _ -> invalid_arg "min_max is not defined on empty lists" ;;
let fmin_by fold f (hx, tx) = snd (fold (min_kons_by f) (f hx, hx) tx)
let max_by f = function 
  (hx :: tx) -> snd (fold (max_kons_by f) (f hx, hx) tx)
  | _ -> invalid_arg "min_max is not defined on empty lists" ;;
let min_max_by f = function
  (hx :: tx) -> 
    let ((_, min), (_, max)) = 
      fold (min_max_kons_by f) ((f hx, hx), (f hx, hx)) tx in
    (min, max)
  | _ -> invalid_arg "min_max is not defined on empty lists" ;;
let rev l = fold cons [] l ;;
let map f l = let kons kar kdr = (f kar) :: kdr in 
  fold kons [] (rev l) ;;
let map2 f la lb = let kons a b kdr = (f a b) :: kdr in 
  fold2 kons [] (rev la) (rev lb) ;;
let map3 f la lb lc = let kons a b c kdr = (f a b c) :: kdr in 
  fold3 kons [] (rev la) (rev lb) (rev lc) ;;
let iter f l = let kons kar () = f kar in fold kons () l ;;
let iter2 f la lb = let kons a b () = f a b in 
  fold2 kons () la lb ;;
let iter4 f la lb lc ld = let kons a b c d () = f a b c d in 
  fold4 kons () la lb lc ld ;;
let app m n = fold cons n (rev m) ;;
let catmap f l = let kons kar kdr = app (f kar) kdr in fold kons [] (rev l) ;;
let filtmap f l = 
  let kons kar kdr = match f kar with Some x -> x :: kdr | None -> kdr in
  fold kons [] (rev l) ;;
let sfiltmap f l = 
  let kons kar kdr = match f kar with Some x -> x :: kdr | None -> kdr in
  fold kons [] l ;;
let cat l = fold app [] (rev l) ;;

(** Note that partition and grep reverse the order of the list *)

let grep p l = let kons kar kdr = if p kar then kar :: kdr else kdr in
  fold kons [] l ;;

(** partition a list according to a predicate *)
let partition p l = 
  let kons x (ts, fs) = if p x then (x :: ts, fs) else (ts, x :: fs) in
  fold kons ([], []) l ;;

(** partition a list into (a list of) equivalence classes *)
let partition_eqc eq l =
  let eqcs_kons el eqcs =
    let c_kons eqc (eqcs, added) =
      if not added && eq (List.hd eqc) el then ((el :: eqc) :: eqcs, true)
      else (eqc :: eqcs, added)
    in
    let (new_eqcs, added) = fold c_kons ([], false) eqcs in
    if added then new_eqcs else [el] :: eqcs
  in
  fold eqcs_kons [] l

let rec findf f = function
    [] -> raise Not_found
  | x :: l -> (match f x with Some v -> v | None -> findf f l)

(** a fold which runs [kons] once for each equivalence class of elements
    in a list.  The first argument to [kons] is a list of all the
    elements in the equivalence class. *)
let rec fold_eqc eq kons knil els = fold kons knil (partition_eqc eq els)

let ( -- ) a b = 
  if a < b then unfold ((>) a) identity pred b [] 
  else unfold ((<) a) identity succ b [] ;;
  (** Interval operator *)

let range a b = a -- b ;;
let rec do_n n f = if n <= 0 then () else (f () ; do_n (n - 1) f)

let sum = fold (+) 0
let sumf = fold (+.) 0. ;;

let prod = fold ( * ) 0
let prodf = fold ( *. ) 0. ;;

let pow a b = 
  let rec f b t = match b with 0 -> t | b -> f (b - 1) (a * t) in f b 1

let cross a b =
  let kons ael knil = fold (fun bel kn -> (ael, bel) :: kn) knil (rev b) in
  fold kons [] (rev a) ;;

let crossf f a b =
  let kons ael knil = fold (fun bel kn -> f ael bel :: kn) knil (rev b) in
  fold kons [] (rev a) ;;

let rec cross_ls = function 
    [] -> [[]]
  | hd :: tl -> crossf cons hd (cross_ls tl)
  
  

(** Parallel processing *)

let pprociter4 f la lb lc ld = 
  let kons a b c d kn = match Unix.fork () with
      0 -> (f a b c d ; exit 0)
    | r -> r :: kn in
  ignore (map (Unix.waitpid []) (cfold4 kons [] la lb lc ld))

(** {7 Zip and unzip} *)

let zip la lb = let f a b = (a, b) in map2 f la lb ;;
let zip3 la lb lc = let f a b c = (a, b, c) in map3 f la lb lc ;;

let unzip l = let kons (a, b) (al, bl) = (a :: al, b :: bl) in 
  let (a, b) = fold kons ([], []) l in 
  (rev a, rev b) ;;
let unzip3 l = let kons (a, b, c) (al, bl, cl) = (a :: al, b :: bl, c :: cl) in 
  let (a, b, c) = fold kons ([], [], []) l in
  (rev a, rev b, rev c) ;;

(** {7 words from Haskell} *)

let take n l = 
  let rec take_ n acc l = match l with 
    [] -> acc
    | (hl :: tl) -> if n > 0 then take_ (n - 1) (hl :: acc) tl else rev acc in
  take_ n [] l ;;

let rec drop n l = 
  if n = 0 then l else 
  match l with 
      [] -> []
    | hd :: tl -> drop (n - 1) tl

let repeat n x = 
  let rec r n a x = if n > 0 then r (n - 1) (x :: a) x else a in r n [] x ;;

(** {7 words from perl} *)

let split = Pcre.split ~pat:""
let splitws = Pcre.split
let splitnl = Pcre.split ~pat:"\\n"
let splittab = Pcre.split ~pat:"\\t"
let join = String.concat
let joinsp = String.concat " "
let joinnl = String.concat "\n"

(** {7 creative ideas} *)

(** A better syntax for compare: [compare (f a) (f b)] *)
let compare_with f a b = compare (f a) (f b)

(** {!compare_with} with opposite ordering *)
let compare_with_m f a b = compare (f b) (f a)

(** recurse until a fixed point is achieved *)

(* fix_step takes (check last this) which is a predicate indicating the update
   is "done" *)
let rec fix_step check f i = let n = f i in 
  if check i n then n else fix_step check f n
(* if check is eq, argument order doesn't matter *)
let fix_eq eq f i = fix_step eq f i
let fix f i = fix_eq ( = ) f i

(** recurse a number of times *)
let rec rec_n n f i = if n = 0 then i else rec_n (n - 1) f (f i)

(** recurse until... *)
let rec rec_p p f i = if p i then i else rec_p p f (f i)

(** list de-duplication *)
let rle xs =
  let rec rle_ acc lv ct = function
      [] -> rev acc
    | hd :: tl -> if hd == lv 
        then rle_ acc lv (ct + 1) tl 
        else rle_ ((lv, ct) :: acc) hd 1 tl in
  match xs with [] -> [] | hd :: tl -> rle_ [] hd 1 tl ;;

(** {7 Quick and Dirty File IO} *)

let dump filename str = let ch = open_out filename in 
  output_string ch str ; close_out ch ;;
let dump_obj filename obj = let ch = open_out filename in 
  output_value ch obj ; close_out ch ;;

let slurp filename = let chan = open_in filename in
  let rec slurp_ str =
    match (try Some (input_line chan) with End_of_file -> None) with
      Some line -> slurp_ (str ^ line ^ "\n") | None -> str in
  let cont = slurp_ "" in close_in chan ; cont
let slurp_stdout command = let chan = Unix.open_process_in command in
  let rec slurp_ str =
    match (try Some (input_line chan) with End_of_file -> None) with
      Some line -> slurp_ (str ^ line ^ "\n") | None -> str in
  let cont = slurp_ "" in close_in chan ; cont
let slurp_obj filename = let chan = open_in filename in
  input_value chan ;;

(** {5 The extended remix} *)

module Ext = struct
  let rmap f l = let kons kar kdr = (f kar) :: kdr in fold kons [] l ;;
  let rapp a b = fold cons a b ;;

  let canonical_pair a b = if a < b then (a, b) else (b, a) ;;
  let is_singleton l = (l <> []) && (List.tl l == []) ;;
  let time thunk =
    let start = Unix.gettimeofday () in thunk () ;
    Unix.gettimeofday () -. start
end

(* inspired by the haskell implementation as usual *)
let hsl2rgb (h, s, l) = 
  let mod1 f = mod_float f 1.0 in
  let hk = h /. 360. and third = 1. /. 3. in
  let tr = mod1 (hk +. third) and tg = mod1 hk and tb = mod1 (hk -. third) in
  let q = if l < 0.5 then l *. (1. +. s) else l +. s -. l *. s in
  let p = 2. *. l -. q in
  let c t = truncate (255. *.
    if t < 1. /. 6. then p +. ((q -. p) *. 6. *. t) else
    if t < 1. /. 2. then q else
    if t < 2. /. 3. then p +. ((q -. p) *. 6. *. (2. /. 3. -. t)) else p)
  in (c tr, c tg, c tb)
