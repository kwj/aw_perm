 
let perm_sigma arr =
  let len = Array.length arr in
  let tmp = arr.(0) in
  Array.blit arr 1 arr 0 (len - 1);
  arr.(len - 1) <- tmp

let perm_tau arr =
  let tmp = arr.(0) in
  arr.(0) <- arr.(1); arr.(1) <- tmp

let find_idx elm arr =
  let rec aux i =
    if i < 0 then
      failwith "index error"
    else
      if arr.(i) = elm then i else aux (pred i)
  in
  aux ((Array.length arr) - 1)

let is_tau arr t_arr =
  if Array.for_all2 (=) arr t_arr then
    false
  else
    let len = Array.length arr in
    let idx = ((find_idx len arr) + 1) mod len in
    if idx <> 1 then (
      arr.(1) = (arr.(idx) mod (len - 1)) + 1
    ) else (
      arr.(1) = (arr.(2) mod (len - 1)) + 1
    )

let constr_lst arr lst =
  let rec reorder l acc =
    match l with
    | [] -> List.rev acc
    | hd :: tl -> reorder tl ((List.nth lst (hd - 1)) :: acc)
  in
  reorder (Array.to_list arr) []

let aw_perm_generator lst =
  let rec factorial n =
    if n <= 0 then 1 else n * factorial (n - 1)
  in
  let len = List.length lst in
  let cnt = ref (factorial len) in
  match len with
  | n when n <= 0 -> failwith "size error"
  | 1 ->
     let next () =
       if !cnt = 0 then None else (
         cnt := !cnt - 1;
         Some lst
       )
     in
     next
  | 2 ->
     let l = ref [[(List.nth lst 0); (List.nth lst 1)]; [(List.nth lst 1); (List.nth lst 0)]] in
     let next () =
       if !cnt = 0 then None else (
         cnt := !cnt - 1;
         Some (List.nth !l !cnt)
       )
     in
     next
  | _ ->
     let trigger = Array.init len (fun i -> len - i) in
     let a_idx = Array.copy trigger in
     perm_tau a_idx;
     let next () =
       if !cnt = 0 then
         None
       else (
         cnt := !cnt - 1;
         let result = constr_lst a_idx lst in
         if is_tau a_idx trigger then perm_tau a_idx else perm_sigma a_idx;
         Some result
       )
     in
     next
