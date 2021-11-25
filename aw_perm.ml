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
    let idx = ((find_idx (len - 1) arr) + 1) mod len in
    if idx <> 1 then (
      arr.(1) = (arr.(idx) + 1) mod (len - 1)
    ) else (
      arr.(1) = (arr.(2) + 1) mod (len - 1)
    )

let constr_lst arr lst =
  let rec reorder l acc =
    match l with
    | [] -> List.rev acc
    | hd :: tl -> reorder tl ((List.nth lst hd) :: acc)
  in
  reorder (Array.to_list arr) []

let aw_perm_generator lst =
  let len = List.length lst in
  match len with
  | n when n <= 0 -> failwith "size error"
  | 1 ->
     let cnt = ref 1 in
     let next () =
       if !cnt = 0 then None
       else (
         cnt := !cnt - 1;
         Some lst
       )
     in
     next
  | 2 ->
     let cnt = ref 2 in
     let l = ref [[(List.nth lst 0); (List.nth lst 1)]; [(List.nth lst 1); (List.nth lst 0)]] in
     let next () =
       if !cnt = 0 then None
       else (
         cnt := !cnt - 1;
         Some (List.nth !l !cnt)
       )
     in
     next
  | _ ->
     let trigger = Array.init len (fun i -> len - 1 - i) in
     let a_idx = Array.copy trigger in
     let a_idx_end = Array.copy trigger in
     let end_flag = ref false in
     perm_tau a_idx;
     perm_sigma a_idx_end;
     perm_tau a_idx_end;
     let next () =
       if !end_flag = true then
         None
       else
         let result = constr_lst a_idx lst in
         if Array.for_all2 (=) a_idx a_idx_end then (
           end_flag := true
         ) else (
           if is_tau a_idx trigger then perm_tau a_idx else perm_sigma a_idx;
         );
         Some result
     in
     next
