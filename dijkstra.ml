(* open Global *)
(* open Higashi *)
open Metro
open Kenmei

module Tree = struct

type ('a, 'b) t = Empty
		| Node of ('a, 'b) t * 'a * 'b * ('a, 'b) t

let empty = Empty

let rec insert tree k v = match tree with
    Empty -> Node (Empty, k, v, Empty)
  | Node (left, key, value, right) -> 
      if k = key
	then Node (left, k, v, right)
      else if k < key
	then Node (insert left k v, key, value, right)
	else Node (left, key, value, insert right k v)

let rec search tree k = match tree with
    Empty -> raise Not_found
  | Node (left, key, value, right) -> 
      if k = key then value
      else if k < key then search left k
      else search right k

let rec fold f init tree = match tree with
    Empty -> init
  | Node (left, key, value, right) ->
      f (fold f init left) key value (fold f init right)

let height tree = fold (fun left _ _ right -> 1 + max left right) 0 tree

let length tree = fold (fun left _ _ right -> left + right + 1) 0 tree

end



type eki_t = {
  namae : string*string; 
  saitan_kyori : float;
  temae_list : (string*string) list; 
}


let insert_eki tree pair1 pair2 len =
  try
      let eki1 = Tree.search tree pair1 in
      Tree.insert tree pair1 ((pair2,len) :: eki1)
    with Not_found ->
      Tree.insert tree pair1 ((pair2,len):: [])


(* 目的：ekikan_tree  と ekikan_t 型の駅間を受け取ったら、 その情報を挿入した木を返す *)
(* insert_ekikan : ekikan_tree  -> ekikan_t -> ekikan_tree *)
let insert_ekikan tree ekikan = match ekikan with
    {kiten = first;
     kenk =kenf;
     shuten = last;
     kens = kenl;
     keiyu = change;
     kyori = length;
     jikan =time;
    }  ->
     insert_eki (insert_eki tree (first, kenf) (last, kenl) length) (last, kenl) (first, kenf) length
      

(* 目的：駅間をひとつではなくリストで受け取り、それを順に挿入した木を返 す関数 *)
(* inserts_ekikan : ekikan_tree -> ekikan_t list -> ekikan_tree *)
let rec inserts_ekikan tree lst = match lst with
    [] -> tree
  |first :: rest -> insert_ekikan (inserts_ekikan tree rest) first



(* 目的:駅名ペアと「駅名ペアと距離のペアのリスト」を受け取ったら駅名ペアに該当するペアを見つけて距離を返す。*)
(* search_list : string*string -> ((string*string) * float) list -> float *)
let rec search_list eki lst 
  = match lst with [] -> infinity | first :: rest 
				    -> match first with (ekimei, kyori) 
							-> if eki = ekimei then kyori
							   else search_list eki rest

(* 目的：駅名ペア２つとekikan_treeを受け取ってきたらその２駅間の距離をekikan_treeから求める。 *)
(* get_ekikan_kyori2 : string*string -> string*string -> ekikan_tree -> float *)
let get_ekikan_kyori2 pair1 pair2 tree = search_list pair2 (Tree.search tree pair1)


(*eki_tのリストの作成*)
(*起点のみについては、saitan_kyori を 0. に、temae_list を起点の駅名ペアのみからなるeki_t 型の リストを作る関数 *)
(* make_initial_eki_list2  :  ekimei_t list -> (string*string) -> eki_t list *)
let make_initial_eki_list2 lst kiten =
  (* make_eki : ekimei_t -> eki_t *)
  let make_eki n = match n with {kanji = k;
                                 kana =h;
                                 romaji = e;
                                 ken = p;
                                 shozoku = s;
                                } ->
    if kiten=(k,p)
    then {namae=(k,p);
          saitan_kyori=0.;
          temae_list=[(k,p)]}
    else
      {namae=(k,p);
       saitan_kyori=infinity;
       temae_list=[]}
  in List.map make_eki lst;;

    

(* 目的：ダイクストラ法で最短距離の更新をする関数 *)
(* koushin1 : eki_t -> eki_t -> ekikan_tree -> eki_t *)
let koushin1 p q tree =match (p,q)
  with({namae=pn ;saitan_kyori=ps ;temae_list=pt},{namae=qn;saitan_kyori=qs;temae_list=qt})
    ->let kyori =(get_ekikan_kyori2 pn qn tree) in
  if kyori = infinity then q
  else if  qs > ps+.kyori then {namae=q.namae;saitan_kyori=ps+.kyori;temae_list=q.namae ::p.temae_list }
  else q;;



(* 目的：点 pと 最短距離が未確定の点の集合 V、および 駅間のリストを受け取ったら、V 中の全ての駅について、必要に応じて更新処理を行った後の 未確定の駅の集合を返す関数 *)
(* koushin : eki_t -> eki_t list -> ekikan_tree -> eki_t *)
let koushin p v tree =
  let f q = koushin1 p q tree
  in List.map f v



(* 目的：eki_t list 型のリストを受け取ったら、 「最短距離最小の駅」と「最短距離最小の駅以外からなるリスト」の組を返す関数 *)
(* saitan_wo_bunri2  : eki_t list -> (eki_t * eki_t list)  *)

let rec saitan_wo_bunri2 lst =match lst with
    []->({namae=("","");saitan_kyori=infinity;temae_list=[]},[])
  | { namae = name;saitan_kyori=length;temae_list=temae} ::rest ->
    if rest =[] then ( { namae = name;saitan_kyori=length;temae_list=temae} ,[])
    else  let (a,b)  = saitan_wo_bunri2 rest in
      if a.saitan_kyori >length then ({ namae = name;saitan_kyori=length;temae_list=temae},a::b)
      else (a,{ namae = name;saitan_kyori=length;temae_list=temae}::b)
           


(* 目的：起点のみ最短距離が 0 で他は infinity となっている 駅のリスト と駅間リストを受け取ったら、「起点からの最短距離と『起点からその駅に至る 駅名の（逆順の）リスト』が入った駅」のリストを返すような関数  *)
(* dijkstra_main : eki_t list  -> ekikan_t list -> eki_t list *)
let rec dijkstra_main ekilst ekikan =
  let tree = inserts_ekikan Tree.empty ekikan in
  match ekilst with [] -> [] | first :: rest ->
    let (kiten,eki_v)= saitan_wo_bunri2 ekilst in
    kiten :: dijkstra_main (koushin kiten eki_v tree) ekikan
      
(* 目的：ローマ字の文字列による駅名と駅名リストを受け取ったら、その駅の漢字表記（文字列）と県名(文字列)のペアを返す関数 *)
(* romaji_to_kanji2 : string -> ekimei_t list -> (string * string) *)
let rec romaji_to_kanji2 eki lst =match lst with [] -> ("","")
  | {kanji = k ; kana = h ; romaji = r; ken = p ; shozoku = s} :: rest -> if r = eki then (k,p) else romaji_to_kanji2 eki rest ;;


(* 目的：ekimei_list と駅名を受け取ったら、それを順に整列し, ekimei_list を返す関数 seiretsu2で使う*)
(* ekimei _insert : ekimei_list  -> string -> ekimei_list  *)
let rec ekimei lst eki = match lst with
    [] -> [eki]
  | {kanji = k;
   kana =h;
   romaji = e;
   ken = p;
   shozoku = s;
    } :: rest -> if (eki.ken=p)&&(eki.kanji =k) then eki ::  rest
    else if(eki.ken <> p)&&(eki.ken < p) then eki ::{kanji = k;
   kana =h;
   romaji = e;
   ken = p;
   shozoku = s;
    }:: rest
    else if(eki.ken =p) && (eki.kanji<k) then eki ::{kanji = k;
   kana =h;
   romaji = e;
   ken = p;
   shozoku = s;
    } :: rest
    else  {kanji = k;
   kana =h;
   romaji = e;
   ken = p;
   shozoku = s;
          } :: ekimei rest eki;;


(* 目的：ekimei_list を受け取ったら、それを順に整列し、 さらに重複した駅を取り除いた ekimei_list を返す関数 *)
(* seiretsu2 : ekimei_list  -> ekimei_list  *)
let rec seiretsu2 lst =match lst with [] -> []
| first:: rest -> ekimei (seiretsu2 rest) first




(* 目的：手前リストの結果を表示する関数 *)
let f lst = let (eki, ken)= lst in
  (print_string eki;
   print_string ",";);;



(* 目的：「起点からの最短距離と『起点からその駅に至る 駅名の（逆順の）リスト』が入った駅」のリストから終点の駅を返す *)
(* search_end : eki_t list -> string -> eki_t *)
let rec search_end lst shuten =match lst with [] ->
  (print_string "駅が見つかりませんでした";
   print_newline();
    {namae = ("","") ; saitan_kyori = infinity ; temae_list = [] })
| {namae = (name,ken) ; saitan_kyori = len; temae_list = temae }:: rest ->
  if (name,ken) = shuten then
    (List.iter f temae;
     print_newline();
     print_string "最短距離"; 
     print_float len;
     print_newline();
     {namae = (name,ken) ; saitan_kyori = len; temae_list = temae })
                         else search_end rest shuten


   
(* 目的：起点の（ローマ字の）駅名と終点の（ローマ字の）駅名 と駅名リスト（ekimei_t list 型）・ 駅間リスト（ekikan_t list 型） を受け取ったら終点の駅（eki_t 型）を探して返す *)
(* dijkstra : string -> string -> ekimei_t list -> ekikan_t list -> eki_t  *)
let dijkstra kiten shuten ekimei ekikan =
  let kit=(romaji_to_kanji2 kiten ekimei) in
  let shu=(romaji_to_kanji2 shuten ekimei) in
  let ekimei_lst =seiretsu2 ekimei in
  let v_lst = dijkstra_main (make_initial_eki_list2 ekimei_lst kit) global_ekikan_list 
  in  search_end v_lst shu



(* 課題2-1 *)

module Dijkstra = struct

(* 目的：ekikan_treeと駅間ペアと距離を受け取ったら、その情報を挿入した木を返す *)
(* insert_ekikan : ekikan_tree  -> ekikan_t -> ekikan_tree *)
let insert_eki tree pair1 pair2 len =
  try
      let eki1 = Tree.search tree pair1 in
      Tree.insert tree pair1 ((pair2,len) :: eki1)
    with Not_found ->
      Tree.insert tree pair1 ((pair2,len):: [])




(* 目的：ekikan_tree  と ekikan_t 型の駅間を受け取ったら、 その情報を挿入した木を返す *)
(* insert_ekikan : ekikan_tree  -> ekikan_t -> ekikan_tree *)
let insert_ekikan tree ekikan = match ekikan with
    {kiten = first; kenk =kenf; shuten = last; kens = kenl; keiyu = change; kyori = length; jikan =time;}  
    -> insert_eki (insert_eki tree (first, kenf) (last, kenl) length) (last, kenl) (first, kenf) length




(* 目的：駅間をひとつではなくリストで受け取り、それを順に挿入した木を返 す関数 *)
(* inserts_ekikan : ekikan_tree -> ekikan_t list -> ekikan_tree *)
let rec inserts_ekikan tree lst = match lst with
    [] -> tree
  |first :: rest -> insert_ekikan (inserts_ekikan tree rest) first



(* 目的:駅名ペアと「駅名ペアと距離のペアのリスト」を受け取ったら駅名ペアに該当するペアを見つけて距離を返す。*)
(* search_list : string*string -> ((string*string) * float) list -> float *)
let rec search_list eki lst 
  = match lst with [] -> infinity | first :: rest 
				    -> match first with (ekimei, kyori) 
							-> if eki = ekimei then kyori
							   else search_list eki rest

(* 目的：駅名ペア２つとekikan_treeを受け取ってきたらその２駅間の距離をekikan_treeから求める。 *)
(* get_ekikan_kyori2 : string*string -> string*string -> ekikan_tree -> float *)
let get_ekikan_kyori2 pair1 pair2 tree = search_list pair2 (Tree.search tree pair1)



(*起点のみについては、saitan_kyori を 0. に、temae_list を起点の駅名ペアのみからなるeki_t 型の リストを作る関数 *)
(* make_initial_eki_list2  :  ekimei_t list -> (string*string) -> eki_t list *)
let make_initial_eki_list2 lst kiten =
  (* make_eki : ekimei_t -> eki_t *)
  let make_eki n = match n with 
  {kanji = k; kana =h; romaji = e; ken = p; shozoku = s;} 
  -> if kiten=(k,p) then {namae=(k,p); saitan_kyori=0.; temae_list=[(k,p)]}
    else {namae=(k,p); saitan_kyori=infinity; temae_list=[]}
  in List.map make_eki lst;;




(* 目的：ダイクストラ法で最短距離の更新をする関数 *)
(* koushin1 : eki_t -> eki_t -> ekikan_tree -> eki_t *)
let koushin1 p q tree =match (p,q)
  with({namae=pn ;saitan_kyori=ps ;temae_list=pt},{namae=qn;saitan_kyori=qs;temae_list=qt})
    ->let kyori =(get_ekikan_kyori2 pn qn tree) in
  if kyori = infinity then q
  else if  qs > ps+.kyori then {namae=q.namae;saitan_kyori=ps+.kyori;temae_list=q.namae ::p.temae_list }
  else q;;



(* 目的：点 pと 最短距離が未確定の点の集合 V、および 駅間のリストを受け取ったら、V 中の全ての駅について、必要に応じて更新処理を行った後の 未確定の駅の集合を返す関数 *)
(* koushin : eki_t -> eki_t list -> ekikan_tree -> eki_t *)
let koushin p v tree =
  let f q = koushin1 p q tree
  in List.map f v



(* 目的：eki_t list 型のリストを受け取ったら、 「最短距離最小の駅」と「最短距離最小の駅以外からなるリスト」の組を返す関数 *)
(* saitan_wo_bunri2  : eki_t list -> (eki_t * eki_t list)  *)

let rec saitan_wo_bunri2 lst =match lst with
    []->({namae=("","");saitan_kyori=infinity;temae_list=[]},[])
  | { namae = name;saitan_kyori=length;temae_list=temae} ::rest ->
    if rest =[] then ( { namae = name;saitan_kyori=length;temae_list=temae} ,[])
    else  let (a,b)  = saitan_wo_bunri2 rest in
      if a.saitan_kyori >length then ({ namae = name;saitan_kyori=length;temae_list=temae},a::b)
      else (a,{ namae = name;saitan_kyori=length;temae_list=temae}::b)
           


(* 目的：起点のみ最短距離が 0 で他は infinity となっている 駅のリスト と駅間リストを受け取ったら、「起点からの最短距離と『起点からその駅に至る 駅名の（逆順の）リスト』が入った駅」のリストを返すような関数  *)
(* dijkstra_main : eki_t list  -> ekikan_t list -> eki_t list *)
let rec dijkstra_main ekilst ekikan =
  let tree = inserts_ekikan Tree.empty ekikan in
  match ekilst with [] -> [] | first :: rest ->
    let (kiten,eki_v)= saitan_wo_bunri2 ekilst in
    kiten :: dijkstra_main (koushin kiten eki_v tree) ekikan

      
(* 目的：ローマ字の文字列による駅名と駅名リストを受け取ったら、その駅の漢字表記（文字列）と県名(文字列)のペアを返す関数 *)
(* romaji_to_kanji2 : string -> ekimei_t list -> (string * string) *)
let rec romaji_to_kanji2 eki lst =match lst with [] -> ("","")
  | {kanji = k ; kana = h ; romaji = r; ken = p ; shozoku = s} :: rest -> if r = eki then (k,p) else romaji_to_kanji2 eki rest ;;


(* 目的：ekimei_list と駅名を受け取ったら、それを順に整列し, ekimei_list を返す関数 seiretsu2で使う*)
(* ekimei _insert : ekimei_list  -> string -> ekimei_list  *)
let rec ekimei lst eki = match lst with
    [] -> [eki]
  | {kanji = k; kana =h; romaji = e; ken = p; shozoku = s; } :: rest 
  -> if (eki.ken=p)&&(eki.kanji =k) then eki :: rest
    else if(eki.ken <> p)&&(eki.ken < p) 
    then eki ::{kanji = k; kana =h; romaji = e; ken = p; shozoku = s; }:: rest
    else if(eki.ken =p) && (eki.kanji<k) 
    then eki ::{kanji = k; kana =h; romaji = e; ken = p; shozoku = s; } :: rest
    else  {kanji = k; kana =h; romaji = e; ken = p; shozoku = s; } :: ekimei rest eki;;


(* 目的：ekimei_list を受け取ったら、それを順に整列し、 さらに重複した駅を取り除いた ekimei_list を返す関数 *)
(* seiretsu2 : ekimei_list  -> ekimei_list  *)
let rec seiretsu2 lst =match lst with [] -> []
| first:: rest -> ekimei (seiretsu2 rest) first




(* 目的：手前リストの結果を表示する関数 *)
let f lst = let (eki, ken)= lst in
  (print_string eki;
   print_string ",";);;



(* 目的：「起点からの最短距離と『起点からその駅に至る 駅名の（逆順の）リスト』が入った駅」のリストから終点の駅を返す *)
(* search_end : eki_t list -> string -> eki_t *)
let rec search_end lst shuten =match lst with [] ->
  (print_string "駅が見つかりませんでした";
   print_newline();
    {namae = ("","") ; saitan_kyori = infinity ; temae_list = [] })
| {namae = (name,ken) ; saitan_kyori = len; temae_list = temae }:: rest ->
  if (name,ken) = shuten then
    (List.iter f temae; print_newline();
     print_string "最短距離"; print_float len; print_newline();
     {namae = (name,ken) ; saitan_kyori = len; temae_list = temae })
                         else search_end rest shuten
  
 

(* dijkstra : string -> string -> eki_t  *)
let dijkstra kiten shuten =
  let kit=(romaji_to_kanji2 kiten global_ekimei_list) in
  let shu=(romaji_to_kanji2 shuten global_ekimei_list) in
  let ekimei_lst = seiretsu2 global_ekimei_list in
  let v_lst = dijkstra_main (make_initial_eki_list2 ekimei_lst kit) global_ekikan_list 
  in  search_end v_lst shu
    
end



(*課題2-2*)
module Dijkstra2 = struct

(* 目的：ekikan_treeと駅間ペアと距離を受け取ったら、その情報を挿入した木を返す *)
(* insert_ekikan : ekikan_tree  -> ekikan_t -> ekikan_tree *)
let insert_eki tree pair1 pair2 len =
  try
      let eki1 = Tree.search tree pair1 in
      Tree.insert tree pair1 ((pair2,len) :: eki1)
    with Not_found ->
      Tree.insert tree pair1 ((pair2,len):: [])




(* 目的：ekikan_tree  と ekikan_t 型の駅間を受け取ったら、 その情報を挿入した木を返す *)
(* insert_ekikan : ekikan_tree  -> ekikan_t -> ekikan_tree *)
let insert_ekikan tree ekikan = match ekikan with
    {kiten = first; kenk =kenf; shuten = last; kens = kenl; keiyu = change; kyori = length; jikan =time;}  
    -> insert_eki (insert_eki tree (first, kenf) (last, kenl) length) (last, kenl) (first, kenf) length




(* 目的：駅間をひとつではなくリストで受け取り、それを順に挿入した木を返 す関数 *)
(* inserts_ekikan : ekikan_tree -> ekikan_t list -> ekikan_tree *)
let rec inserts_ekikan tree lst = match lst with
    [] -> tree
  |first :: rest -> insert_ekikan (inserts_ekikan tree rest) first



(* 目的:駅名ペアと「駅名ペアと距離のペアのリスト」を受け取ったら駅名ペアに該当するペアを見つけて距離を返す。*)
(* search_list : string*string -> ((string*string) * float) list -> float *)
let rec search_list eki lst 
  = match lst with [] -> infinity | first :: rest 
				    -> match first with (ekimei, kyori) 
							-> if eki = ekimei then kyori
							   else search_list eki rest

(* 目的：駅名ペア２つとekikan_treeを受け取ってきたらその２駅間の距離をekikan_treeから求める。 *)
(* get_ekikan_kyori2 : string*string -> string*string -> ekikan_tree -> float *)
let get_ekikan_kyori2 pair1 pair2 tree = search_list pair2 (Tree.search tree pair1)



(*起点のみについては、saitan_kyori を 0. に、temae_list を起点の駅名ペアのみからなるeki_t 型の リストを作る関数 *)
(* make_initial_eki_list2  :  ekimei_t list -> (string*string) -> eki_t list *)
let make_initial_eki_list2 lst kiten =
  (* make_eki : ekimei_t -> eki_t *)
  let make_eki n = match n with 
  {kanji = k; kana =h; romaji = e; ken = p; shozoku = s;} 
  -> if kiten=(k,p) then {namae=(k,p); saitan_kyori=0.; temae_list=[(k,p)]}
    else {namae=(k,p); saitan_kyori=infinity; temae_list=[]}
  in List.map make_eki lst;;



    

(* 目的：ダイクストラ法で最短距離の更新をする関数 *)
(* koushin1 : eki_t -> eki_t -> ekikan_tree -> eki_t *)
let koushin1 p q tree =match (p,q)
  with({namae=pn ;saitan_kyori=ps ;temae_list=pt},{namae=qn;saitan_kyori=qs;temae_list=qt})
    ->let kyori =(get_ekikan_kyori2 pn qn tree) in
  if kyori = infinity then q
  else if  qs > ps+.kyori then {namae=q.namae;saitan_kyori=ps+.kyori;temae_list=q.namae ::p.temae_list }
  else q;;


(* 目的：点 pと 最短距離が未確定の点の集合 V、および 駅間のリストを受け取ったら、V 中の全ての駅について、必要に応じて更新処理を行った後の 未確定の駅の集合を返す関数 *)
(* koushin : eki_t -> eki_t list -> ekikan_tree -> eki_t *)
let koushin p v tree =
  let f q = koushin1 p q tree
  in List.map f v



(* 目的：eki_t list 型のリストを受け取ったら、 「最短距離最小の駅」と「最短距離最小の駅以外からなるリスト」の組を返す関数 *)
(* saitan_wo_bunri2  : eki_t list -> (eki_t * eki_t list)  *)

let rec saitan_wo_bunri2 lst =match lst with
    []->({namae=("","");saitan_kyori=infinity;temae_list=[]},[])
  | { namae = name;saitan_kyori=length;temae_list=temae} ::rest ->
    if rest =[] then ( { namae = name;saitan_kyori=length;temae_list=temae} ,[])
    else  let (a,b)  = saitan_wo_bunri2 rest in
      if a.saitan_kyori >length then ({ namae = name;saitan_kyori=length;temae_list=temae},a::b)
      else (a,{ namae = name;saitan_kyori=length;temae_list=temae}::b)



(* 目的：起点のみ最短距離が 0 で他は infinity となっている 駅のリスト と駅間リストを受け取ったら、「起点からの最短距離と『起点からその駅に至る 駅名の（逆順の）リスト』が入った駅」のリストを返すような関数  *)
(* dijkstra_main : eki_t list  -> ekikan_t list -> eki_t list *)
let rec dijkstra_main ekilst ekikan =
  let tree = inserts_ekikan Tree.empty ekikan in
  match ekilst with [] -> [] | first :: rest ->
    let (kiten,eki_v)= saitan_wo_bunri2 ekilst in
    kiten :: dijkstra_main (koushin kiten eki_v tree) ekikan


(* 目的：ekimei_list と駅名を受け取ったら、それを順に整列し, ekimei_list を返す関数 seiretsu2で使う*)
(* ekimei _insert : ekimei_list  -> string -> ekimei_list  *)
let rec ekimei lst eki = match lst with
    [] -> [eki]
  | {kanji = k; kana =h; romaji = e; ken = p; shozoku = s; } :: rest 
  -> if (eki.ken=p)&&(eki.kanji =k) then eki :: rest
    else if(eki.ken <> p)&&(eki.ken < p) 
    then eki ::{kanji = k; kana =h; romaji = e; ken = p; shozoku = s; }:: rest
    else if(eki.ken =p) && (eki.kanji<k) 
    then eki ::{kanji = k; kana =h; romaji = e; ken = p; shozoku = s; } :: rest
    else  {kanji = k; kana =h; romaji = e; ken = p; shozoku = s; } :: ekimei rest eki;;


(* 目的：ekimei_list を受け取ったら、それを順に整列し、 さらに重複した駅を取り除いた ekimei_list を返す関数 *)
(* seiretsu2 : ekimei_list  -> ekimei_list  *)
let rec seiretsu2 lst =match lst with [] -> []
| first:: rest -> ekimei (seiretsu2 rest) first




(* 目的：手前リストの結果を表示する関数 *)
let f lst = let (eki, ken)= lst in
  (print_string eki;
   print_string ",";);;



(* 目的：「起点からの最短距離と『起点からその駅に至る 駅名の（逆順の）リスト』が入った駅」のリストから終点の駅を返す *)
(* search_end : eki_t list -> string -> eki_t *)
let rec search_end lst shuten =match lst with [] ->
  (print_string "駅が見つかりませんでした";
   print_newline();
    {namae = ("","") ; saitan_kyori = infinity ; temae_list = [] })
| {namae = (name,ken) ; saitan_kyori = len; temae_list = temae }:: rest ->
  if (name,ken) = shuten && len <> infinity then
    (List.iter f temae; print_newline();
     print_string "最短距離"; print_float len; print_newline();
     {namae = (name,ken) ; saitan_kyori = len; temae_list = temae })
                         else search_end rest shuten
  
 
 

(* 目的：ローマ字の文字列による駅名と駅名リストを受け取ったら、その駅の漢字表記（文字列）と県名(文字列)のペアを返す関数 *)
(* romaji_to_kenmei : string -> ekimei_t list -> string *)
let rec romaji_to_kenmei ken lst = match lst with [] -> ""
| (romaji, kanji) :: rest ->
  if romaji = ken then kanji 
  else romaji_to_kenmei ken rest ;;


(* 目的：ローマ字の文字列による駅名と駅名リストを受け取ったら、その駅の漢字表記（文字列）と県名(文字列)のペアを返す関数 *)
(* romaji_to_kanji2 : string -> ekimei_t list -> (string * string) *)
let rec romaji_to_kanji2 eki kenmei lst =match lst with [] -> ("","")
  | {kanji = k ; kana = h ; romaji = r; ken = p ; shozoku = s} :: rest ->
  if r = eki && p = kenmei then (k,p) 
  else romaji_to_kanji2 eki kenmei rest ;;


   
(* 目的：起点の（ローマ字の）駅名と終点の（ローマ字の）駅名 と駅名リスト（ekimei_t list 型）・ 駅間リスト（ekikan_t list 型） を受け取ったら終点の駅（eki_t 型）を探して返す *)
(* dijkstra : string -> string -> ekimei_t list -> ekikan_t list -> eki_t  *)
  let dijkstra kiten start shuten goal =
  let kit= romaji_to_kanji2 kiten (romaji_to_kenmei start global_kenmei_list) global_ekimei_list in
  let shu= romaji_to_kanji2 shuten (romaji_to_kenmei goal global_kenmei_list) global_ekimei_list in
  let ekimei_lst =seiretsu2 global_ekimei_list in
  let v_lst = dijkstra_main (make_initial_eki_list2 ekimei_lst kit) global_ekikan_list in  search_end v_lst shu

end

