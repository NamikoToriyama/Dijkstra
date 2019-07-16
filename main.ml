open Metro
(*pen Global*)
(* open Higashi *)
open Kenmei
open Dijkstra
 
(*メイン関数*)
let main kiten shuten =
  Dijkstra.dijkstra kiten shuten ;;

(*テスト*)
let test1=  main "" ""
let test2 = main "tokyo" "shibuya"
let test3=  main "myogadani" "ginza"


(* 県名を使った方法 *)
let main2 kiten start shuten goal = 
  Dijkstra2.dijkstra kiten start shuten goal ;;


(*テスト*)
let test1=  main2 "" "" "" ""
let test2 = main2 "otemachi" "tokyo" "shibuya" "tokyo"
let test3=  main2 "myogadani" "tokyo" "shinjuku" "tokyo"
