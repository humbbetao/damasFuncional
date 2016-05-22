let rec criarMatrizInicial matriz lista = 
  match matriz with
    | []-> lista
    | hd::ht -> hd :: criarMatrizInicial ht lista
;;



(*chamada de execucao*)
criarMatrizInicial  [] [["B";"R";"B";"R";"B";"R";"B";"R";"B";];["R";"B";"R";"B";"R";"B";"R";"B";"R";]; ["B";"R";"B";"R";"B";"R";"B";"R";"B";];["P";"B";"P";"B";"P";"B";"P";"B";"P";];["B";"P";"B";"P";"B";"P";"B";"P";"B";];["C";"B";"C";"B";"C";"B";"C";"B";"C";];["B";"C";"B";"C";"B";"C";"B";"C";"B";];["C";"B";"C";"B";"C";"B";"C";"B";"C";];]
;;










(*
;;

let menos posicao  = posicao - 1
;;

let rec inserir lista e =
match lista with
| [] ->[e] 
| hd::ht->hd :: (inserir ht e)
;;*)

(*crio a matriz Inicial *)
(*let damas =
  print_string "Bem vindo a damas feito em ocaml" ;*)
(*encadeamento de funcoes*)




(* tento mover uma peca com a cor e a peca 
   let rec moverPeca matriz cor linha coluna = 


   ;;
*)

(*
let
val BRANCO = "B"
val PRETO = "P"
val VERMELHO = "R"
val CINZA = "C"
val QUEEN = "Q"
val KING = "K"
in
end;*)

(* B = branco, P = preto, R = vermelha; C = Cinza *)


(* daqui pra baixo eh so chamada de execucao*)
