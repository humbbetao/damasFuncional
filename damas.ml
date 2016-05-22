let rec criarMatrizInicial matriz lista = 
  match matriz with
    | []-> lista
    | hd::ht -> hd :: criarMatrizInicial ht lista
;;


let rec printar matriz = 
  match matriz with
    | []-> ()
    | hd::ht -> print_string hd ;print_string " | "; printar ht
;;


let bemvindo  = print_string "Bem vindo ao jogo de damas";;


(*chamada de execucao*)
criarMatrizInicial  [] [["B";"R";"B";"R";"B";"R";"B";"R";"B";];["R";"B";"R";"B";"R";"B";"R";"B";"R";]; ["B";"R";"B";"R";"B";"R";"B";"R";"B";];["P";"B";"P";"B";"P";"B";"P";"B";"P";];["B";"P";"B";"P";"B";"P";"B";"P";"B";];["C";"B";"C";"B";"C";"B";"C";"B";"C";];["B";"C";"B";"C";"B";"C";"B";"C";"B";];["C";"B";"C";"B";"C";"B";"C";"B";"C";];]
;;


let rec escolhaDaPosicao =
  let () = print_string "Digite a linha: " in
  let i = read_int ()  in
  let ()  = print_string "Digite a coluna: " in
  let j = read_int()  in


    print_int i; print_int j;;




let damas = function
  let matriz = criarMatrizInicial  [] [["B";"R";"B";"R";"B";"R";"B";"R";"B";];["R";"B";"R";"B";"R";"B";"R";"B";"R";]; ["B";"R";"B";"R";"B";"R";"B";"R";"B";];["P";"B";"P";"B";"P";"B";"P";"B";"P";];["B";"P";"B";"P";"B";"P";"B";"P";"B";];["C";"B";"C";"B";"C";"B";"C";"B";"C";];["B";"C";"B";"C";"B";"C";"B";"C";"B";];["C";"B";"C";"B";"C";"B";"C";"B";"C";];]
  in 
    printar matriz; escolhaDaPosicao;;


damas;;



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
