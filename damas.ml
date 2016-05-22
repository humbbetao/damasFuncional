
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



let rec criarMatrizInicial matriz lista = 
  match matriz with
    | []-> lista
    | hd::ht -> hd :: criarMatrizInicial ht lista
;;

let rec printaMatriz matriz j = 
	match matriz with
		| [] -> []
		| hd::ht -> print_string "\n"; print_int j; print_string " - "; (printarLista hd (j+1) ); print_string "\n"; (printaMatriz ht  (j+1) )
		and	printarLista lista i = 
			match lista with
				| []->  print_string ""
				| hd::ht-> if i =0 then (print_string" | "; print_int i;  print_string" - "; print_string hd;)
							else print_string hd; print_string" | "; (printarLista ht (i+1)) ;;						
		

print_string "Bem vindo ao jogo de damas";;
print_string "\nJogo de Damas Atual";;
print_string "\n    1 | 2 | 3 | 4 | 5 | 6 | 7 | 8";;
print_string "\n    .   .   .   .   .   .   .   .";;
printaMatriz (criarMatrizInicial [] [["B";"R";"B";"R";"B";"R";"B";"R";];["R";"B";"R";"B";"R";"B";"R";"B";]; ["B";"R";"B";"R";"B";"R";"B";"R";];["P";"B";"P";"B";"P";"B";"P";"B";];["B";"P";"B";"P";"B";"P";"B";"P";];["C";"B";"C";"B";"C";"B";"C";"B";];["B";"C";"B";"C";"B";"C";"B";"C";];["C";"B";"C";"B";"C";"B";"C";"B";];]) 1
;;
print_string "Sua Jogada\n ";;

let rec escolhaDaPosicao =
  let () = print_string "Digite a linha: " in
  let i = read_int ()  in
  let ()  = print_string "Digite a coluna: " in
  let j = read_int()  in  
    	print_int i; print_int j; print_string "\n"
;;
	
	
	
	