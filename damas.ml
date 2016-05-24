
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

(* link importante http://severus.no.sapo.pt/files/escola/disciplinas/LAP/teorica2.html*)



let rec printarLista lista i = 
	match lista with
		| []->  print_string ""
		| hd::ht-> if i =0 then (print_string" | "; print_int i;  print_string" - "; print_string hd;)
					else print_string hd; print_string" | "; (printarLista ht (i+1)) ;;						


let rec printaMatrizInicial matriz j = 
	match matriz with
		| [] -> []
		| hd::ht -> print_string "\n"; print_int j; print_string " - "; (printarLista hd (j+1) ); print_string "\n"; (printaMatrizInicial ht  (j+1) );;
	

let rec printaMatriz matriz j = 
	match matriz with
		| [] -> []
		| hd::ht -> print_string "\n"; print_int j; print_string " - "; (printarLista hd (j+1) ); print_string "\n"; (printaMatriz ht  (j+1) )
		and	printarLista lista i = 
			match lista with
				| []->  print_string ""
				| hd::ht-> if i =0 then (print_string" | "; print_int i;  print_string" - "; print_string hd;)
							else print_string hd; print_string" | "; (printarLista ht (i+1)) ;;						


let rec criarMatrizInicial matriz lista = 
  match matriz with
    | []-> lista
    | hd::ht -> hd :: criarMatrizInicial ht lista
;;

let rec moverPeca matriz peca i j x y = 
	match matriz with
		| [] -> []
		| hd::ht -> if x > 0 then (moverPeca ht peca i j (x-1) y ) else (moverY hd peca i j x (y-1)) 
		and moverY hd peca i j x y  =
			match hd with
				| [] -> []
				| hd::ht -> if y >0 then (moverY ht peca i j x (y-1)) else peca::ht;
		;;

let rec mover matriz i j x y = 
	match matriz with
		| [] -> []
		| hd::ht -> if i >0 then mover ht (i-1) j x y else moverJ hd i (j-1) x y  
		and moverJ hd i j x y =
			match hd with
				| [] -> []
				| hd::ht -> if j >0 then (moverJ ht i (j-1) x y) else "P"::ht;
				
;;
print_string "Bem vindo ao jogo de damas";;
print_string "\nJogo de Damas Atual";;
print_string "\n    1 | 2 | 3 | 4 | 5 | 6 | 7 | 8";;
print_string "\n    .   .   .   .   .   .   .   .";;
let m = printaMatrizInicial (criarMatrizInicial [] [["B";"R";"B";"R";"B";"R";"B";"R";];["R";"B";"R";"B";"R";"B";"R";"B";]; ["B";"R";"B";"R";"B";"R";"B";"R";];["P";"B";"P";"B";"P";"B";"P";"B";];["B";"P";"B";"P";"B";"P";"B";"P";];["C";"B";"C";"B";"C";"B";"C";"B";];["B";"C";"B";"C";"B";"C";"B";"C";];["C";"B";"C";"B";"C";"B";"C";"B";];]) 1
;;

print_string "Sua Jogada\n ";;


let rec escolhaDaPosicao =
  let () = print_string "Digite a linha em que voce esta: " in
  let i = read_int ()  in
  let ()  = print_string "Digite a coluna em que voca esta: " in
  let j = read_int()  in  
  let () = print_string "Digite a linha para a qual deseja se mover: " in
  let x = read_int() in
  let () = print_string "Digite a coluna para a qual deseja se mover: " in
  let y = read_int() in
		print_string "\n";  mover m i j x y;
;;

printaMatrizInicial m 0;;		
	