
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
		| hd::ht ->  print_string "\n"; print_int j; print_string " - "; (printarLista hd (j+1) ); print_string "\n"; (printaMatrizInicial ht  (j+1) ) ;;
(*comentario teste
let rec printaMatrizInicial matriz j = 
	match matriz with
		| [] -> []
		| hd::ht -> if j=1  then print_string "\nJogo de Damas Atual" ;	print_string "\n    1 | 2 | 3 | 4 | 5 | 6 | 7 | 8";	print_string "\n    .   .   .   .   .   .   .   .";	print_string "\n"; print_int j; print_string " - "; (printarLista hd (j+1) );print_string "\n";	(printaMatrizInicial ht (j+1) )
					else print_string "\n"; print_int j; print_string " - "; (printarLista hd (j+1) ); print_string "\n"; (printaMatrizInicial ht  (j+1) ) ;;
	*)
					

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

let rec substituirPeca matriz peca x y = 
	match matriz with
		| []->[]
		| hd::ht ->hd::ht;;
(*		
let rec verificarPeca cabeca peca lista ht =
	if peca= "R" then lista::"P"::ht
	else lista::"P"::ht ;;
*)
let rec verificarPeca peca lista ht =
	if peca= "R" then lista::"P"::ht
	else lista::"P"::ht ;;
	
	
let rec acharPecaColuna linha lista peca i j =
	match linha with	
		| [] -> []
		| hd::ht -> if j=0 then "P"::ht else acharPecaColuna ht (lista) peca i (j-1);;
		
let rec acharPeca matriz peca i j = 
	match matriz with 
		| [] -> []		
		| hd::ht-> if i=0 then acharPecaColuna hd [] peca i j else acharPeca ht peca (i-1) j ;; 

(*let rec moverPeca matriz i j x y peca = 
	match matriz with
		| [] -> []
		| hd::ht -> if (i>0 j>0) then (acharPeca matriz peca i j)
				else (substituirPeca matriz peca x y);;
	*)	

		
		
		
(*		
let rec moverY hd peca x y  =
	match hd with
		| [] -> []
		| hd::ht -> if y >0 then (moverY ht peca x (y-1)) else peca::ht;
		;;

let rec moverPeca matriz peca x y = 
	match matriz with
		| [] -> []
		| hd::ht -> if x > 0 then (moverPeca ht peca (x-1) y ) else (moverY hd peca x (y-1)) ;;

		
let rec andarEmColuna hd lista i j  =
	match hd with
		| [] -> []
		| hd::ht -> if j >0 then (moverJ ht hd::lista i (j-1)) else lista::"P"::ht ;;
				
let rec andarEmLinha matriz lista i j  = 
	match matriz with
		| [] -> []
		| hd::ht -> if i > 0 then andarEmLinha ht hd::lista (i-1) j else andarEmLinha hd [] i (j-1)  ;;
*)


		
let bemVindo = print_string "Bem vindo ao jogo de damas";;

let inicioDeTurno = print_string "\nJogo de Damas Atual"; print_string "\n    1 | 2 | 3 | 4 | 5 | 6 | 7 | 8"; print_string "\n    .   .   .   .   .   .   .   .";;

let turno jogador m  =  if  (jogador mod 2 ) = 0 then print_string "Turno do Jogador "   else print_string "Turno do Computador";;

(*let m = printaMatrizInicial (criarMatrizInicial [] [["B";"R";"B";"R";"B";"R";"B";"R";];["R";"B";"R";"B";"R";"B";"R";"B";]; ["B";"R";"B";"R";"B";"R";"B";"R";];["P";"B";"P";"B";"P";"B";"P";"B";];["B";"P";"B";"P";"B";"P";"B";"P";];["C";"B";"C";"B";"C";"B";"C";"B";];["B";"C";"B";"C";"B";"C";"B";"C";];["C";"B";"C";"B";"C";"B";"C";"B";];]) 1;;
*)
(*
let digitarALinhaEmQueEsta = print_string "Digite a linha em que voce esta: " ;
 
let digitarAColunaEmQueEsta  = print_string "Digite a coluna em que voca esta: " ;
 
let digitarALinhaParaQueVai = print_string "Digite a linha para a qual deseja se mover: " ;
  
let digitarAColunaParaQueVai = print_string "Digite a coluna para a qual deseja se mover: ";
*)

let escolhaDaPosicao m1 m =
	let () = print_string "Digite a linha em que voce esta: " in 
	let i = read_int () in 
	let () = print_string "Digite a coluna em que voca esta: " in
	let j = read_int()  in 
	let () = print_string "Digite a linha para a qual deseja se mover: " in
	let x = read_int() in	
	let () = print_string "Digite a coluna para a qual deseja se mover: " in
	let y = read_int() in  acharPeca m i j;;	
	
	(*
let jogada turno =
	printaMatriz m 1

let turnar matriz  turno = if (0 mod 2) =0 then jogada turno+1 matriz else  jogadaComputador turno+1 matriz;;

	
	
let damas = 	
	let matrizOriginal = criarMatrizInicial [] [["B";"R";"B";"R";"B";"R";"B";"R";];["R";"B";"R";"B";"R";"B";"R";"B";]; ["B";"R";"B";"R";"B";"R";"B";"R";];["P";"B";"P";"B";"P";"B";"P";"B";];["B";"P";"B";"P";"B";"P";"B";"P";];["C";"B";"C";"B";"C";"B";"C";"B";];["B";"C";"B";"C";"B";"C";"B";"C";];["C";"B";"C";"B";"C";"B";"C";"B";];]
		let turnar matrizOriginal 0 ;;
	*)
let damas = 	
	let matrizOriginal = criarMatrizInicial [] [["B";"R";"B";"R";"B";"R";"B";"R";];["R";"B";"R";"B";"R";"B";"R";"B";]; ["B";"R";"B";"R";"B";"R";"B";"R";];["P";"B";"P";"B";"P";"B";"P";"B";];["B";"P";"B";"P";"B";"P";"B";"P";];["C";"B";"C";"B";"C";"B";"C";"B";];["B";"C";"B";"C";"B";"C";"B";"C";];["C";"B";"C";"B";"C";"B";"C";"B";];]
	in escolhaDaPosicao (printaMatrizInicial matrizOriginal 1 ) matrizOriginal ;;
	

	(* x y "P";;*)
	(*
  let i = read_int ()  in
  let digitarAColunaEmQueEsta  = print_string "Digite a coluna em que voca esta: " in
  let j = read_int()  in  
  let digitarALinhaParaQueVai = print_string "Digite a linha para a qual deseja se mover: " in
  let x = read_int() in
  let digitarAColunaParaQueVai = print_string "Digite a coluna para a qual deseja se mover: " in
  let y = read_int() in print_string "\n"*)


		
		
(*bemVindo;;*)
(*printaMatrizInicial m 1;;	
escolhaDaPosicao;;
turno 0;;
printaMatrizInicial m 1;;
*)
damas;;