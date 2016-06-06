(*
MATRIZ BRANCO = "0"
MATRIZ PRETO = "1"
PECA VERMELHO = "B"
PECA CINZA = "P"
DAMA BRANCA = "D"
DAMA PRETA = "Q"
*)

				
let rec inserirNaUltimaPosicao lista elemento = 
	match lista with	
		| []-> [elemento]
		| hd::ht -> inserirNaUltimaPosicao ht elemento
;;

let rec appendLista lista elemento =
	match lista with
		| []-> [elemento]
		| hd::ht -> hd::appendLista ht elemento;;

let rec appendLista2 lista elemento =
	match lista with
		| []-> elemento
		| hd::ht -> hd::appendLista2 ht elemento;;

		
let rec substituirPecaEmColuna lista j peca  = 
	match lista with
		| [] ->[]
		| hd::ht -> if j=1 then peca@ht
					else hd::(substituirPecaEmColuna ht (j-1) peca)
;;					
let rec substituirPeca matriz i j peca =
	match matriz with
		| [] ->[]
		| hd::ht -> if i = 1 then   (substituirPecaEmColuna hd j peca)::ht
					else hd::(substituirPeca ht (i-1) j peca)
;;

let moverPecaDeIJParaXY matriz i j x y =  substituirPeca (substituirPeca matriz i j ["1"] ) x y ["B"] 
	

(*
let moverPecaDeIJParaXY matriz i j x y = 
	if (verificarPeca matriz i j) = 0 then substituirPeca (substituirPeca matriz i j ["1"] ) x y ["B"] 
	else moverPeca matriz (escolhaDaPosicao 3) (escolhaDaPosicao 2) (escolhaDaPosicao 1) (escolhaDaPosicao 0)(*o primeiro eh na linha e depois coluna da linha que ele esta*)
	
;;*)
let comerPecaDeIJParaXY  matriz i j x y k l = substituirPeca (substituirPeca (substituirPeca matriz k l ["1"]) i j ["1"]) x y ["B"]
;;

let escolhaDaPosicao numeroDeRequisicao = 
		if numeroDeRequisicao = 0 then let() = print_string "\nInsira as posicoes :\nDigite o numero da Linha em que esta: " in read_int()
		else if numeroDeRequisicao = 1 then let() = print_string "\nDigite o numero da Coluna em que esta: " in read_int()
		else if numeroDeRequisicao  = 2 then let() = print_string "\nDigite o numero da Linha para que vai: " in read_int()
		else let() = print_string "\nDigite o numero da Coluna para que vai: " in read_int() 
;;

let pecaCapturada matriz i j x y =
	if y = (j-1) then comerPecaDeIJParaXY matriz i j x y (x-1) (y+1) 
	else comerPecaDeIJParaXY matriz i j x y (x-1)(y-1)
;;



let moverPeca matriz i j x y =  moverPecaDeIJParaXY matriz y x j i 
;;
(*
let moverPeca matriz i j x y =   
	if i = (x-1) then moverPecaDeIJParaXY matriz y x j i 
	else if i = (x+1) then moverPecaDeIJParaXY matriz y x j i 
	else pecaCapturada matriz y x j i
;;*)

let turnoDoJogador matriz = moverPeca matriz (escolhaDaPosicao 3) (escolhaDaPosicao 2) (escolhaDaPosicao 1) (escolhaDaPosicao 0)(*o primeiro eh na linha e depois coluna da linha que ele esta*)
;;

let rec verificarPecaNaLinhaDoComputadorBranca lista i j = 
	match lista with
		| []->[]
		| hd::ht->	if hd="B" then i::j::[]
					else verificarPecaNaLinhaDoComputadorBranca ht i (j+1)
;;

let rec verificarPecaNaLinhaDoComputadorPreta lista i j = 
	match lista with
		| []->[]
		| hd::ht->	if hd="P" then i::j::[]
					else verificarPecaNaLinhaDoComputadorPreta ht i (j+1)
;;

let rec verificarPecaMaisAFrenteERetornarOPosicaoIJ matriz listaAuxiliar i = 
	match matriz with
		| []-> []
		| hd::ht-> if (verificarPecaNaLinhaDoComputadorBranca hd i 1)!=[] then
						verificarPecaMaisAFrenteERetornarOPosicaoIJ ht hd (i+1)
					else verificarPecaNaLinhaDoComputadorBranca listaAuxiliar i 1 
;;
let rec verificarPecaMaisAFrenteERetornarOPosicaoXY matriz listaAuxiliar i = 
	match matriz with
		| []-> []
		| hd::ht-> if (verificarPecaNaLinhaDoComputadorPreta hd i 1)!=[] then
						verificarPecaMaisAFrenteERetornarOPosicaoIJ ht hd (i+1)
					else verificarPecaNaLinhaDoComputadorPreta listaAuxiliar i 1 
;;
let moverPecaDoComputador matriz = 
;;

let turnoDoComputador matriz  = 
	let posicaoIJ  = verificarPecaMaisAFrenteERetornarOPosicaoIJ matriz
	in let posicaoXY = verificarPecaMaisAFrenteERetornarOPosicaoXY matriz
	in moverPecaDoComputador matriz 
;;
	
let turnar matriz numeroDeTurno = if (numeroDeTurno mod 2) =0  then turnoDoJogador matriz
									else turnoDoComputador matriz
;;	
let rec printarMatriz matriz turno j matrizNova =
	match matriz with
		| [] -> print_string ""
		| hd::ht -> if (turno mod 2) =0	then
		
						if j =1  then(  print_string "\n\nTurno do Jogador :\n\n";
										print_string "Jogo Atual :\n\n";
										print_string"    1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 |";
										print_string "\n    .   .   .   .   .   .   .   . ";
										print_string "\n";
										print_int j;
										print_string " - ";	
										(printarLista hd 1 j (appendLista matrizNova hd) turno); 
										print_string "\n";
										(printarMatriz ht turno (j+1) (appendLista matrizNova hd) )
									)
						else		    ( print_string "\n";
										print_int j;
										print_string " - ";	
										(printarLista hd 1 j (appendLista matrizNova hd) turno); 
										print_string "\n";
										(printarMatriz ht turno (j+1) (appendLista matrizNova hd) 	 )
									)
					else 
						if j =1  then(  print_string "\nTurno do Computador :\n\n";
										print_string "Jogo Atual :\n\n";
										print_string"    1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 |";
										print_string "\n    .   .   .   .   .   .   .   . ";
										print_string "\n";
										print_int j;
										print_string " - ";	
										(printarLista hd 1 j (appendLista matrizNova hd) turno); 
										print_string "\n";
										(printarMatriz ht turno (j+1) (appendLista matrizNova hd) )
									)
						else		    ( print_string "\n";
										print_int j;
										print_string " - ";	
										(printarLista hd 1 j (appendLista matrizNova hd) turno); 
										print_string "\n";
										(printarMatriz ht turno (j+1) (appendLista matrizNova hd) 	 )
									)
		
		and	printarLista lista i j matriz turno  = 
			match lista with
				| []-> print_string "";
				| hd::ht -> if ( (i=8) && (j=8) ) then  (print_string hd; (printarMatriz (turnar matriz turno) (turno+1) 1 []))
							else print_string hd; print_string" | "; (printarLista ht (i+1) j matriz turno) 
				
;;
(*
let DecideOJogador = 
	
let Vencedor =
	print_string "Quem venceu foi o ";
	DecideOJogador;*)
(*
let rec printarMatriz matriz turno j matrizNova =
	match matriz with
		| [] -> []
		| hd::ht -> print_string "\n";
					print_int j;
					print_string " - ";
					(printarLista hd 1 j matrizNova turno); 
					print_string "\n";
					(printarMatriz ht turno (j+1) (inserirNaUltimaPosicao matrizNova hd));
		and	printarLista lista i j matriz turno  = 
			match lista with
				| []-> if ((i=8) && (j=8)) then (printarMatriz (turnar matriz turno) (turno+1) 0 [])
				| hd::ht -> print_string hd; print_string" | "; (printarLista ht (i+1) j matriz turno) 
;;


*)
print_string "Bem vindo ao Jogo de Damas \n";;
print_string "Legenda:\n ";;
print_string "Peca branca:  B\n Peca preta: P \n Peca Dama branca: Q\n Peca Dama Preta K \n Casa Do tabuleiro Branca: 0\n Casa Do Tabuleiro: 1";;


printarMatriz  [["0";"B";"0";"B";"0";"B";"0";"B";];
				["B";"0";"B";"0";"B";"0";"B";"0";];
				["0";"B";"0";"B";"0";"B";"0";"B";];
				["1";"0";"1";"0";"1";"0";"1";"0";];
				["0";"1";"0";"1";"0";"1";"0";"1";];
				["P";"0";"P";"0";"P";"0";"P";"0";];
				["0";"P";"0";"P";"0";"P";"0";"P";];
				["P";"0";"P";"0";"P";"0";"P";"0";]]
				0 1 []			
;;

(*
printarMatriz  [[["0"];["B"];["0"];["B"];["0"];["B"];["0"];["B"];];
				[["B"];["0"];["B"];["0"];["B"];["0"];["B"];["0"];];
				[["0"];["B"];["0"];["B"];["0"];["B"];["0"];["B"];];
				[["1"];["0"];["1"];["0"];["1"];["0"];["1"];["0"];];
				[["0"];["1"];["0"];["1"];["0"];["1"];["0"];["1"];];
				[["P"];["0"];["P"];["0"];["P"];["0"];["P"];["0"];];
				[["0"];["P"];["0"];["P"];["0"];["P"];["0"];["P"];];
				[["P"];["0"];["P"];["0"];["P"];["0"];["P"];["0"];]]
				0 1 [[ ]]					
;;*)