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
		| []-> elemento
		| hd::ht -> inserirNaUltimaPosicao ht elemento
;;

let rec substituirPecaEmColuna lista listaAuxiliar j peca  = 
	match lista with
		|[] ->[]
		| hd::ht -> if j=0 then inserirNaUltimaPosicao listaAuxiliar (inserirNaUltimaPosicao (peca::[]) ht)
					else substituirPecaEmColuna ht (inserirNaUltimaPosicao listaAuxiliar hd) (j-1) peca
;;					
let rec substituirPeca matriz matrizAuxiliar i j peca =
	match matriz with
		[] ->[]
		| hd::ht -> if i = 0 then  inserirNaUltimaPosicao(inserirNaUltimaPosicao matrizAuxiliar (substituirPecaEmColuna hd [] j peca)) ht
					else substituirPeca ht (inserirNaUltimaPosicao matrizAuxiliar hd) (i-1) j peca
;;

let moverPecaDeIJParaXY matriz i j x y = substituirPeca (substituirPeca matriz [] i j "1" ) [] x y "B" 
;;

let comerPecaDeIJParaXY  matriz i j x y k l = substituirPeca (substituirPeca (substituirPeca matriz [] k l "1") [] i j "1") [] x y "B"
;;


let moverPeca matriz i j x y = moverPecaDeIJParaXY matriz y x j i  
;;

let escolhaDaPosicao numeroDeRequisicao = 
		if numeroDeRequisicao = 0 then let() = print_string "Digite o numero da Linha em que esta: " in read_int()
		else if numeroDeRequisicao = 1 then let() = print_string "Digite o numero da Coluna em que esta: " in read_int()
		else if numeroDeRequisicao  = 2 then let() = print_string "Digite o numero da Linha para que vai: " in read_int()
		else let() = print_string "Digite o numero da Coluna para que vai: " in read_int() 
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
	
let turnar matriz numeroDeTurno =  turnoDoJogador matriz
;;	

let rec printarMatriz matriz turno j matrizNova =
	match matriz with
		| [] -> print_string ""
		| hd::ht -> if j =1 then(   print_string "\n\nJogo Atual :\n\n";
									print_string"    1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 |";
									print_string "\n    .   .   .   .   .   .   .   . ";
									print_string "\n";
									print_int j;
									print_string " - ";	
									(printarLista hd 1 j matrizNova turno); 
									print_string "\n";
									(printarMatriz ht turno (j+1) (inserirNaUltimaPosicao matrizNova hd))
								)
					else		    ( print_string "\n";
									print_int j;
									print_string " - ";	
									(printarLista hd 1 j matrizNova turno); 
									print_string "\n";
									(printarMatriz ht turno (j+1) (inserirNaUltimaPosicao matrizNova hd))
								)
		
		and	printarLista lista i j matriz turno  = 
			match lista with
				| []-> print_string ""
				| hd::ht -> if ( (i=8) && (j=8) ) then  print_string hd	
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