let rec inserirNaUltimaPosicao lista elemento = 
	match lista with	
		| []-> [elemento]
		| hd::ht -> inserirNaUltimaPosicao ht elemento
;;

let rec appendLista lista elemento =
	match lista with
		| []-> [elemento]
		| hd::ht -> hd::appendLista ht elemento
;;

let rec appendLista2 lista elemento =
	match lista with
		| []-> elemento
		| hd::ht -> hd::appendLista2 ht elemento
	;;

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

let rec verificarPecaemY lista y =
	match lista with
		|[]->[]
		|hd::ht -> if (y=0 && hd="P") then ["P"]
					else verificarPecaemY ht (y-1)
;;					

let rec verificarPecaEMXY matriz x y  =
	match matriz with
	|[]->[]
	|hd::ht->if x=0 then
				verificarPecaemY hd y
				else verificarPecaEMXY ht (x-1) y
;;

let decidirPosicao j y =
	if (j = 2 && y = 1	) then j
	else if (j=7 && y=8) then j
	else if (j < y) then (y+1)
	else if (j > y) then (y-1)
	else (y-1)	
;;


let moverPecaDeIJParaXY matriz i j x y = if (["P"] = (verificarPecaEMXY matriz x y)) then
											substituirPeca (substituirPeca (substituirPeca matriz i j ["1"] ) x y ["1"]) (x+1) (decidirPosicao j y) ["B"]
										else
											substituirPeca (substituirPeca matriz i j ["1"] ) x y ["B"]
;;

(*Captura a peça Preta e desloca a peça Branca para sua nova posição,
sendo i é a linha e j é a coluna em que esta, x é a linha e y é a coluna para onde ir se deslocar e 
k é a linha e l é a coluna em que se encontra a peça a ser capturada*)
let comerPecaDeIJParaXY  matriz i j x y k l = substituirPeca (substituirPeca (substituirPeca matriz k l ["1"]) i j ["1"]) x y ["B"]
;;

(*O jogador informa a localidade atual da sua peça e para onde deseja se deslocar*)
let escolhaDaPosicao numeroDeRequisicao = 
		if numeroDeRequisicao = 0 then let() = print_string "\nInsira as posicoes :\nDigite o numero da Linha em que esta: " in read_int()
		else if numeroDeRequisicao = 1 then let() = print_string "\nDigite o numero da Coluna em que esta: " in read_int()
		else if numeroDeRequisicao  = 2 then let() = print_string "\nDigite o numero da Linha para que vai: " in read_int()
		else let() = print_string "\nDigite o numero da Coluna para que vai: " in read_int() 
;;

(*Descobre a posição da peça a ser capturada,
sendo i é a linha e j é a coluna em que esta e 
x é a linha e y é a coluna para onde ir se deslocar,
podemos saber qual a posição que se encontra a peça a ser capturada*)
let pecaCapturada matriz i j x y =
	if y = (j-1) then comerPecaDeIJParaXY matriz i j x y (x-1) (y+1) 
	else comerPecaDeIJParaXY matriz i j x y (x-1)(y-1)
;;

(*Move uma peça Preta, onde i é a linha e j é a coluna em que esta e x é a linha e y é a coluna para onde ir se deslocar*)
let moverPecaPreta matriz i j x y =  substituirPeca (substituirPeca matriz i j ["1"] ) x y ["P"]
;;

let moverPeca matriz i j x y = moverPecaDeIJParaXY matriz y x j i 
;;

(*Chama a função para o jogador fazer a sua jogada, pasando como parametro a linha e a coluna em que se encontra e para onde deseja se deslocar*)
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
		| hd::ht-> if (verificarPecaNaLinhaDoComputadorPreta hd i 1)=[] then
						verificarPecaMaisAFrenteERetornarOPosicaoXY ht hd (i+1)
					else verificarPecaNaLinhaDoComputadorPreta hd i 1 
;;


let moverPecaDoComputador matriz posicaoIJ posicaoXY =
	match posicaoIJ with
		|[] ->[]
		|i::j::tailIJ ->match posicaoXY with
						|[]->[]
						|x::y::tailXY-> if j >= y then moverPecaPreta matriz x y (x-1)(y+1)
										else moverPecaPreta matriz x y (x-1) (y+1)
;;

let rec verificarSeTemPecaNaLinha lista peca i j =
	match lista with
		|[]->[]
		|hd::ht -> if hd="B" then i::j::[]
					else verificarSeTemPecaNaLinha ht peca i (j+1)
;;

let rec verificarSeTemPecaNaLinhaAdjacente lista peca i j parOrdenado =
	match lista with
		|[]->[],[]
		|hd::ht -> match parOrdenado with
					|[]->[],[]
					|head::tail -> 
								if (((List.hd tail) = (j-1)) && (hd ="P")) then
									((i::j::[]), (head::tail))
								else if (((List.hd tail) = (j+1)) && (hd ="P"))then
									((i::j::[]), (head::tail))
								else verificarSeTemPecaNaLinhaAdjacente	ht peca i (j+1) (head::tail)
;;


let rec verificarSeDaPraComer matriz lista i=
	match matriz with
		|[]-> [],[]
		|hd::ht ->if i<7&&(verificarSeTemPecaNaLinhaAdjacente (List.hd ht) "P" i 1 (verificarSeTemPecaNaLinha hd "B" i 1 ))= ([],[]) then
							verificarSeDaPraComer ht lista (i+1)
						else (verificarSeTemPecaNaLinhaAdjacente (List.hd ht) "P" i 1 (verificarSeTemPecaNaLinha hd "B" i 1 ))
;;

let turnoDoComputador matriz = 
	let posicaoIJ, posicaoXY  = if (verificarSeDaPraComer matriz [] 1)=([],[]) then
			verificarPecaMaisAFrenteERetornarOPosicaoIJ matriz [] 1, verificarPecaMaisAFrenteERetornarOPosicaoXY matriz [] 1
		else verificarSeDaPraComer matriz [] 1
	in  moverPecaDoComputador matriz posicaoIJ posicaoXY
;;

	
let turnar matriz numeroDeTurno = if (numeroDeTurno mod 2) =0  then turnoDoJogador matriz
									else turnoDoComputador matriz
;;	

(*Printa a matriz a cada novo turno, onde passamos a matriz, o numero de turno é 0, j é 1 e matrizNova é vazia*)
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
