(*
MATRIZ BRANCO = "0"
MATRIZ PRETO = "1"
PECA VERMELHO = "B"
PECA CINZA = "P"
DAMA BRANCA = "D"
DAMA PRETA = "Q"
*)


let rec printarLista lista i = 
	match lista with
		| []->  print_string ""
		| hd::ht-> if i =0 then (print_string" | "; print_int i;  print_string" - "; print_string hd;)
					else print_string hd; print_string" | "; (printarLista ht (i+1)) 
;;

let rec printaMatriz matriz j = 
	match matriz with
		| [] -> []
		| hd::ht ->  print_string "\n"; print_int j; print_string " - "; (printarLista hd (j+1) ); print_string "\n"; (printaMatriz ht  (j+1) ) 


;;

(*	
let rec acharNaMatrizALinha matriz peca x y = 
	if peca = "P" then 
	else if peca = "B" then
	else if peca = "Q" then
	else if peca = "D" then
	else print_string "Erro"
	match matriz with 
		| [] -> []		
		| hd::ht-> if i=0 then acharPecaColuna hd [] peca i j else acharPeca ht peca (i-1) j ;; 
*)
		
let bemVindo = print_string "Bem vindo ao jogo de damas";;

let inicioDeTurno = print_string "\nJogo de Damas Atual"; print_string "\n    1 | 2 | 3 | 4 | 5 | 6 | 7 | 8"; print_string "\n    .   .   .   .   .   .   .   .";;


(*Codigo Aproveitavel*)

let rec substituirPecaEmColuna lista listaAuxiliar j peca  = 
	match lista with
		| [] -> []
		| hd::ht -> if j=0 then listaAuxiliar::peca::ht 
					else  substituirPecaEmColuna ht lista::hd j-1 peca
;;					
let rec substituirPeca matriz matrizAuxiliar i j peca =
	match matriz with
		| []->[]
		| hd::ht -> if i = 0 then matrizAuxiliar::(substituirPecaEmColuna hd [] j peca)::ht 
					else substituirPeca ht matrizAuxiliar::hd i-1 j peca
;;

let moverPecaDeIJParaXY matriz i j x y = substituirPeca (substituirPecaDeIJ matriz [] i j "1") [] x y "P"
;;

let comerPecaDeIJParaXY  matriz i j x y  = substituirPeca (substituirPecaDeIJ  matriz [] i j "1") []  x y "P"
;;

let rec verificarNaColuna linha posicaoJ =
	match linha with	
		| []->[]
		| hd::ht -> if posicaoJ = 0 && hd="1" then 0
					else verificarNaColuna ht posicaoJ-1 
;;

let rec verificarNaLinhaEColuna matriz posicaoI posicaoJ =
	match matriz with
			| []->[]
		| hd::ht -> if posicaoI=0 then verificarNaColuna hd posicaoJ 
					else  verificarNaLinhaEColuna ht matriz posicaoI-1 posicaoJ
;;

let rec verificarPeca matriz i j = 
	if (verificarNaLinhaEColuna matriz i+1 j-1 && verificarNaLinhaEColuna matriz i+1 j+1) =0  then 0
	else 1
;;
	
let moverPeca matriz i j x y =
	if (verificar matriz i j) = 0 then moverPecaDeIJParaXY matriz i j x y 
	else comerPecaDeIJParaXY matriz i j x y
;;	

let escolhaDaPosicao numeroDeRequisicao = 
		if (numeroDeRequisicao mod 4) = 0 then let() = print_string "Digite o numero da Linha em que esta: " in read_int()
		else if (numeroDeRequisicao mod 4) = 1 then let() = print_string "Digite o numero da Coluna em que esta: " in read_int()
		else if (numeroDeRequisicao mod 4) = 2 then let() = print_string "Digite o numero da Linha para que vai: " in read_int()
		else let() = print_string "Digite o numero da Linha para que vai: " in read_int() 
;;
	
let turnoDoJogador matriz = 
	let numeroDeRequisicao = 0 in
		moverPeca matriz (escolhaDaPosicao  numeroDeRequisicao) (escolhaDaPosicao numeroDeRequisicao+1) (escolhaDaPosicao numeroDeRequisicao+2) (escolhaDaPosicao numeroDeRequisicao+3)(*o primeiro eh na linha e depois coluna da linha que ele esta*)
;;
(*
let moverPecaComputadorDeIJParaXY matriz numeroDeRequisicao=
	if  (verificaMovimentoDaPecaDoComputador matriz numeroDeRequisicao-1 numeroDeRequisicao-1) == 1 then 
	else if  (moverPecaDoComputador matriz numeroDeRequisicao-1 numeroDeRequisicao+1) == 1 then 
	else if  (moverPecaDoComputador matriz numeroDeRequisicao-2 numeroDeRequisicao-2) == 1 then 
	else if  (moverPecaDoComputador matriz numeroDeRequisicao-2 numeroDeRequisicao) == 1 then 
	else if  (moverPecaDoComputador matriz numeroDeRequisicao-2 numeroDeRequisicao+2) == 1 then 
;;
*)
let verificarPosicaoNaLinhaRetornadoOLugar lista i j =
	match lista with	
	| []->[]
	| hd::ht -> if hd="B" then  i::j::[] 
				else verificarPosicaoNaLinhaRetornadoOLugar ht i j+1
;;
	
let verificarPosicaoMaisAFrenteEMeRetornarAParOrdenadoDoLugar matriz i j =
	match matriz with
		| [] -> []
		| hd::ht -> let lista = verificarPosicaoNaLinhaRetornadoOLugar hd i j in 
						if lista !=[] then lista 
						else verificarPosicaoMaisAFrenteEMeRetornarAParOrdenadoDoLugar ht i+1 j
;;

let verificarPosicaoNaLinha linha j lista =
	match linha with
		| []-> []
		| hd::ht -> if hd="B" then 1
					else verificarPosicaoNaLinha ht j+1 lista
;;

let verificarPosicoesMaisAFrente matriz i j lista = 
	match matriz with	
		| []->0
		| hd::ht -> if (verificarPosicaoNaLinha hd j i::[] = 1) then i::j::[]
					else (verificarPosicoesMaisAFrente ht i+1 j lista = 1)
;;
					
let moverPecaDoComputadordeIJParaXY matriz i j x y =  substituirPeca (substituirPecaDeIJ matriz [] i j "1") [] x y "B"
;;
	
let moverPecaComputador matriz =
	let parOrdenado = verificarPosicaoMaisAFrenteEMeRetornarAParOrdenadoDoLugar matriz 1 1 in
			match parOrdenado with 
				| [] ->[]
				| hd::ht -> moverPecaDoComputadordeIJParaXY matriz hd ht hd+1 ht+1
;;

let turnoDoComputador matriz = 
	let numeroDeRequisicao = 0 in
		moverPecaComputador matriz ; 
;;

let rec turno matriz numeroDeTurno = 
	if numeroDeTurno mod 2 = 0 then
		turno (turnoDoJogador matriz) numeroDeTurno+1
	else turno (turnoDoComputador matriz) numeroDeTurno+1(*Como turno tem que ser recursivo ou chamonturno do jogador e do pc tendo o retonro de matriz*)
;;

let rec criarMatrizInicial matriz lista = 
  match matriz with
    | []-> lista
    | hd::ht -> hd :: criarMatrizInicial ht lista
;;

let damas = 
	let matrizOriginal = criarMatrizInicial [] [["0";"B";"0";"B";"0";"B";"0";"B";];["B";"0";"B";"0";"B";"0";"B";"0";]; ["0";"B";"0";"B";"0";"B";"0";"B";];["1";"0";"1";"0";"1";"0";"1";"0";];["0";"1";"0";"1";"0";"1";"0";"1";];["P";"0";"P";"0";"P";"0";"P";"0";];["0";"P";"0";"P";"0";"P";"0";"P";];["P";"0";"P";"0";"P";"0";"P";"0";];]
	in turno matrizOriginal 0
;;
	  
damas;;