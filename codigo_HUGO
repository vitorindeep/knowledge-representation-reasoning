%-----------------------------------------------------------------------------------------------------
% SIST. REPR. CONHECIMENTO E RACIOCINIO - MiEI/3
% Trabalho prático nº1

%-----------------------------------------------------------------------------------------------------
% SICStus PROLOG: Declaracoes iniciais

:- set_prolog_flag( discontiguous_warnings,off ).
:- set_prolog_flag( single_var_warnings,off ).
:- set_prolog_flag( unknown,fail ).


%-----------------------------------------------------------------------------------------------------
% SICStus PROLOG: definicoes iniciais

:- op( 900,xfy,'::' ).

% Para os invariantes:

:- dynamic utente/4.
:- dynamic cuidadoPrestado/4.
:- dynamic atoMedico/4.

%-----------------------------------------------------------------------------------------------------
% Extensao do predicado utente: IdU,Nome,Idade,Morada -> {V,F}

utente(1,hugo,20,cerveira).
utente(2,alexandre,21,povoadevarzim).
utente(3,luis,20,pontedelima).
utente(4,francisco,33,braga).
utente(5,antonio,5,guimaraes).
utente(6,anarita,25,pontedelima).
utente(7,marta,65,guimaraes).
utente(8,paulo,15,barcelos).
utente(9,josefina,42,barcelos).
utente(10,luis,52,vizela).


%-----------------------------------------------------------------------------------------------------
% Extensao do predicado cuidadoPrestado: IdC,Descricao,Instituicao,Cidade -> {V,F}

cuidadoPrestado(1, pediatria, cscerveira, vncerveira).
cuidadoPrestado(2, cardiologia, hospitalbraga, braga).
cuidadoPrestado(3, cirurgia, hospitalbraga, braga).
cuidadoPrestado(4, ginecologia, hospitalbraga, braga).
cuidadoPrestado(5, neurologia, hsmm, barcelos).
cuidadoPrestado(6, psiquiatria, hsog, guimaraes).
cuidadoPrestado(7, oftamologia, hospitaldaluz, povoadevarzim).


%-----------------------------------------------------------------------------------------------------
% Extensao do predicado atoMedico: Data,IdU,IdC,Custo -> {V,F}

atoMedico(01-01-2017, 1, 6, 15).
atoMedico(13-01-2017, 3, 4, 30).
atoMedico(13-01-2017, 2, 5, 30).
atoMedico(14-01-2017, 2, 7, 8).
atoMedico(20-01-2017, 4, 2, 20).
atoMedico(02-02-2017, 7, 4, 5).
atoMedico(03-02-2017, 3, 5, 24).
atoMedico(20-02-2017, 6, 7, 37).
atoMedico(22-02-2017, 6, 2, 55).
atoMedico(04-03-2017, 2, 2, 98).
atoMedico(15-03-2017, 1, 1, 5).



% /////////////////////////// Ponto 1 ////////////////////////

%-----------------------------------------------------------------------------------------------------
% Invariante que não permite a inserção de conhecimento de um utente com um id já existente

+utente(Id, N, I, M) :: (solucoes((Id, N), utente(Id, X, Y, Z), S),
				    	  comprimento(S, L),
				 	      L == 1).


%-----------------------------------------------------------------------------------------------------
% Invariante que não permite a inserção de conhecimento de um cuidadoPrestado com um id já existente

+cuidadoPrestado(Id,D,I,C) :: (solucoes((Id, D), cuidadoPrestado(Id, X, Y, Z), S),
				    	 	   comprimento(S, L),
				 	           L == 1).

%-----------------------------------------------------------------------------------------------------
% Invariante que não permite a inserção de conhecimento de um atoMedico quando o id do utente/cuidadoPrestado
% não existem na base de conhecimento 

+atoMedico(D,U,S,C) :: (solucoes((S, Xs), cuidadoPrestado(S, Xs, Ys, Zs), Servs),
				    	comprimento(Servs, Ls),
				 	    Ls == 1,
						solucoes((U, Xu), utente(U, Xu, Yu, Zu), Uts),
				    	comprimento(Uts, Lu),
				 	    Lu == 1).




% /////////////////////////// Ponto 2 ////////////////////////


%-----------------------------------------------------------------------------------------------------
% Extensao do predicado utentesPNome: Nome,Resultado -> {V,F}

utentesPNome(Nome,Resultado) 
	:- findall( (IdU,Nome,Idade,Morada), utente(IdU,Nome,Idade,Morada), Resultado ).

%-----------------------------------------------------------------------------------------------------
% Extensao do predicado utentesPIdade: Idade,Resultado -> {V,F}

utentesPIdade(Idade,Resultado) 
	:- findall( (IdU,Nome,Idade,Morada), utente(IdU,Nome,Idade,Morada), Resultado ).

%-----------------------------------------------------------------------------------------------------
% Extensao do predicado utentesPMorada: Morada,Resultado -> {V,F}

utentesPMorada(Morada,Resultado) 
	:- findall( (IdU,Nome,Idade,Morada), utente(IdU,Nome,Idade,Morada), Resultado ).

%-----------------------------------------------------------------------------------------------------
% Extensao do predicado utentesQGastaramMaisQX: Valor,Resultado -> {V,F}

utentesQGastaramMaisQX(Valor,R1) 
	:- findall( (IdU,Custo), atoMedico(Data,IdU,IdC,Custo), Resultado), 
	utentesQGastaramMaisQXAux(Resultado,Valor,R),removeDup(R,R1).

% Extensao do predicado utentesQGastaramMaisQXAux: Lista,Valor,Resultado -> {V,F}

utentesQGastaramMaisQXAux([],Valor,[]).
utentesQGastaramMaisQXAux([(IdU,Custo)|T],Valor,L):- Custo =< Valor,utentesQGastaramMaisQXAux(T,Valor,L).
utentesQGastaramMaisQXAux([(IdU,Custo)|T],Valor,[IdU|L]):- Custo > Valor,utentesQGastaramMaisQXAux(T,Valor,L).

%-----------------------------------------------------------------------------------------------------
% Extensao do predicado ListarUtentesMaisFreq: Resultado -> {V,F}

listarUtentesMaisFreq(Resultado):- findall( IdU, utente(IdU,Nome,Idade,Morada), R ),
									findall( IdU, atoMedico(Data,IdU,IdC,Custo), R2 ),
									listarUtentesMaisFreqAux(R,R2,R3),
									ordenarDecresc(R3,Resultado).

% Extensao do predicado listarUtentesMaisFreqAux: Utentes,Resultado -> {V,F}
% Esta funcao pega em cada elemento da 1ª lista e verifica quantas vezes aparece na segunda, no fim devolve um par com cada elemento e o nr de vezes q apareceu.

listarUtentesMaisFreqAux([],L,[]).
listarUtentesMaisFreqAux([H|T], L, [(H,Q)|R]):- quantosTem(H,L,Q), listarUtentesMaisFreqAux(T,L,R).

% /////////////////////////// Ponto 3 ////////////////////////


% ----------------------------------------------------------------------------------------------------
% Extensao do predicado instituicoes: Valor,Resultado -> {V,F}

instituicoes(Resultado) 
	:- findall( Instituicao, cuidadoPrestado(IdC,Descricao,Instituicao,Cidade), R ), removeDup(R,Resultado).

% /////////////////////////// Ponto 4 ////////////////////////

% ----------------------------------------------------------------------------------------------------
% Extensao do predicado getCuidadosbyInstituicao: I,R -> {V,F}

getCuidadosbyInstituicao(I,S) :- findall((Ss,Desc,I,Cid), cuidadoPrestado(Ss,Desc,I,Cid), S).


% Extensao do predicado getCuidadosbyCidade: C,R -> {V,F}

getCuidadosbyCidade(C,S) :- findall((Ss,Desc,Ins,C),cuidadoPrestado(Ss,Desc,Ins,C),S).


% /////////////////////////// Ponto 5 ////////////////////////

% ----------------------------------------------------------------------------------------------------
% Extensao do predicado getUtBySer: D,R -> {V,F}

getUtBySer(D,R) :-
        findall(Ss , cuidadoPrestado(Ss,D,_,_), R1 ), 
        getUtBySerAux(R1,R2),
        getUt(R2,R).


% Extensao do predicado getUtBySerAux: Lista,Lista -> {V,F}

getUtBySerAux([],[]).
getUtBySerAux([IdC|Y],R) :- 
		findall(IdU, atoMedico(_,IdU,IdC,_), R1),
 		getUtBySerAux(Y,R2),
 		concatena(R1,R2,R).

% ----------------------------------------------------------------------------------------------------
% Extensao do predicado getUtByIns: I,R -> {V,F}

getUtByIns(I,R):- findall(Ss ,cuidadoPrestado(Ss,_,I,_), R1), 
				  getUtByInsAux(R1,R2),
				  getUt(R2,R).

% Extensao do predicado getUtByInsAux: Lista,Lista -> {V,F}

getUtByInsAux([],[]).
getUtByInsAux([IdC|Y],R) :- findall(IdU, atoMedico(_,IdU,IdC,_), R1),
 							getUtByInsAux(Y,R2), concatena(R1,R2,R).


% Extensao do predicado getUtByInsAux: Lista,Lista -> {V,F}

getUt([],[]).
getUt([IdU|Y],R) :- findall((IdU,Nome,Idade,Morada), utente(IdU,Nome,Idade,Morada), R1),
 					getUt(Y,R2), concatena(R1,R2,R).

% /////////////////////////// Ponto 6 ////////////////////////

% ----------------------------------------------------------------------------------------------------
% Extensao do predicado getAtosBySer: S,R -> {V,F}

getAtosBySer(S,R) :- findall(Ss ,cuidadoPrestado(Ss,S,_,_), R1), getAtosBySerAux(R1,R).

% Extensao do predicado getAtosBySerAux: L,L -> {V,F}
getAtosBySerAux([],[]).
getAtosBySerAux([IdC|Y],R) :- findall((Data,IdU,IdC,Custo), atoMedico(Data,IdU,IdC,Custo), R1),
 						   	  getAtosBySerAux(Y,R2), concatena(R1,R2,R).


% ----------------------------------------------------------------------------------------------------
% Extensao do predicado getAtosByIns: I,R -> {V,F}

getAtosByIns(I,R):- findall(Ss ,cuidadoPrestado(Ss,_,I,_), R1), getAtosByInsAux(R1,R).

% Extensao do predicado getAtosBySer: L,L -> {V,F}

getAtosByInsAux([],[]).
getAtosByInsAux([IdC|Y],R) :- findall((Data,IdU,IdC,Custo), atoMedico(Data,IdU,IdC,Custo), R1),
 							  getAtosByInsAux(Y,R2), concatena(R1,R2,R).

% ----------------------------------------------------------------------------------------------------
% Extensao do predicado getAtosByUt: U,R -> {V,F}

getAtosByUt(U,R) :-
        findall((D,U,S,C),atoMedico(D,U,S,C),R).


% /////////////////////////// Ponto 7 ////////////////////////

% ----------------------------------------------------------------------------------------------------
% Extensao do predicado getInstSerByUt: Id,Resultado -> {V,F}

getInstSerByUt(IdU,Resultado2) :-
		findall(IdC , atoMedico(Data,IdU,IdC,Custo), R ),removeDup(R,Resultado), getInstSerByUtAux(Resultado,Resultado2).

% Extensao do predicado getInstByUtAux: L,Resultado -> {V,F}

getInstSerByUtAux([],[]).
getInstSerByUtAux([IdC|T],[(Instituicao,Descricao)|Resto]) :- cuidadoPrestado(IdC,Descricao,Instituicao,Cidade),
										getInstSerByUtAux(T,Resto).


% /////////////////////////// Ponto 8 ////////////////////////

% ----------------------------------------------------------------------------------------------------
% Extensao do predicado getTotalByUtente: IdUtente, Total -> {V,F}

getTotalByUtente(Id,T) :-
		solucoes(C, atoMedico(D, Id, S, C), TotUtente),
		somatorio(TotUtente,T).

% ----------------------------------------------------------------------------------------------------
% Extensao do predicado getTotalByServiço: IdServiço, Total -> {V,F}

getTotalByServico(Id,T) :-
		solucoes(C, atoMedico(D, U, Id, C), TotServico),
		somatorio(TotServico,T).

% ----------------------------------------------------------------------------------------------------
% Extensao do predicado getTotalByData: Data, Total -> {V,F}

getTotalByData(D,T) :-
		solucoes(C, atoMedico(D, U, S, C), TotData),
		somatorio(TotData,T).

% ----------------------------------------------------------------------------------------------------
% Extensao do predicado getTotalByInstituicao: Instituicao, Total -> {V,F}

getTotalByInstituicao(I,T) :-
		solucoes(Id, cuidadoPrestado(Id, D, I, C), Servs),
		getTotalListServs(Servs,T).

getTotalListServs([],T) :- 
		T is 0.
getTotalListServs([X|Y], T) :-
		getTotalListServs(Y,Z),
		getTotalByServico(X,R),
		T is Z+R.

% /////////////////////////// Ponto 9 ////////////////////////

%-----------------------------------------------------------------------------------------------------
% Invariante que não permite a remocao de conhecimento de um utente não presente na base de conhecimento
% e com id associado a atoMedico

-utente(Id,N,I,M) :: (solucoes((Id), utente(Id,N,I,M), Uts),
					  comprimento(Uts, Lu),
					  Lu == 1,
					  solucoes((Id), atoMedico(X, Id, Y, Z), R),
				      comprimento(R, L),
				 	  L == 0).

%-----------------------------------------------------------------------------------------------------
% Invariante que não permite a remocao de conhecimento de um cuidadoPrestado não presente na base de 
% conhecimento e com o id associado a atoMedico

-cuidadoPrestado(Id, D, I, C) :: (solucoes((Id), cuidadoPrestado(Id,D,I,C), Servs),
					  			  comprimento(Servs, Lc),
					              Lc == 1,
								  solucoes((Id), atoMedico(X, Y, Id, Z), R),
				    	  		  comprimento(R, L),
				 	      		  L == 0).

%-----------------------------------------------------------------------------------------------------
% Invariante que não permite a remocao de conhecimento de um atoMedico não presente na base de conhecimento

-atoMedico(D,U,S,C) :: (solucoes((D,U,S,C), atoMedico(D,U,S,C), A),
					    comprimento(A, L),
					    L == 1).



% /////////////////////////// Funcoes auxiliar ////////////////////////

% ----------------------------------------------------------------------------------------------------
% Extensão do predicado registar: Termo -> {V,F}
 
registar(Termo) :-
    findall(Invariante, +Termo::Invariante, Lista),
    insercao(Termo),
    testar(Lista).

% ----------------------------------------------------------------------------------------------------
% Extensão do predicado insercao: Termo -> {V,F}

insercao(T) :-
    assert(T).
insercao(T) :-
    retract(T), !, fail.

% ----------------------------------------------------------------------------------------------------
% Extensão do predicado remover: Termo -> {V,F}

remover(Termo) :-
    findall(Invariante, -Termo::Invariante, Lista),
    testar(Lista),
    remocao(Termo).

% ----------------------------------------------------------------------------------------------------
% Extensão do predicado remocao: Termo -> {V,F}

remocao(T) :-
    retract(T).
remocao(T) :-
    assert(T), !, fail.

% ----------------------------------------------------------------------------------------------------
% Extensão do predicado solucoes: X,Y,Z -> {V,F}

solucoes(X,Y,Z) :-
    findall(X,Y,Z).
 
 % ----------------------------------------------------------------------------------------------------
% Extensão do predicado comprimento: Lista, Resultado -> {V,F}

comprimento(X, Z) :-
    length(X, Z).

% ----------------------------------------------------------------------------------------------------
% Extensão do predicado testar: Lista -> {V,F}

testar([]).
testar([I|L]) :-
    I,
    testar(L).

% ----------------------------------------------------------------------------------------------------
% Extensao do predicado somatorio: lista, resultado -> {V,F}

somatorio([], 0). 
somatorio([X|Y], R) :-
	somatorio(Y,G),
	R is X+G.

% Extensao do predicado nao: Q -> {V,F}

nao(Q):- Q, !, fail.
nao(Q).

% Extensao do predicado quantosTem:A,Lista,Resultado  -> {V,F}

quantosTem(A,[],0).
quantosTem(A,[H|T],Resultado):- (A == H), quantosTem(A,T,R), Resultado is R+1.
quantosTem(A,[H|T],Resultado):- (A \= H), quantosTem(A,T,Resultado).

% Extensao do predicado pertence: X,L -> {V,F}

pertence(X,[]) :- fail.
pertence(X,[X|T]):- X==X.
pertence(X,[H|T]) :- X\=H, pertence(X,T).

% Extensao do predicado removeDup: L,R -> {V,F}

removeDup([], []).
removeDup([X|T], R) :- pertence(X,T),
					   removeDup(T, R).
removeDup([X|T], [X|R]) :- nao(pertence(X, T)),
						   removeDup(T, R).	

% Extensao do predicado ordenarDecresc: L,Resultado -> {V,F}

ordenarDecresc([X],[X]).
ordenarDecresc([X|Y],T):- 
	ordenarDecresc(Y,R),insereOrdenado(X,R,T).

% Extensao do predicado insereOrdenado: X,L,Resultado  -> {V,F}

insereOrdenado((X1,Y1), [], [(X1,Y1)]).
insereOrdenado((X1,Y1), [(X2,Y2)|Z], [(X1,Y1)|[(X2,Y2)|Z]]) :- Y1>Y2.
insereOrdenado((X1,Y1), [(X2,Y2)|Z], [(X2,Y2)|R2]) :- Y1=<Y2,
	insereOrdenado((X1,Y1),Z,R2).

% Extensao do predicado concatena(L1,L2,L3)->{V,F}

concatena([],L2,L2).
concatena([X|L1],L2,[X|L]) :- concatena(L1,L2,L).