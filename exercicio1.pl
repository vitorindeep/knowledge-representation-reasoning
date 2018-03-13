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

:- op(900,xfy,'::').

% Para os invariantes:

:- dynamic utente/4.
:- dynamic prestador/4.
:- dynamic cuidado/5.

%-----------------------------------------------------------------------------------------------------
% Extensao do predicado utente: IdUt,Nome,Idade,Morada -> {V,F}

utente(1,carlos,21,famalicao).
utente(2,diana,20,trofa).
utente(3,marcos,20,anais).
utente(4,vitor,20,guimaraes).
utente(5,antonio,6,povoadevarzim).
utente(6,rita,26,pontedelima).
utente(7,marta,66,guimaraes).
utente(8,paulo,16,barcelos).
utente(9,maria,43,barcelos).
utente(10,luis,51,vizela).

%-----------------------------------------------------------------------------------------------------
% Extensao do predicado prestador: IdPrest,Nome,Especialidade,Instituicao -> {V,F}

prestador(1, pres1, pediatria, csjoane).
prestador(2, pres2, cardiologia, hospitalbraga).
prestador(3, pres3, cirurgia, hospitalbraga).
prestador(4, pres4, ginecologia, hospitalbraga).
prestador(5, pres5, neurologia, hsmm).
prestador(6, pres6, psiquiatria, hsog).
prestador(7, pres7, oftamologia, htrofa).

%-----------------------------------------------------------------------------------------------------
% Extensao do predicado cuidado: Data,IdUt,IdPrest,Descricao,Custo -> {V,F}

cuidado(01-02-2017, 1, 6, hipnose, 15).
cuidado(13-02-2017, 3, 4, papanico, 30).
cuidado(13-02-2017, 2, 5, cerebrotroca, 30).
cuidado(14-02-2017, 2, 7, olhoterapia, 7).
cuidado(20-03-2017, 4, 2, pacemaker, 20).
cuidado(02-04-2017, 7, 4, ovariologia, 5).
cuidado(03-04-2017, 3, 5, neuroterapia, 25).
cuidado(20-04-2017, 6, 7, retinoterapia, 35).
cuidado(22-05-2017, 6, 2, cardioterapia, 55).
cuidado(04-06-2017, 2, 2, cardiograma, 99).
cuidado(15-06-2017, 1, 1, terapiafala, 5).

% //////////////////////////////////////////////// Ponto 1 ///////////////////////////////////////////
%-----------------------------------------------------------------------------------------------------
% Invariante que não permite a inserção de conhecimento de um utente com um id já existente

+utente(Id, N, I, M) :: (solucoes((Id,N),utente(Id,X,Y,Z),S),
						comprimento(S,L),
						L==1).

%-----------------------------------------------------------------------------------------------------
% Invariante que não permite a inserção de conhecimento de um prestador com um id já existente

+prestador(Id, D, I,C) :: (solucoes((Id, D), prestador(Id, X, Y, Z), S),
								comprimento(S, L),
								L == 1).

%-----------------------------------------------------------------------------------------------------
% Invariante que não permite a inserção de conhecimento de um cuidado quando o id do utente/prestador
% não existem na base de conhecimento 

+cuidado(D,U,S,C) :: (solucoes((S, Xs), prestador(S, Xs, Ys, Zs), Servs),
						comprimento(Servs, Ls),
						Ls == 1,
						solucoes((U, Xu), utente(U, Xu, Yu, Zu), Uts),
						comprimento(Uts, Lu),
						Lu == 1).

% //////////////////////////////////////////////// Ponto 2 ///////////////////////////////////////////
%-----------------------------------------------------------------------------------------------------
% Extensao do predicado utentesPNome: Nome,Resultado -> {V,F}

utentesPNome(Nome,Resultado) :- 
	solucoes((IdUt,Nome,Idade,Morada),
	utente(IdUt,Nome,Idade,Morada),Resultado).

%-----------------------------------------------------------------------------------------------------
% Extensao do predicado utentesPIdade: Idade,Resultado -> {V,F}

utentesPIdade(Idade,Resultado) :-
	solucoes((IdUt,Nome,Idade,Morada),
	utente(IdUt,Nome,Idade,Morada),Resultado).

%-----------------------------------------------------------------------------------------------------
% Extensao do predicado utentesPMorada: Morada,Resultado -> {V,F}

utentesPMorada(Morada,Resultado) :-
	solucoes((IdUt,Nome,Idade,Morada),
	utente(IdUt,Nome,Idade,Morada),Resultado).

%-----------------------------------------------------------------------------------------------------
% Extensao do predicado utentesQGastaramMaisQX: Valor,Resultado -> {V,F}

utentesQGastaramMaisQX(Valor,R1) :-
	solucoes((IdUt,Custo),cuidado(Data,IdUt,IdPrest,Custo),Resultado),
	utentesQGastaramMaisQXAux(Resultado,Valor,R),
	removeDup(R,R1).

% Extensao do predicado utentesQGastaramMaisQXAux: Lista,Valor,Resultado -> {V,F}

utentesQGastaramMaisQXAux([],Valor,[]).
utentesQGastaramMaisQXAux([(IdUt,Custo)|T],Valor,L):-
	Custo=<Valor,utentesQGastaramMaisQXAux(T,Valor,L).
utentesQGastaramMaisQXAux([(IdUt,Custo)|T],Valor,[IdUt|L]):-
	Custo>Valor,utentesQGastaramMaisQXAux(T,Valor,L).

%-----------------------------------------------------------------------------------------------------
% Extensao do predicado ListarUtentesMaisFreq: Resultado -> {V,F}

listarUtentesMaisFreq(Resultado) :-
	solucoes(IdUt,utente(IdUt,Nome,Idade,Morada),R),
	solucoes(IdUt,cuidado(Data,IdUt,IdPrest,Custo),R2),
	listarUtentesMaisFreqAux(R,R2,R3),
	ordenarDecresc(R3,Resultado).

% Extensao do predicado listarUtentesMaisFreqAux: Utentes,Resultado -> {V,F}
% Esta funcao pega em cada elemento da 1ª lista e verifica quantas vezes aparece na segunda, no fim devolve um par com cada elemento e o nr de vezes q apareceu.

listarUtentesMaisFreqAux([],L,[]).
listarUtentesMaisFreqAux([H|T],L,[(H,Q)|R]):-
	quantosTem(H,L,Q),listarUtentesMaisFreqAux(T,L,R).

% //////////////////////////////////////////////// Ponto 3 ///////////////////////////////////////////
% ----------------------------------------------------------------------------------------------------
% Extensao do predicado instituicoes: Valor,Resultado -> {V,F}

instituicoes(Resultado) :-
	solucoes(Instituicao,prestador(IdPrest,Especialidade,Instituicao,Cidade),R),
	removeDup(R,Resultado).

% //////////////////////////////////////////////// Ponto 4 ///////////////////////////////////////////
% ----------------------------------------------------------------------------------------------------
% Extensao do predicado getCuidadosbyInstituicao: I,R -> {V,F}

getCuidadosbyInstituicao(I,S) :-
	solucoes((Ss,Desc,I,Cid),prestador(Ss,Desc,I,Cid),S).


% Extensao do predicado getCuidadosbyCidade: C,R -> {V,F}

getCuidadosbyCidade(C,S) :-
	solucoes((Ss,Desc,Ins,C),prestador(Ss,Desc,Ins,C),S).

% //////////////////////////////////////////////// Ponto 5 ///////////////////////////////////////////
% ----------------------------------------------------------------------------------------------------
% Extensao do predicado getUtBySer: D,R -> {V,F}

getUtBySer(D,R) :-
    solucoes(Ss,prestador(Ss,D,_,_),R1), 
    getUtBySerAux(R1,R2),
    getUt(R2,R).

% Extensao do predicado getUtBySerAux: Lista,Lista -> {V,F}

getUtBySerAux([],[]).
getUtBySerAux([IdPrest|Y],R) :-
	solucoes(IdUt, cuidado(_,IdUt,IdPrest,_), R1),
 	getUtBySerAux(Y,R2),
	concatena(R1,R2,R).

% ----------------------------------------------------------------------------------------------------
% Extensao do predicado getUtByIns: I,R -> {V,F}

getUtByIns(I,R) :-
	solucoes(Ss ,prestador(Ss,_,I,_), R1), 
	getUtByInsAux(R1,R2),
	getUt(R2,R).

% Extensao do predicado getUtByInsAux: Lista,Lista -> {V,F}

getUtByInsAux([],[]).
getUtByInsAux([IdPrest|Y],R) :-
	solucoes(IdUt, cuidado(_,IdUt,IdPrest,_), R1),
	getUtByInsAux(Y,R2),
	concatena(R1,R2,R).

% Extensao do predicado getUtByInsAux: Lista,Lista -> {V,F}

getUt([],[]).
getUt([IdUt|Y],R) :-
	solucoes((IdUt,Nome,Idade,Morada),utente(IdUt,Nome,Idade,Morada),R1),
 	getUt(Y,R2),
	concatena(R1,R2,R).

% //////////////////////////////////////////////// Ponto 6 ///////////////////////////////////////////
% ----------------------------------------------------------------------------------------------------
% Extensao do predicado getAtosBySer: S,R -> {V,F}

getAtosBySer(S,R) :-
	solucoes(Ss,prestador(Ss,S,_,_),R1),
	getAtosBySerAux(R1,R).

% Extensao do predicado getAtosBySerAux: L,L -> {V,F}
getAtosBySerAux([],[]).
getAtosBySerAux([IdPrest|Y],R) :-
	solucoes((Data,IdUt,IdPrest,Custo),cuidado(Data,IdUt,IdPrest,Custo),R1),
 	getAtosBySerAux(Y,R2),
	concatena(R1,R2,R).

% ----------------------------------------------------------------------------------------------------
% Extensao do predicado getAtosByIns: I,R -> {V,F}

getAtosByIns(I,R):-
	solucoes(Ss,prestador(Ss,_,I,_),R1),
	getAtosByInsAux(R1,R).

% Extensao do predicado getAtosBySer: L,L -> {V,F}

getAtosByInsAux([],[]).
getAtosByInsAux([IdPrest|Y],R):-
	solucoes((Data,IdUt,IdPrest,Custo),cuidado(Data,IdUt,IdPrest,Custo),R1),
	getAtosByInsAux(Y,R2),
	concatena(R1,R2,R).

% ----------------------------------------------------------------------------------------------------
% Extensao do predicado getAtosByUt: U,R -> {V,F}

getAtosByUt(U,R) :-
    solucoes((D,U,S,C),
	cuidado(D,U,S,C),R).

% //////////////////////////////////////////////// Ponto 7 ///////////////////////////////////////////
% ----------------------------------------------------------------------------------------------------
% Extensao do predicado getInstSerByUt: Id,Resultado -> {V,F}

getInstSerByUt(IdUt,Resultado2) :-
	solucoes(IdPrest,cuidado(Data,IdUt,IdPrest,Custo),R),
	removeDup(R,Resultado),
	getInstSerByUtAux(Resultado,Resultado2).

% Extensao do predicado getInstByUtAux: L,Resultado -> {V,F}

getInstSerByUtAux([],[]).
getInstSerByUtAux([IdPrest|T],[(Instituicao,Especialidade)|Resto]) :-
	prestador(IdPrest,Especialidade,Instituicao,Cidade),
	getInstSerByUtAux(T,Resto).

% //////////////////////////////////////////////// Ponto 8 ///////////////////////////////////////////
% ----------------------------------------------------------------------------------------------------
% Extensao do predicado getTotalByUtente: IdUtente, Total -> {V,F}

getTotalByUtente(Id,T) :-
	solucoes(C, cuidado(D, Id, S, C), TotUtente),
	somatorio(TotUtente,T).

% ----------------------------------------------------------------------------------------------------
% Extensao do predicado getTotalByServiço: IdServiço, Total -> {V,F}

getTotalByServico(Id,T) :-
	solucoes(C, cuidado(D, U, Id, C), TotServico),
	somatorio(TotServico,T).

% ----------------------------------------------------------------------------------------------------
% Extensao do predicado getTotalByData: Data, Total -> {V,F}

getTotalByData(D,T) :-
	solucoes(C, cuidado(D, U, S, C), TotData),
	somatorio(TotData,T).

% ----------------------------------------------------------------------------------------------------
% Extensao do predicado getTotalByInstituicao: Instituicao, Total -> {V,F}

getTotalByInstituicao(I,T) :-
	solucoes(Id, prestador(Id, D, I, C), Servs),
	getTotalListServs(Servs,T).
getTotalListServs([],T) :- 
	T is 0.
getTotalListServs([X|Y], T) :-
	getTotalListServs(Y,Z),
	getTotalByServico(X,R),
	T is Z+R.

% //////////////////////////////////////////////// Ponto 9 ///////////////////////////////////////////
%-----------------------------------------------------------------------------------------------------
% Invariante que não permite a remocao de conhecimento de um utente não presente na base de conhecimento
% e com id associado a cuidado

-utente(Id, N, I, M) :: (solucoes((Id),utente(Id,N,I,M),Uts),
						comprimento(Uts,Lu),
						Lu==1,
						solucoes((Id),cuidado(X,Id,Y,Z),R),
						comprimento(R,L),
						L==0).

%-----------------------------------------------------------------------------------------------------
% Invariante que não permite a remocao de conhecimento de um prestador não presente na base de 
% conhecimento e com o id associado a cuidado

-prestador(Id, D, I,C) :: (solucoes((Id), prestador(Id,D,I,C), Servs),
								comprimento(Servs, Lc),
								Lc == 1,
								solucoes((Id), cuidado(X, Y, Id, Z), R),
								comprimento(R, L),
								L == 0).

%-----------------------------------------------------------------------------------------------------
% Invariante que não permite a remocao de conhecimento de um cuidado não presente na base de conhecimento

-cuidado(D, U,S,C) :: (solucoes((D,U,S,C), cuidado(D,U,S,C), A),
						comprimento(A, L),
						L == 1).

% ////////////////////////////////////////// Funções auxiliares //////////////////////////////////////
% ----------------------------------------------------------------------------------------------------
% Extensão do predicado evoluacao: Termo -> {V,F}
 
evoluacao(Termo) :-
	solucoes(Invariante, +Termo::Invariante, Lista),
	insercao(Termo),
	teste(Lista).

% ----------------------------------------------------------------------------------------------------
% Extensão do predicado insercao: Termo -> {V,F}

insercao(T) :-
	assert(T).
insercao(T) :-
	retract(T),!,fail.

% ----------------------------------------------------------------------------------------------------
% Extensão do predicado involucao: Termo -> {V,F}

involucao(Termo) :-
	solucoes(Invariante, -Termo::Invariante, Lista),
	teste(Lista),
	remocao(Termo).

% ----------------------------------------------------------------------------------------------------
% Extensão do predicado remocao: Termo -> {V,F}

remocao(T) :-
	retract(T).
remocao(T) :-
	assert(T),!,fail.

% ----------------------------------------------------------------------------------------------------
% Extensão do predicado teste: Lista -> {V,F}

teste([]).
teste([I|L]) :-
	I,
	teste(L).

% ----------------------------------------------------------------------------------------------------
% Extensão do predicado solucoes: X,Y,Z -> {V,F}

solucoes(X,Y,Z) :-
	findall(X,Y,Z).

% ----------------------------------------------------------------------------------------------------
% Extensão do predicado comprimento: Lista, Resultado -> {V,F}

comprimento(X,Z):-
	length(X,Z).

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
quantosTem(A,[H|T],Resultado):-
	(A == H), quantosTem(A,T,R), Resultado is R+1.
quantosTem(A,[H|T],Resultado):-
	(A \= H), quantosTem(A,T,Resultado).

% Extensao do predicado pertence: X,L -> {V,F}

pertence(X,[]):- fail.
pertence(X,[X|T]):- X==X.
pertence(X,[H|T]):-
	X\=H,
	pertence(X,T).

% Extensao do predicado removeDup: L,R -> {V,F}

removeDup([],[]).
removeDup([X|T],R):-
	pertence(X,T),
	removeDup(T,R).
removeDup([X|T],[X|R]):-
	nao(pertence(X,T)),
	removeDup(T,R).	

% Extensao do predicado ordenarDecresc: L,Resultado -> {V,F}

ordenarDecresc([X],[X]).
ordenarDecresc([X|Y],T):- 
	ordenarDecresc(Y,R),
	insereOrdenado(X,R,T).

% Extensao do predicado insereOrdenado: X,L,Resultado  -> {V,F}

insereOrdenado((X1,Y1),[],[(X1,Y1)]).
insereOrdenado((X1,Y1),[(X2,Y2)|Z],[(X1,Y1)|[(X2,Y2)|Z]]):-
	Y1>Y2.
insereOrdenado((X1,Y1), [(X2,Y2)|Z], [(X2,Y2)|R2]) :-
	Y1=<Y2,
	insereOrdenado((X1,Y1),Z,R2).

% Extensao do predicado concatena(L1,L2,L3)->{V,F}

concatena([],L2,L2).
concatena([X|L1],L2,[X|L]) :-
	concatena(L1,L2,L).