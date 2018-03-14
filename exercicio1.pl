%-----------------------------------------------------------------------------------------------------
% SIST. REPR. CONHECIMENTO E RACIOCINIO - MiEI/3
% Trabalho prático nş1

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

% FEITO VITOR
% //////////////////////////////////////////////// Ponto 1 ///////////////////////////////////////////
%-----------------------------------------------------------------------------------------------------
% Invariante que năo permite a inserçăo de conhecimento de um utente com um id já existente
% uso _ quando o dado năo é importante e năo quero saber dele
+utente(Id, N, I, M) :: (solucoes(Id, utente(Id,_,_,_), S),
						comprimento(S,L),
						L==1).

%-----------------------------------------------------------------------------------------------------
% Invariante que năo permite a inserçăo de conhecimento de um prestador com um id já existente

+prestador(Id, N, E, I) :: (solucoes(Id, prestador(Id,_,_,_), S),
							comprimento(S, L),
							L == 1).

%-----------------------------------------------------------------------------------------------------
% Invariante que năo permite a inserçăo de conhecimento de um cuidado quando o id do utente/prestador
% năo existem na base de conhecimento

+cuidado(Dat,U,P,D,C) :: (solucoes(P, prestador(P, _, _, _), LisP),
						comprimento(LisP, NumP),
						NumP == 1,
						solucoes(U, utente(U, _, _, _), LisU),
						comprimento(LisU, NumU),
						NumU == 1).

% FEITO VITOR
% //////////////////////////////////////////////// Ponto 2 ///////////////////////////////////////////
%-----------------------------------------------------------------------------------------------------
% Invariante que năo permite a remocao de conhecimento de um utente năo presente na base de conhecimento
% e com id associado a cuidado

-utente(Id, N, I, M) :: (solucoes(Id, utente(Id,N,I,M), Uts),
						comprimento(Uts, Lu),
						Lu==1,
						solucoes(Id, cuidado(_,Id,_,_,_), Cuids),
						comprimento(Cuids, Lc),
						Lc == 0).

%-----------------------------------------------------------------------------------------------------
% Invariante que năo permite a remocao de conhecimento de um prestador năo presente na base de
% conhecimento e com o id associado a cuidado

-prestador(Id, N, E, I) :: (solucoes(Id, prestador(Id,_,_,_), Prests),
							comprimento(Prests, Lp),
							Lp == 1,
							solucoes(Id, cuidado(_,_,Id,_,_), Cuids),
							comprimento(Cuids, Lc),
							Lc == 0).

%-----------------------------------------------------------------------------------------------------
% Invariante que năo permite a remocao de conhecimento de um cuidado năo presente na base de conhecimento

-cuidado(Dat, U, P, D, C) :: (solucoes((Dat,U,P,D,C), cuidado(Dat,U,P,D,C), Cuids),
							comprimento(Cuids, L),
							L == 1).

% FEITO VITOR
% //////////////////////////////////////////////// Ponto 3 ///////////////////////////////////////////
%-----------------------------------------------------------------------------------------------------
% Extensao do predicado utentesPNome: Nome,Lis -> {V,F}

utentesPNome(Nome, Lis) :-
	solucoes((IdUt,Nome,Idade,Morada), utente(IdUt,Nome,Idade,Morada), Lis).

%-----------------------------------------------------------------------------------------------------
% Extensao do predicado utentesPIdade: Idade,Lis -> {V,F}

utentesPIdade(Idade, Lis) :-
	solucoes((IdUt,Nome,Idade,Morada), utente(IdUt,Nome,Idade,Morada), Lis).

%-----------------------------------------------------------------------------------------------------
% Extensao do predicado utentesPMorada: Morada,Lis -> {V,F}

utentesPMorada(Morada,Lis) :-
	solucoes((IdUt,Nome,Idade,Morada), utente(IdUt,Nome,Idade,Morada), Lis).

% DAQUI PARA BAIXO É O QUE ELES TINHAM A MAIS (N É PRECISO PARA O PT 3)
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
% Esta funcao pega em cada elemento da 1Ş lista e verifica quantas vezes aparece na segunda, no fim devolve um par com cada elemento e o nr de vezes q apareceu.

listarUtentesMaisFreqAux([],L,[]).
listarUtentesMaisFreqAux([H|T],L,[(H,Q)|R]):-
	quantosTem(H,L,Q),listarUtentesMaisFreqAux(T,L,R).

% FEITO VITOR
% //////////////////////////////////////////////// Ponto 4 ///////////////////////////////////////////
% ----------------------------------------------------------------------------------------------------
% Extensao do predicado instituicoes: Resultado -> {V,F}

instituicoes(Resultado) :-
	solucoes(Instituicao, prestador(_,_,_,Instituicao), Insts),
	removeDup(Insts,Resultado).

% DAQUI PARA BAIXO NĂO MEXI, É TUDO DO HUGO
% PONTO 5 -> diana
% PONTO 6,7 -> carlos
% PONTO 8,9 -> MARCOS

% //////////////////////////////////////////////// Ponto ? ///////////////////////////////////////////
% ----------------------------------------------------------------------------------------------------
% Extensao do predicado getCuidadosbyInstituicao: I,R -> {V,F}

getCuidadosbyInstituicao(I,S) :-
	solucoes((Ss,Desc,I,Cid),prestador(Ss,Desc,I,Cid),S).


% Extensao do predicado getCuidadosbyCidade: C,R -> {V,F}

getCuidadosbyCidade(C,S) :-
	solucoes((Ss,Desc,Ins,C),prestador(Ss,Desc,Ins,C),S).

% //////////////////////////////////////////////// Ponto ? ///////////////////////////////////////////
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

% //////////////////////////////////////////////// Ponto ? ///////////////////////////////////////////
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

% //////////////////////////////////////////////// Ponto ? ///////////////////////////////////////////
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

% //////////////////////////////////////////////// Ponto ? ///////////////////////////////////////////
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

% ////////////////////////////////////////// Funçőes auxiliares //////////////////////////////////////
% ----------------------------------------------------------------------------------------------------
% Extensăo do predicado evoluacao: Termo -> {V,F}

evoluacao(Termo) :-
	solucoes(Invariante, +Termo::Invariante, Lista),
	insercao(Termo),
	teste(Lista).

% ----------------------------------------------------------------------------------------------------
% Extensăo do predicado insercao: Termo -> {V,F}

insercao(T) :-
	assert(T).
insercao(T) :-
	retract(T),!,fail.

% ----------------------------------------------------------------------------------------------------
% Extensăo do predicado involucao: Termo -> {V,F}

involucao(Termo) :-
	solucoes(Invariante, -Termo::Invariante, Lista),
	teste(Lista),
	remocao(Termo).

% ----------------------------------------------------------------------------------------------------
% Extensăo do predicado remocao: Termo -> {V,F}

remocao(T) :-
	retract(T).
remocao(T) :-
	assert(T),!,fail.

% ----------------------------------------------------------------------------------------------------
% Extensăo do predicado teste: Lista -> {V,F}

teste([]).
teste([I|L]) :-
	I,
	teste(L).

% ----------------------------------------------------------------------------------------------------
% Extensăo do predicado solucoes: X,Y,Z -> {V,F}

solucoes(X,Y,Z) :-
	findall(X,Y,Z).

% ----------------------------------------------------------------------------------------------------
% Extensăo do predicado comprimento: Lista, Resultado -> {V,F}

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
