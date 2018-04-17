%-----------------------------------------------------------------------------------------------------
% SIST. REPR. CONHECIMENTO E RACIOCINIO - MiEI/3
% Exercicio prático 2

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
:- dynamic recibo/9.


%------------------------------------------OBJETIVOS--------------------------------------------------
- Representar conhecimento positivo e negativo;
- Representar casos de conhecimento imperfeito, pela utilização de valores nulos de todos os tipos estudados;
- Manipular invariantes que designem restrições à inserção e à remoção de conhecimento do sistema;
- Lidar com a problemática da evolução do conhecimento, criando os procedimentos adequados;  ------------- MÉTODO TROLHA OU ENGENHEIRO
- Desenvolver um sistema de inferência capaz de implementar os mecanismos de raciocínio inerentes a estes sistemas.


%-----------------------------------------RESUMO GERAL------------------------------------------------
% A Belem e filha de uma pessoa de que se desconhece a identidade
filho( belem,xpto023 ).
excecao( filho(F,P) ) :-
	filho( F,xpto023 ).

% A Maria e filha do Faria ou do Garcia
excecao( filho(maria,faria) ).
excecao( filho(maria,garcia) ).

% O Julio tem um filho que ninguem pode conhecer
filho( xpto732,julio ).
excecao( filho(F,P) ) :-
	filho( xpto732,P ).
nulointerdito( xpto732 ).



+filho( F,P ) :: (solucoes())

%-----------------------------------------------------------------------------------------------------
% Representaçao de conhecimento positivo
%-----------------------------------------------------------------------------------------------------

%-----------------------------------------------------------------------------------------------------
% Extensao do predicado utente: IdUt,Nome,Idade,Morada -> {V,F}
utente(1, carlos, 21, famalicao).
utente(2, diana, 20, trofa).
utente(3, marcos, 20, anais).
utente(4, vitor, 20, guimaraes).
utente(5, antonio, 6, povoadevarzim).
utente(6, rita, 26, pontedelima).
utente(7, marta, 66, guimaraes).
utente(8, paulo, 16, barcelos).
utente(9, maria, 43, barcelos).
utente(10, luis, 51, vizela).

%-----------------------------------------------------------------------------------------------------
% Extensao do predicado prestador: IdPrest,Nome,Especialidade,Instituicao -> {V,F}
prestador(1, pres1, pediatria, csjoane).
prestador(2, pres2, cardiologia, hospitalbraga).
prestador(3, pres3, cirurgia, hospitalbraga).
prestador(4, pres4, ginecologia, hospitalbraga).
prestador(5, pres5, neurologia, hsmm).
prestador(6, pres6, psiquiatria, hsog).
prestador(7, pres7, oftamologia, htrofa).
prestador(8, pres8, reumatologia, htrofa).
prestador(9, pres9, psiquiatria, hospitalbraga).

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
cuidado(18-06-2017, 2, 8, reumatomagrafia, 350).
cuidado(18-06-2017, 2, 9, cbt, 10).

%-----------------------------------------------------------------------------------------------------
% Extensao do predicado recibo: IdRecibo, IdUt, NomeUt, Morada, Especialidade, Instituicao, Data, Descricao, Custo -> {V,F}
recibo(1, 3, marcos, anais, ginecologia, hospitalbraga, 13-02-2017, papanico, 30).
recibo(2, 4, vitor, guimaraes, cardiologia, hospitalbraga, 20-03-2017, pacemaker, 20).


%-----------------------------------------------------------------------------------------------------
% Representaçao de conhecimento negativo
%-----------------------------------------------------------------------------------------------------



%-----------------------------------------------------------------------------------------------------
% Representar casos de conhecimento imperfeito incerto
%-----------------------------------------------------------------------------------------------------

% Nulo incerto morada
utente(12, mariana, 43, i1).
excecao( utente( A, B, C, D ) ) :-
	utente( A, B, C, i1).

% Nulo incerto nome e especialidade
prestador(11, i2, i3, hospitalbraga).
excecao( prestador( A, B, C, D ) ) :-
	prestador( A, i2, C, D).
excecao( prestador( A, B, C, D ) ) :-
	prestador( A, B, i3, D).

%Nulo incerto (intervalo) custo > 35 e custo < 60
recibo(5, 10, luis, vizela, psiquiatria, hbraga, 02-11-2017, hipnose, i6).
excecao( recibo(A, B, C, D, E, F, G, H, I) ) :-
				recibo(A, B, C, D, E, F, G, H, i6).
recibo(5, 10, luis, vizela, psiquiatria, hbraga, 02-11-2017, hipnose, i6) :-
				i6 > 35, i6 < 60.

%-----------------------------------------------------------------------------------------------------
% Representar casos de conhecimento imperfeito impreciso
%-----------------------------------------------------------------------------------------------------

% Nulo impreciso 23 ou 24 anos
excecao( utente(11,ines,23,lisboa) ).
excecao( utente(11,ines,24,lisboa) ).

% Nulo impreciso hospital de braga ou de são joão
excecao( prestador(10, pres10, dermatologia, hospitalbraga) ).
excecao( prestador(10, pres10, dermatologia, hsaojoao) ).

% Nulo impreciso ao utente 2 ou 3
excecao( cuidado(19-12-2017, 2, 8, penso, 10) ).
excecao( cuidado(19-12-2017, 3, 8, penso, 10) ).

% Nulo impreciso quimioterapia ou radioterapia, custo=50 ou custo=70
excecao( cuidado(12-05-2017, 8, 5, radioterapia, 50) ).
excecao( cuidado(12-05-2017, 8, 5, radioterapia, 70) ).
excecao( cuidado(12-05-2017, 8, 5, quimioterapia, 50) ).
excecao( cuidado(12-05-2017, 8, 5, quimioterapia, 70) ).

% Nulo impreciso recibo 27-03-2017 ou 28-03-2017
excecao( recibo(3, 2, diana, trofa, nutricao, htrofa, 27-03-2017, rotina, 50) ).
excecao( recibo(3, 2, diana, trofa, nutricao, htrofa, 28-03-2017, rotina, 50) ).

%-----------------------------------------------------------------------------------------------------
% Representar casos de conhecimento imperfeito interdito
%-----------------------------------------------------------------------------------------------------

% Nulo interdito descricao
cuidado( 13-08-2017, 10, 7, i4, 45).
excecao( cuidado(A, B, C, D, E) ) :-s
	cuidado(A, B, C, i4, E).
nulointerdito( i4 ).

% Nulo interdito instituicao
recibo( 4, 7, marta, guimaraes, dermatologia, i5, 04-12-2017, peelingquimico, 30 ).
excecao( recibo(A, B, C, D, E, F, G, H, I) ) :-
	recibo(A, B, C, D, E, i5, G, H, I).
nulointerdito( i5 ).

%-----------------------------------------------------------------------------------------------------
% Manipular invariantes que designem restrições à inserção e à remoção de conhecimento do sistema
%-----------------------------------------------------------------------------------------------------

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

%-----------------------------------------------------------------------------------------------------
% Invariante que năo permite a inserçăo de conhecimento de um recibo com um id já existente
% uso _ quando o dado năo é importante e năo quero saber dele

+recibo(Id, U, N, M, E, I, Dat, D, C) :: (solucoes(Id, recibo(Id,_,_,_,_,_,_,_,_), R),
					comprimento(R,Lr),
					Lr==1,
					solucoes((U,N,M), utente(U,N,_,M), Uts),
					comprimento(Uts,Lu),
					Lu==1,
					solucoes((P,E,I), prestador(P,_,E,I), Pres),
					comprimento(Pres, Lp),
					Lp>=1,
					solucoes((Dat,U,P,D,C), cuidado(Dat,U,P,D,C), Cuids),
					comprimento(Cuids, Lc),
					Lc==1).

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

%-----------------------------------------------------------------------------------------------------
% Invariante que não permite a remoção de qualquer recibo, uma vez que a informação
% financeira nunca pode ser eliminada. Anti-fraude.

-recibo(Id, U, N, M, E, I, Dat, D, C) :: fail.

% ////////////////////////////////////////// Predicados Extra /////////////////////////////////////////
% ----------------------------------------------------------------------------------------------------
% Extensăo do predicado inserir: Termo -> {V,F}
% mesmo que EVOLUCAO

inserir(Termo) :-
	solucoes(Invariante, +Termo::Invariante, Lista),
	insercao(Termo),
	teste(Lista).

% ----------------------------------------------------------------------------------------------------
% Extensăo do predicado remover: Termo -> {V,F}

remover(Termo) :-
	solucoes(Invariante, -Termo::Invariante, Lista),
	teste(Lista),
	remocao(Termo).

% ----------------------------------------------------------------------------------------------------
% Extensăo do predicado insercao: Termo -> {V,F}

insercao(T) :-
	assert(T).
insercao(T) :-
	retract(T),!,fail.

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

% ----------------------------------------------------------------------------------------------------
% Extensao do predicado nao: Q -> {V,F}

nao(Q):- Q, !, fail.
nao(Q).

% ----------------------------------------------------------------------------------------------------
% Extensao do predicado quantosTem:A,Lista,Resultado  -> {V,F}

quantosTem(A,[],0).
quantosTem(A,[H|T],Resultado):-
	(A == H), quantosTem(A,T,R), Resultado is R+1.
quantosTem(A,[H|T],Resultado):-
	(A \= H), quantosTem(A,T,Resultado).

% ----------------------------------------------------------------------------------------------------
% Extensao do predicado pertence: X,L -> {V,F}

pertence(X,[]):- fail.
pertence(X,[X|T]):- X==X.
pertence(X,[H|T]):-
	X\=H,
	pertence(X,T).

% ----------------------------------------------------------------------------------------------------
% Extensao do predicado removeDup: L,R -> {V,F}

removeDup([],[]).
removeDup([X|T],R):-
	pertence(X,T),
	removeDup(T,R).
removeDup([X|T],[X|R]):-
	nao(pertence(X,T)),
	removeDup(T,R).

% ----------------------------------------------------------------------------------------------------
% Extensao do predicado ordenarDecresc: L,Resultado -> {V,F}

ordenarDecresc([X],[X]).
ordenarDecresc([X|Y],T):-
	ordenarDecresc(Y,R),
	insereOrdenado(X,R,T).

% ----------------------------------------------------------------------------------------------------
% Extensao do predicado insereOrdenado: X,L,Resultado  -> {V,F}

insereOrdenado((X1,Y1),[],[(X1,Y1)]).
insereOrdenado((X1,Y1),[(X2,Y2)|Z],[(X1,Y1)|[(X2,Y2)|Z]]):-
	Y1>Y2.
insereOrdenado((X1,Y1), [(X2,Y2)|Z], [(X2,Y2)|R2]) :-
	Y1=<Y2,
	insereOrdenado((X1,Y1),Z,R2).

% ----------------------------------------------------------------------------------------------------
% Extensao do predicado concatena(L1,L2,L3)->{V,F}

concatena([],L2,L2).
concatena([X|L1],L2,[X|L]) :-
	concatena(L1,L2,L).