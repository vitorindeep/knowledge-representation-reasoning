%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SIST. REPR. CONHECIMENTO E RACIOCINIO - Trabalho prático nº 2

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SICStus PROLOG: Declaracoes iniciais

:- set_prolog_flag( discontiguous_warnings,off ).
:- set_prolog_flag( single_var_warnings,off ).
:- set_prolog_flag( unknown,fail ).

%-----------------------------------------------------------------------------------------------------
% SICStus PROLOG: definicoes iniciais

:- op( 900,xfy,'::' ).

% Para os invariantes:

:- dynamic '-'/1.
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
cuidadoPrestado(7, oftalmologia, hospitaldaluz, povoadevarzim).

%-----------------------------------------------------------------------------------------------------
% Extensao do predicado atoMedico: Data,IdU,IdC,Custo -> {V,F}

atoMedico(01-01-2017, 1, 6, 15).
atoMedico(13-01-2017, 3, 4, 30).
atoMedico(14-01-2017, 2, 7, 8).
atoMedico(20-01-2017, 4, 2, 20).
atoMedico(02-02-2017, 7, 4, 5).
atoMedico(03-02-2017, 3, 5, 24).
atoMedico(20-02-2017, 6, 7, 37).
atoMedico(22-02-2017, 6, 2, 55).
atoMedico(04-03-2017, 2, 2, 98).
atoMedico(15-03-2017, 1, 1, 5).

%-----------------------------------------------------------------------------------------------------
% Representaçao de conhecimento negativo
%-----------------------------------------------------------------------------------------------------

-utente(Id,N,I,M) :- 
					nao(utente(Id,N,I,M)),
					nao(excecao(utente(Id,N,I,M))).

-cuidadoPrestado(Id,D,I,C) :-
					nao(cuidadoPrestado(Id,D,I,C)),
					nao(excecao(cuidadoPrestado(Id,D,I,C))).

-atoMedico(D,IdU,Id,C) :-
					nao(atoMedico(D,IdU,Id,C)),
					nao(excecao(atoMedico(D,IdU,Id,C))).

%-----------------------------------------------------------------------------------------------------
% Representar casos de conhecimento imperfeito
%-----------------------------------------------------------------------------------------------------

%-----------------------------------------------------------------------------------------------------
% Conhecimento imperfeito incerto

% não se sabe qual é a instituição do cuidadoPrestado com id 8, correpondente a oftalmologia em Lisboa

cuidadoPrestado(8,oftalmologia,xpto3,lisboa).
excecao( cuidadoPrestado(IdC,Descricao,Instituicao,Cidade) ) :-
	cuidadoPrestado(IdC,Descricao,xpto3,Cidade).

% não se sabe o nome do utente com id 13, de 55 anos e residente em Vizela 

utente(13,xpto2,55,vizela).
excecao( utente(Id,N,I,M) ) :-
	utente(Id,xpto2,I,M).

% não se sabe a morada do utente com id11, com nome Mario e 30 anos de idade. 
% no entanto, sabe-se que o utente não reside em Braga.

utente(11,mario,30,xpto1).
excecao( utente(Id,N,I,M) ) :-
	utente(Id,N,I,xpto1).

% representação de conhecimento negativo

-utente(11,mario,30,braga).

%-----------------------------------------------------------------------------------------------------
% Conhecimento imperfeito impreciso

% não se sabe se o cuidado prestado com id 9 correspondente a psicologia é no hospitalbraga ou hsmm

excecao( cuidadoPrestado(9,psicologia,hospitalbraga,braga) ).
excecao( cuidadoPrestado(9,psicologia,hsmm,barcelos) ).

% não se sabe se o utente felisberto (id 12 e residente em Aveiro) tem 20 ou 21 anos

excecao( utente(12,felisberto,20,aveiro) ).
excecao( utente(12,felisberto,21,aveiro) ).

%-----------------------------------------------------------------------------------------------------
% Conhecimento imperfeito interdito

% nunca será possível saber qual o preço que o cliente 1 pagou pelo serviço 4 no dia 29-03-2017 

atoMedico(29-03-2017, 1, 4, xpto4).
excecao( atoMedico(D,U,S,C) ) :-
			atoMedico(D,U,S,xpto4).
nulo(xpto4).
+atoMedico( D,U,S,C ) :: (solucoes((D,U,S), (atoMedico(29-03-2017, 1, 4, Cs),nao(nulo(Cs))),Servs ),
                 		  comprimento( Servs,N ),
                 		  N == 0).

%-----------------------------------------------------------------------------------------------------
% Manipular invariantes que designem restrições à inserção e à remoção de conhecimento do sistema
%-----------------------------------------------------------------------------------------------------

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
% Invariante que não permite a inserção de conhecimento de um atoMedico quando o id do utente ou o id
% do serviço não existem na base de conhecimento 

+atoMedico(D,U,S,C) :: (solucoes((S, Xs), cuidadoPrestado(S, Xs, Ys, Zs), Servs),
				    	comprimento(Servs, Ls),
				 	    Ls == 1,
						solucoes((U, Xu), utente(U, Xu, Yu, Zu), Uts),
				    	comprimento(Uts, Lu),
				 	    Lu == 1).

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

%-----------------------------------------------------------------------------------------------------
% Evolução do conhecimento 
% permite adicionar conhecimento ou atualizar conhecimento imperfeito
%-----------------------------------------------------------------------------------------------------

evolucao(utente(Id,Nome,Idade,Morada)):-
		demo(utente(Id,Nome,Idade,Morada),desconhecido),
		findall(utente(Id,N,I,M), utente(Id,N,I,M),L),
		remocaoL(utente(Id,Nome,Idade,Morada),L).

evolucao(utente(Id,Nome,Idade,Morada)):-
		demo(utente(Id,Nome,Idade,Morada),falso),
		registar(utente(Id,Nome,Idade,Morada)).

evolucao(cuidadoPrestado( Id,Desc,Inst,Cidade ) ):-
		demo(cuidadoPrestado(Id,Desc,Inst,Cidade) ,desconhecido),
		findall(cuidadoPrestado(Id,E,I,C), cuidadoPrestado(Id,E,I,C),L),
		remocaoL(cuidadoPrestado( Id,Desc,Inst,Cidade ),L).

evolucao(cuidadoPrestado( Id,Desc,Inst,Cidade ) ):-
		demo(cuidadoPrestado(Id,Desc,Inst,Cidade),falso),
		registar(cuidadoPrestado( X,Desc,Inst,Cidade)).

evolucao(atoMedico(Data,IdU, IdS,Custo)):-
		demo(atoMedico(Data,IdU, IdS,Custo),desconhecido),
		findall(atoMedico(Data,IdU, IdS,Custo), atoMedico(Data,IdU, IdS,Custo),L),
		remocaoL(atoMedico(Data,IdU, IdS,Custo),L).

evolucao(atoMedico(Data,IdU, IdS,Custo)):-
		demo(atoMedico(Data,IdU, IdS,Custo),falso),
		registar(atoMedico(Data,IdU, IdS,Custo)).

% ----------------------------------------------------------------------------------------------------
% Extensão do predicado que permite a registar conhecimento
 
registar(Termo) :-
    findall(Invariante, +Termo::Invariante, Lista),
    insercao(Termo),
    testar(Lista).

insercao(T) :-
    assert(T).
insercao(T) :-
    retract(T), !, fail.

% ----------------------------------------------------------------------------------------------------
% Extensão do predicado que permite a remover conhecimento

remover(Termo) :-
    findall(Invariante, -Termo::Invariante, Lista),
    testar(Lista),
    remocao(Termo).

remocao(T) :-
    retract(T).
remocao(T) :-
    assert(T), !, fail.

% ----------------------------------------------------------------------------------------------------
% Extensão do predicado que permite a remover conhecimento de uma lista

remocaoL( Termo,L ) :-
    retractL( L ),
    registar(Termo).
remocaoL( Termo,L) :-
    assertL( L ),!,fail.

retractL([]).
retractL([X|L]):- 
		retract(X), 
		retractL(L).

assertL([]).
assertL([X|L]):- 
		assert(X), 
		assertL(L).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do meta-predicado demo: Questao,Resposta -> {verdadeiro, falso, desconhecido}

demo( Questao,verdadeiro ) :-
    Questao.
demo( Questao, falso ) :-
    -Questao.
demo( Questao,desconhecido ) :-
    nao( Questao ),
    nao( -Questao ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do meta-predicado nao: Questao -> {V,F}

nao( Questao ) :-
    Questao, !, fail.
nao( Questao ).

% ----------------------------------------------------------------------------------------------------
% Extensão do predicado solucoes

solucoes(X,Y,Z) :-
    findall(X,Y,Z).
 
 % ----------------------------------------------------------------------------------------------------
% Extensão do predicado comprimento: Lista, Tamanho -> {V,F}

comprimento(X, Z) :-
    length(X, Z).

% ----------------------------------------------------------------------------------------------------
% Extensão do predicado testar: Lista -> {V,F}

testar([]).
testar([I|L]) :-
    I,
    testar(L).
