%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SIST. REPR. CONHECIMENTO E RACIOCINIO - MiEI/3

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Trabalho de Grupo - 2º Exercicio
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SICStus PROLOG: Declaracoes iniciais

:- set_prolog_flag( discontiguous_warnings,off ).
:- set_prolog_flag( single_var_warnings,off ).
:- set_prolog_flag( unknown,fail ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SICStus PROLOG: definicoes iniciais

:- dynamic utente/4.
:- dynamic servico/4.
:- dynamic consulta/4.
:- dynamic '-'/1.
:- op( 900,xfy,'::' ).
:- op( 900,xfy,'e' ).
:- dynamic excecao/1.
:- dynamic idUtente/1.
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% TP2 Extensao  {V,F,D}

% utente: IdUt,Nome,Idade,Morada -> {V,F,D}
utente( 1,rafa,21,barca ).
utente( 2,chico,20,povoa ).
utente( 3,rui,21,montalegre ).
utente( 4,jessica,21,pontelima ).
utente( 5,pedro,23,arcos ).
utente( 6,alexandre,18,viladoconde ).
utente( 7,carlos,53,melgaco ).
utente( 8,maria,44,aveiro ).
utente( 9,laura,78,lisboa ).
utente( 10,amalia,95,lisboa ).
utente( 11,armindo,78,porto ).
utente( 12,joaquim,20,pontelima ).
utente( 13,manuel,94,montalegre ).
utente( 14,rosa,78,viseu ).
utente( 15,teresa,55,braga ).
utente( 16,vasco,38,porto ).

% servico: Serv,Descricao,Instituicao,Cidade -> {V,F,D}
servico( 1,pediatria,saojoao,porto ).
servico( 2,pediatria,clipovoa,povoa ).
servico( 3,pediatria,hospitalnovo,braga ).
servico( 4,pediatria,cliaveiro,aveiro ).
servico( 5,oftalmologia,multiolhar,barca ).
servico( 6,oftalmologia,olhoscapital,lisboa ).
servico( 7,oftalmologia,visaoporto,porto ).
servico( 8,ortopedia,saudeossos,arcos ).
servico( 9,ortopedia,ortoconde,viladoconde ).
servico( 10,ortopedia,trataossos,melgaco ).
servico( 11,dentista,climelga,melgaco ).
servico( 12,dentista,santoantonio,porto ).
servico( 13,dentista,saojose,lisboa ).
servico( 14,psiquiatria,psiminho,pontelima ).
servico( 15,psiquiatria,psicomonte,montalegre ).
servico( 16,psiquiatria,psicobraga,braga ).

% consulta: Data,IdUt,Serv,Custo -> {V,F,D}
consulta( 23/01/2015,1,1,50 ).
consulta( 02/02/2014,2,2,20 ).
consulta( 25/03/2013,3,3,30 ).
consulta( 21/04/2016,4,4,40 ).
consulta( 12/05/2010,5,5,100 ).
consulta( 23/06/2014,6,6,500 ).
consulta( 21/07/2015,7,7,120 ).
consulta( 01/08/2015,8,8,23 ).
consulta( 23/01/2015,9,9,52 ).
consulta( 02/02/2014,10,10,21 ).
consulta( 25/03/2013,11,11,13 ).
consulta( 21/04/2016,12,12,41 ).
consulta( 12/05/2010,13,13,102 ).
consulta( 23/06/2014,14,14,40 ).
consulta( 21/07/2015,15,15,54 ).
consulta( 01/08/2015,16,16,224 ).

%-------------------------------------------------------
% Representaçao de conhecimento negativo, positivo e imperfeito

-utente(Id,Nome,Idade,Morada) :-
	nao(utente(Id,Nome,Idade,Morada)),
	nao(excecao(utente(Id,Nome,Idade,Morada))).

-servico(Id,Especialidade,Instituicao,Cidade) :-
	nao(servico(Id,Especialidade,Instituicao,Cidade)),
	nao(excecao(servico(Id,Especialidade,Instituicao,Cidade))).


-consulta(Data,IdU, IdS,Custo) :-
	nao(consulta(Data,IdU, IdS,Custo)),
	nao(excecao(consulta(Data,IdU, IdS,Custo))).

%---------------------Connhecimento imperfeito incerto -------------------
%---------------------------------------------------------------------------


excecao(utente(Id,Nome,Idade,Morada)) :- 
	utente(Id,Nome,incerto,Morada).

excecao(utente(Id,Nome,Idade,Morada)) :- 
	utente(Id,incerto,Idade,Morada).

excecao(utente(Id,Nome,Idade,Morada)) :- 
	utente(Id,Nome,Idade,incerto).

excecao(utente(Id,Nome,Idade,Morada)) :- 
	utente(Id,incerto,incerto,Morada).

excecao(utente(Id,Nome,Idade,Morada)) :- 
	utente(Id,incerto,Idade,incerto).

excecao(utente(Id,Nome,Idade,Morada)) :- 
	utente(Id,Nome,incerto,incerto).

excecao(utente(Id,Nome,Idade,Morada)) :- 
	utente(Id,incerto,incerto,incerto).



excecao(servico(Id,Especialidade,Instituicao,Cidade)) :- 
	servico(Id,incerto,Instituicao,Cidade).

excecao(servico(Id,Especialidade,Instituicao,Cidade)) :- 
	servico(Id,Especialidade,incerto,Cidade).

excecao(servico(Id,Especialidade,Instituicao,Cidade)) :- 
	servico(Id,Especialidade,Instituicao,incerto).

excecao(servico(Id,Especialidade,Instituicao,Cidade)) :- 
	servico(Id,incerto,incerto,Cidade).

excecao(servico(Id,Especialidade,Instituicao,Cidade)) :- 
	servico(Id,incerto,Instituicao,incerto).



excecao(consulta(Data,IdU, IdS,Custo)) :- 
	consulta(incerto,IdU, IdS,Custo).

excecao(consulta(Data,IdU, IdS,Custo)) :- 
	consulta(Data,incerto, IdS,Custo).

excecao(consulta(Data,IdU, IdS,Custo)) :- 
	consulta(Data,IdU, incerto,Custo).

excecao(consulta(Data,IdU, IdS,Custo)) :- 
	consulta(Data,IdU, IdS,incerto).

excecao(consulta(Data,IdU, IdS,Custo)) :- 
	consulta(incerto,incerto, IdS,Custo).

excecao(consulta(Data,IdU, IdS,Custo)) :- 
	consulta(incerto,IdU, incerto,Custo).

excecao(consulta(Data,IdU, IdS,Custo)) :- 
	consulta(incerto,IdU, IdS,incerto).

excecao(consulta(Data,IdU, IdS,Custo)) :- 
	consulta(Data,incerto, incerto,Custo).

excecao(consulta(Data,IdU, IdS,Custo)) :- 
	consulta(Data,incerto, IdS,incerto).

excecao(consulta(Data,IdU, IdS,Custo)) :- 
	consulta(Data,IdU, incerto ,incerto).

excecao(consulta(Data,IdU, IdS,Custo)) :- 
	consulta(Data,incerto, incerto ,incerto).

excecao(consulta(Data,IdU, IdS,Custo)) :- 
	consulta(incerto,IdU, incerto ,incerto).

excecao(consulta(Data,IdU, IdS,Custo)) :- 
	consulta(incerto,incerto, IdS ,incerto).

excecao(consulta(Data,IdU, IdS,Custo)) :- 
	consulta(incerto,incerto, incerto ,Custo).
	



%  O utente Ze reside no porto porem ninguém conhece a sua idade.

utente(18,ze, incerto, porto).


% --------------------------------------------
%A utente Matilde com ID 19, tem 25 anos e não se sabe a sua morada.

utente(19,matilde, 25, incerto).


%--------------------------------------------
% O utente com Id 17 tem 75 anos de idade, 
%não se conhecendo o seu nome nem o local onde mora.

utente(17, incerto, 75, incerto).



%--------------------------------------------
% Não se conhece o nome da instituição com id 17 que disponibiliza o serviço
% de pediatria na Barca.

servico(17,pediatria,incerto ,barca).


%-------------------------------------------
%A Instituição PsicoNorte com ID 18 disponibiliza o serviço de psiquiatria 
% aos seus utentes,contudo não se sabe em que cidade se situa.

servico(18,pediatria,psiconorte ,incerto).


% --------------------------------------------
% Não se conhece a especialidade fornecida pela Instituição ambulante CuraTodos,
%  com id 19, bem como a sua localização.

servico(19,incerto,curatodos ,incerto).

%--------------------------------------------
% A consulta no serviço 2 do utente com IdUtente 7 no dia 12/05/2014 tem um custo que ninguém conhece.

consulta(12/05/2014,7, 2,incerto).


%--------------------------------------------
% Quando se questionou o utente 12 acerca da data na qual foi a uma consulta do serviço 3 cujo custo foi de 34 euros este indicou que não se lembrava de quando tinha sido

consulta(incerto,12, 3,34).


% --------------------------------------------
% Existe uma consulta com custo de 27 euros no dia 01/01/2007 no qual se desconhece o utente e o serviço em questão. 

consulta(01/01/2007,incerto, incerto,27).


%---------------------Connhecimento imperfeito impreciso -------------------
%---------------------------------------------------------------------------

% ----------------------------------------------------------------
% O utente com ID 22, de nome Jaime que reside em Montalegre tem idade compreendida entre 30 e 35 anos.

excecao( utente(22, jaime, Idade, montalegre) ) :- 
		Idade >= 30, Idade =< 35.

excecao( utente(22, jaime, Idade, pontelima) ) :- 
		Idade >= 30, Idade =< 35.

% ----------------------------------------------------------------
% O utente com ID 23 de nome Joaquina, tem idade compreendida entre 50 e 60 anos e não se sabe se tem residência em Ponte de Lima ou em Braga.


excecao( utente(23, joaquina, Idade, pontelima) ):- Idade >= 50 , Idade =< 60. 
excecao( utente(23, joaquina, Idade, braga) ):- Idade >= 50 , Idade =< 60.


% ----------------------------------------------------------------
% O utente idoso Alberto com ID 27 não se lembra se tem consulta no serviço de ID 20 no dia 01/05/2016 ou no dia 02/05/2016, só sabe que custa 60 euros.

excecao(consulta(01/05/2016,27,20,60)).
excecao(consulta(02/05/2016,27,20,60)).

% ----------------------------------------------------------------
% A consulta no serviço com ID 21 do utente com ID23, no dia 03/05/2016 foi sujeita a um desconto, tendo um custo compreendido entre 25 e 50 euros.

excecao(consulta(03/05/2016,23,21,Custo)):- Custo >= 25, Custo =< 50.

% ----------------------------------------------------------------
% O serviço de pediatria com id 22 existe ou na cidade do Porto ou na cidade de Póvoa de Varzim

excecao( servico(22,pediatria, cspaz,povoa) ).
excecao( servico(22,pediatria, cspaz,porto) ).

% ----------------------------------------------------------------
% O serviço de oftalmologia com id 18 está disponível ou na cidade de Braga ou na cidade de Póvoa de Lanhoso.

excecao( servico(18,oftalmologia,cscruz,braga) ).
excecao( servico(18,oftalmologia,cscruz,povoadelanhoso) ).

%---------------------Connhecimento imperfeito interdito -------------------
%---------------------------------------------------------------------------

nulo(null).

% ----------------------------------------------------------------
% Devido ao sistema de proteção de testemunhas, não é possível saber a morada do utente com ID 21, de nome Edgar e idade 33. 

utente( 21,edgar,33, null ).
excecao(utente(Id,Nome,Idade, Morada)):- utente(Id, Nome, Idade, null).



+utente(Id, Nome, Idade, Morada) :: ( solucoes((Id, Nome, Idade, Ms), 
									   (utente(21, edgar, 33, Ms), nao(nulo(Ms)) ), S), comprimento(S,N), N == 0).


% ------------------------------------------------------
% O serviço fornecido pela clinica ilegal da Barca com nome LipoBarca, e de ID 31, tem especialidade que não deve ser conhecida pelo publico.

servico( 20,null,lipobarca,barca ).
excecao(servico(Id,Especialidade,Instituicao,Cidade)) :-
	servico(Id,null,Instituicao,Cidade).

+servico(Id,Esp,Instituicao,Cidade) :: 
	(solucoes(Id, (servico(20,Esp,lipobarca,barca),nao(nulo(Esp))),S),
	 comprimento(S,N), N==0).

%------------------------------------------------------
% O utente com ID 30, não quer que se saiba em que dia é que tem a consulta de psiquiatria com Id de serviço 14, cujo custo irá ser 500 euros.

consulta(null,30,14,500).
excecao( consulta(Data,IdUtente,IdServico,Custo) ):- consulta(null,IdUtente,IdServico,Custo).



+consulta(Data,IdUtente,IdServico,Custo) :: ( solucoes((Ds,IdUtente,IdServico,Custo), 
									   (consulta(Ds,30,14,500), nao(nulo(Ds)) ), S), comprimento(S,N), N == 0).

%------------------------------------------------------
% Não se pode saber a idade da utente nº 20, cujo nome é Felisberta, que mora em Braga.

utente(20,felisberta,null,braga).
excecao( utente(Id,Nome,Idade, Morada) ):- utente(Id, Nome, null, Morada).


+utente(Id, Nome, Idade, Morada) :: ( solucoes((Id, Nome, Is, Morada), 
									   (utente(20,felisberta,Is,braga), nao(nulo(Is)) ), S), comprimento(S,N), N == 0).

%------------------------------------------------------
% É impossível encontrar a cidade onde é prestado o serviço de ortopedia na instituição da malade

servico( 21,ortopedia,malade,null ).
excecao(servico(Id,Especialidade,Instituicao,Cidade)) :-
	servico(Id,Especialidade,Instituicao,null).


+servico(Id,Esp,Instituicao,Cidade) :: 
	(solucoes(Id, (servico(21,ortopedia,malade,Cidade),nao(nulo(Cidade))),S),
	 comprimento(S,N), N==0).
 
%------------------------------------------------------
% É interdito saber o custo da consulta com id de serviço 18 do dia 20/08/2015, do id de utente nº 22.

consulta(20/08/2015,22,18,null).
excecao( consulta(Data,IdUtente,IdServico,Custo) ):- consulta(Data,IdUtente,IdServico,null).


+consulta(Data,IdUtente,IdServico,Custo) :: ( solucoes((Data,IdUtente,IdServico,Cs), 
									   (consulta(20/08/2015,22,18,Cs), nao(nulo(Cs)) ), S), comprimento(S,N), N == 0).


%----------------------------------------------------------------------------
%------------------------- Invariantes insercao -----------------------------

% --------------------------------- - - - - - - - - - -  -  -  -  -   -
% Invariantes Estruturais:  nao permitir a insercao de conhecimento repetido

+utente( Id,Nome,Idade,Morada ) :: (findall( Id,(utente( Id,N,I,M )),L ),
                  comprimento( L,N ), 
				  N == 1
                  ). 

% Invariante: nao permitir a insercao de servicos iguais

+servico( Id,Especialidade,Instituicao,Cidade ) :: (findall( ( Id,Especialidade,Instituicao,Cidade ),(servico( Id,Especialidade,Instituicao,Cidade )),L ),
                  comprimento( L,N ), 
				  N == 1
                  ).

% -- não impedimos a insercao de conhecimento repetido nas consultas, pois é possivel um mesmo utente ir na mesma data ao mesmo servico na 
%    mesma instituição, com o mesmo custo, mais do que uma vez

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Invariante de Integridade: nao permitir a insercao de dados nao correspondentes com a base de conhecimento

+consulta( Data,IdU,IdS,Custo ) :: ( findall( IdUt,utente(IdUt,Nome,Idade,Morada),L1 ), 
						findall( IdSe,servico( IdSe,Especialidade,Instituicao,Cidade ),L2 ),
						pertence( IdU,L1 ),
						pertence( IdS,L2 ) ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Invariantes: nao permitir a insercao de utentes cujo Id nao seja indicada por um inteiro, e Nome e Morada nao seja indicada por uma "string"

+utente( Id,Nome,Idade,Morada ) :: ( integer(Id), atom(Nome),atom(Morada) ). 



+servico( Id,Especialidade,Instituicao,Cidade ) :: ( integer(Id), atom(Especialidade), atom(Instituicao),atom(Cidade) ). 



%----------------------------------------------------------------------------
%------------------------- Invariantes remocao -----------------------------

% --------------------------------- - - - - - - - - - -  -  -  -  -   -
% Invariantes Estruturais : nao permitir a remocao de conhecimento presentes noutros predicados

-utente( Id,Nome,Idade,Morada ) :: (findall( IdU,(consulta( Data,IdU,IdS,Custo )),L ),
                  nao(pertence( Id,L ))
                  ).

-servico( Id,Especialidade,Instituicao,Cidade ) :: (findall( IdS,(consulta( Data,IdU,IdS,Custo )),L ),
                  nao(pertence( Id,L ))
                  ).

%----------------------------------------------------------------------------
%------------------------- Predicados Auxiliares ----------------------------

% Auto-Increment de ID de utente e de servico
% ID a inserir na BC
idUtente(24).
idServico(23).


getID([X|_], X).

autoIncrement(utente,N) :- 
				findall(X,idUtente(X),L),
				getID(L,R),
				Y is R+1,
				assert(idUtente(Y)),
				N is R,
				retract(idUtente(R)).



autoIncrement(servico,N) :- 
				findall(X,idServico(X),L),
				getID(L,R),
				Y is R+1,
				assert(idServico(Y)),
				N is R,
				retract(idServico(R)).

% Insercao na base de conhecimento-------------------------------------------

evolucao( Termo ) :-
    solucoes( Invariante,+Termo::Invariante,Lista ),
    insercao( Termo ),
    teste( Lista ).

insercao( Termo ) :-
    assert( Termo ).
insercao( Termo ) :-
    retract( Termo ),!,fail.

teste( [] ).
teste( [R|LR] ) :-
    R,
    teste( LR ).

% remocao de dados da base de conhecimento-------------------------------------------

retrocesso( Termo ) :-
    solucoes( Invariante,-Termo::Invariante,Lista ),
    remocao( Termo ),
    teste( Lista ).

remocao( Termo ) :-
    retract( Termo ).
remocao( Termo ) :-
    assert( Termo ),!,fail.




assertL([]).
assertL([X|L]):- 
		assert(X), assertL(L).




remocaoL( Termo,L ) :-
    retractL( L ),
    evolucao(Termo).
remocaoL( Termo,L) :-
    assertL( L ),!,fail.



%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do meta-predicado demo: Questao,Resposta -> {V,F,D}

demo( Questao,verdadeiro ) :-
    Questao.
demo( Questao,falso ) :-
    -Questao.
demo( Questao,desconhecido ) :-
    nao( Questao ),
    nao( -Questao ).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do meta-predicado demoL: ListaQuestoes,ListaRespostas -> {V,F,D}
% Responde individualmente às questões de uma lista de questões

demoL([],[]).
demoL([Q|TQ],[R|TR]) :- demo(Q,R), demoL(TQ,TR).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do meta-predicado demoC: Questao e Questao,Resposta -> {V,F}
% Responde às questões como se estas representassem uma conjuncao.

demoC(Q1 e Q2,R) :- demo( Q1,R1 ), demoC( Q2,R2 ), e(R1,R2,R).
demoC(Q1,R1) :- demo(Q1,R1).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do meta-predicado registar: Termo -> {V,F,D}
% permite a atualizacao de conhecimento imperfeito,
% e insercao de conhecimento imperfeito incerto/ normal).


registar(utente(Id,Nome,Idade,Morada)):-
		demo(utente(Id,Nome,Idade,Morada),desconhecido),
		findall(utente(Id,N,I,M), utente(Id,N,I,M),L),
		remocaoL(utente(Id,Nome,Idade,Morada),L).

registar(utente(Id,Nome,Idade,Morada)):-
		demo(utente(Id,Nome,Idade,Morada),falso),
		autoIncrement(utente,X),
		evolucao(utente(X,Nome,Idade,Morada)).


registar(servico( Id,Especialidade,Instituicao,Cidade ) ):-
		demo(servico(Id,Especialidade,Instituicao,Cidade) ,desconhecido),
		findall(servico(Id,E,I,C), servico(Id,E,I,C),L),
		remocaoL(servico( Id,Especialidade,Instituicao,Cidade ),L).

registar(servico( Id,Especialidade,Instituicao,Cidade ) ):-
		demo(servico(Id,Especialidade,Instituicao,Cidade),falso),
		autoIncrement(servico,X),
		evolucao(servico( X,Especialidade,Instituicao,Cidade ) ).


registar(consulta(Data,IdU, IdS,Custo)):-
		demo(consulta(Data,IdU, IdS,Custo),desconhecido),
		findall(consulta(Data,IdU, IdS,Custo), consulta(Data,IdU, IdS,Custo),L),
		remocaoL(consulta(Data,IdU, IdS,Custo),L).

registar(consulta(Data,IdU, IdS,Custo)):-
		demo(consulta(Data,IdU, IdS,Custo),falso),
		evolucao(consulta(Data,IdU, IdS,Custo)).



%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do meta-predicado e: ValorLogico,ValorLogico,ValorLogico -> {V,F,D}
% O terceiro argumento corresponde ao valor lógico correspondente à conjunção dos valores lógicos dos dois primeiros argumentos
% Entendem-se valores lógicos como: verdadeiro, falso ou desconhecido

e(verdadeiro,verdadeiro,verdadeiro).
e(verdadeiro,desconhecido,desconhecido).
e(verdadeiro,falso,falso).
e(desconhecido,verdadeiro,desconhecido).
e(desconhecido,desconhecido,desconhecido).
e(desconhecido,falso,falso).
e(falso,_,falso).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do meta-predicado nao: Questao -> {V,F}

nao( Questao ) :-
    Questao, !, fail.
nao( Questao ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do meta-predicado solucoes: Formato,Questao, Solucao -> {V,F}

solucoes( X,Y,Z ) :-
    findall( X,Y,Z ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do meta-predicado comprimento: Lista, Tamanho -> {V,F}

comprimento( S,N ) :-
    length( S,N ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado pertence que verifica se um dado elemento pertence a uma determinada lista
% pertence: Elemento,Lista -> {V,F}

pertence( X,[X|L] ).
pertence( X,[Y|L] ) :- X \= Y,
					   pertence( X,L ).

% Extensao do predicado retract de uma lista
% retract: Lista -> {V,F}
retractL([]).
retractL([X|L]):- 
		retract(X), retractL(L).


listar(utente,L):-
	findall(utente(X,Y,Z,G),utente(X,Y,Z,G),L).


listar(servico,L):-
	findall(servico(X,Y,Z,G),servico(X,Y,Z,G),L).


listar(consulta,L):-
	findall(consulta(X,Y,Z,G),consulta(X,Y,Z,G),L).

listar(excecao,L) :-
			findall(excecao(T),excecao(T),L).