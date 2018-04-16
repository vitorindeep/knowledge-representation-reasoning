%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SIST. REPR. CONHECIMENTO E RACIOCINIO - MiEI/3 - EXERCICIO 1


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SICStus PROLOG: Declaracoes iniciais

:- set_prolog_flag(discontiguous_warnings, off).
:- set_prolog_flag(single_var_warnings, off).
:- set_prolog_flag(unknown, fail).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SICStus PROLOG: definicoes iniciais

:- op(900, xfy, '::').
:- op(996, xfy, '&&' ).  % operador de conjuncao
:- op(997, xfy, '$$' ).  % operador de disjuncao 
:- op(998, xfx, '=>' ).  % operador de implicacao 
:- op(999, xfx, '<=>' ). % operador de equivalencia

% Operador usado em invariantes avaliados apenas quando
% se evolui/involui conhecimento imperfeito impreciso
:- op(900,xfy,:~:).

% Operador usado em invariantes avaliados apenas quando
% se evolui/involui conhecimento imperfeito incerto/interdito
:- op(900,xfy,:-:).

:- dynamic utente/4.
:- dynamic cuidado_prestado/4.
:- dynamic profissional/4.
:- dynamic atribuido/2.
:- dynamic ato_medico/5.
:- dynamic '-'/1.
:- dynamic '::'/2.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado utente: IdUt, Nome, Idade, Morada -> {V,F,D}

% Para cada extensao do predicado utente, tem-se a respetiva
% extensao do meta-predicado perfeito: Utente -> {V,F}
% que assinala a existencia de conhecimento perfeito sobre
% o utente inserido

utente(0, 'Jose', 55, 'Rua dos Zecas').
perfeito(utente(0)).

utente(1, 'Joao', 21, 'Rua de Baixo').
perfeito(utente(1)).

utente(2, 'Manuel', 36, 'Rua Maria Albertina').
perfeito(utente(2)).

utente(3, 'Carlos', 43, 'Rua da Fabrica').
perfeito(utente(3)).

utente(4, 'Maria', 73, 'Avenida Camoes').
perfeito(utente(4)).

utente(5, 'Joana', 8, 'Avenida Camoes').
perfeito(utente(5)).

utente(6, 'Fernando', 49, 'Rua da Beira').
perfeito(utente(6)).

utente(7, 'Joao', 29, 'Rua da Encosta').
perfeito(utente(7)).

utente(8, 'Ana', 40, 'Avenida Soares').
perfeito(utente(8)).

utente(9, 'Catarina', 17, 'Avenida Carneiro').
perfeito(utente(9)).

utente(10, 'Maria', 33, 'Rua da Pata').
perfeito(utente(10)).

utente(11, 'Joaquim', 56, 'Rua da Boavista').
perfeito(utente(11)).

% Invariante referencial: nao permitir que se remova um utente enquanto
%                         existirem atos medicos associados a si
-utente(IdUt, _, _, _) :: (
	nao(ato_medico(_, IdUt, _, _, _))
).

% Extensao do predicado que define a negacao forte do predicado utente
-utente(IdUt, Nome, Idade, Morada) :-
	nao(utente(IdUt, Nome, Idade, Morada)),
	nao(excecao(utente(IdUt, Nome, Idade, Morada))).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado cuidado_prestado: IdServ, Descricao, Instituicao, Cidade -> {V,F,D}

cuidado_prestado( 1, 'Cirurgia',     'Hospital Privado de Braga', 'Braga').
cuidado_prestado( 2, 'Dermatologia', 'Hospital Privado de Braga', 'Braga').
cuidado_prestado( 3, 'Pediatria',    'Hospital Privado de Braga', 'Braga').
cuidado_prestado( 4, 'Pneumologia',  'Hospital Privado de Braga', 'Braga').
cuidado_prestado( 5, 'Reumatologia', 'Hospital Privado de Braga', 'Braga').

cuidado_prestado( 6,  'Cardiologia',       'Hospital de Braga', 'Braga').
cuidado_prestado( 7,  'Cirurgia',          'Hospital de Braga', 'Braga').
cuidado_prestado( 8,  'Eletrocardiograma', 'Hospital de Braga', 'Braga').
cuidado_prestado( 9,  'Ortopedia',         'Hospital de Braga', 'Braga').
cuidado_prestado(10,  'Pneumologia',       'Hospital de Braga', 'Braga').
cuidado_prestado(11,  'Radiografia',       'Hospital de Braga', 'Braga').

cuidado_prestado(12, 'Oftalmologia', 'Centro de Saude de Gualtar', 'Braga').
cuidado_prestado(13, 'Psicologia',   'Centro de Saude de Gualtar', 'Braga').
cuidado_prestado(14, 'Psiquiatria',  'Centro de Saude de Gualtar', 'Braga').

cuidado_prestado(15, 'Cirurgia',     'Hospital de Guimaraes', 'Guimaraes').
cuidado_prestado(16, 'Dermatologia', 'Hospital de Guimaraes', 'Guimaraes').
cuidado_prestado(17, 'Pediatria',    'Hospital de Guimaraes', 'Guimaraes').
cuidado_prestado(18, 'Pneumologia',  'Hospital de Guimaraes', 'Guimaraes').
cuidado_prestado(19, 'Psiquiatria',  'Hospital de Guimaraes', 'Guimaraes').
cuidado_prestado(20, 'Reumatologia', 'Hospital de Guimaraes', 'Guimaraes').

cuidado_prestado(21, 'Cardiologia',  'Hospital da Luz de Guimaraes', 'Guimaraes').
cuidado_prestado(22, 'Oftalmologia', 'Hospital da Luz de Guimaraes', 'Guimaraes').
cuidado_prestado(23, 'Ortopedia',    'Hospital da Luz de Guimaraes', 'Guimaraes').
cuidado_prestado(24, 'Radiografia',  'Hospital da Luz de Guimaraes', 'Guimaraes').

cuidado_prestado(25, 'Oftalmologia', 'Centro de Saude de Azurem', 'Guimaraes').
cuidado_prestado(26, 'Psicologia',   'Centro de Saude de Azurem', 'Guimaraes').

cuidado_prestado(27, 'Cardiologia',       'Hospital de S.Joao', 'Porto').
cuidado_prestado(28, 'Cirurgia',          'Hospital de S.Joao', 'Porto').
cuidado_prestado(29, 'Eletrocardiograma', 'Hospital de S.Joao', 'Porto').
cuidado_prestado(30, 'Oncologia',         'Hospital de S.Joao', 'Porto').
cuidado_prestado(31, 'Ortopedia',         'Hospital de S.Joao', 'Porto').
cuidado_prestado(32, 'Pediatria',         'Hospital de S.Joao', 'Porto').
cuidado_prestado(33, 'Pneumologia',       'Hospital de S.Joao', 'Porto').
cuidado_prestado(34, 'Radiografia',       'Hospital de S.Joao', 'Porto').
cuidado_prestado(35, 'Reumatologia',      'Hospital de S.Joao', 'Porto').

% Extensao do predicado que define a negacao forte do predicado cuidado_prestado
-cuidado_prestado(IdServ, Descricao, Instituicao, Cidade) :-
	nao(cuidado_prestado(IdServ, Descricao, Instituicao, Cidade)),
	nao(excecao(cuidado_prestado(IdServ, Descricao, Instituicao, Cidade))).

% Ivariante estrutural: nao permitir a insercao de conhecimento contraditorio

% Ao se inserir um cuidado prestado, tem de existir uma (e uma só) negação desse
% cuidado prestado, correspondente à obtida pela formalização do PMF para os cuidados
% prestados. Se essa negação existir, então não existe conhecimento perfeito positivo
% nem execepções ao cuidado prestado a inserir, pelo que este pode ser inserido.
+cuidado_prestado(IdServ, _, _, _) :: (
	solucoes(IdServ, -cuidado_prestado(IdServ, _, _, _), S),
	comprimento(S, N),
	N == 1
).

% Invariante estrutural: nao permitir a insercao de conhecimento repetido

% Antes de se inserir a negação explícita de um cuidado prestado, tem de existir
% na base de conhecimento uma e uma só negação explícita desse cuidado prestado:
% aquela que é obtida pela formalização do PMF.
+(-cuidado_prestado(IdServ, Descr, Inst, Cidade)) :: (
	solucoes(IdServ, -cuidado_prestado(IdServ, Descr, Inst, Cidade), S),
	comprimento(S, N),
	N == 1
).

% Invariante estrutural: nao permitir cuidados prestados com a mesma descricao,
%                        na mesma instituicao e cidade
+cuidado_prestado(_, Descr, Inst, Cidade) :: (
	solucoes((Descr, Inst, Cidade), cuidado_prestado(_, Descr, Inst, Cidade), S),
	comprimento(S, N),
	N == 1
).

% Invariante referencial: nao permitir que se remova um cuidado prestado enquanto
%                         existirem atos medicos a ele associados
-cuidado_prestado(IdServ, _, _, _) :: (
	nao(ato_medico(_, _, IdServ, _, _))
).

% Invariante referencial: nao permitir a remocao de cuidados prestados se
%                         estiverem atribuidos a profissionais

-cuidado_prestado(IdServ, _, _, _) :: (
	nao(atribuido(_, IdServ))
).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado ato_medico: Data, IdUt, IdServ, Custo, IdPro -> {V,F,D}

ato_medico(data(03,03,2017),  3,  3, 30, 12).
ato_medico(data(07,03,2017),  1, 20, 15,  5).
ato_medico(data(11,03,2017),  0, 14, 10,  3).
ato_medico(data(02,03,2017),  2, 16, 20,  8).
ato_medico(data(05,03,2017),  1, 31, 17,  9).
ato_medico(data(17,03,2017),  2,  7, 45,  8).
ato_medico(data(13,03,2017),  7, 18, 26,  8).
ato_medico(data(14,03,2017),  4,  6, 33, 10).
ato_medico(data(07,03,2017),  8, 10,  5,  9).
ato_medico(data(01,03,2017),  3,  2, 14,  9).
ato_medico(data(28,02,2017),  5, 33, 37, 12).
ato_medico(data(24,02,2017), 10, 26,  7,  4).
ato_medico(data(16,03,2017),  9, 30, 16, 13).
ato_medico(data(16,03,2017),  4, 16, 22,  8).
ato_medico(data(14,03,2017),  6,  7, 14,  8).
ato_medico(data(05,03,2017),  7, 19,  3,  9).
ato_medico(data(09,03,2017),  0, 24, 24, 10).
ato_medico(data(26,02,2017),  2,  5, 27,  3).
ato_medico(data(19,03,2017),  5, 14, 13,  3).
ato_medico(data(15,03,2017),  4, 13, 26,  7).
ato_medico(data(06,03,2017),  8, 28, 50,  5).
ato_medico(data(02,03,2017),  6, 34, 31,  2).
ato_medico(data(27,02,2017),  9,  2, 18,  9).
ato_medico(data(14,03,2017), 10,  1, 25,  2).
ato_medico(data(13,03,2017),  7, 22,  9,  9).

% Invariante referencial: nao permitir a insercao de atos medicos
%                         relativos a servicos ou utentes inexistentes

+ato_medico(_, IdUt, IdServ, _, _) :: (
	utente(IdUt, _, _, _),
	cuidado_prestado(IdServ, _, _, _)
).

% Extensao do predicado que define a negacao forte do predicado ato_medico
-ato_medico(Data, IdUt, IdServ, Custo, IdPro) :-
	nao(ato_medico(Data, IdUt, IdServ, Custo, IdPro)),
	nao(excecao(ato_medico(Data, IdUt, IdServ, Custo, IdPro))).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado data: D, M, A -> {V,F}

data(D, M, A) :-
	pertence(M, [1,3,5,7,8,10,12]),
	D >= 1,
	D =< 31.
data(D, M, A) :-
	pertence(M, [4,6,9,11]),
	D >= 1,
	D =< 30.
data(D, 2, A) :-
	A mod 4 =\= 0, % ano nao bissexto
	D >= 1,
	D =< 28.
data(D, 2, A) :-
	A mod 4 =:= 0,
	D >= 1,
	D =< 29.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado selecionar_utentes: IdUt, Nome, Idade, Morada, R -> {V,F}

selecionar_utentes(IdUt, Nome, Idade, Morada, R) :- 
	solucoes((IdUt, Nome, Idade, Morada), utente(IdUt, Nome, Idade, Morada), R).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado instituicoes: Cidade, R -> {V,F}
%
% Instituicoes que prestam cuidados medicos

instituicoes(Cidade, R) :-
	solucoes((Inst, Cidade), cuidado_prestado(_, _, Inst, Cidade), L),
	unicos(L,R).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado cuidados: Instituicao, Cidade, R -> {V,F}
%
% Identificar os cuidados prestados por instituicao/cidade

cuidados(Inst, Cidade, R) :-
	solucoes((Descr, Inst, Cidade), cuidado_prestado(_, Descr, Inst, Cidade), R).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado utentes_inst_serv: Instituicao, Servico, R -> {V,F}
%
% Identificar os utentes de uma instituicao/servico

utentes_inst_serv(Inst, Serv, R) :-
	solucoes(
		(IdUt, Nome),
		(
			cuidado_prestado(IdServ, Serv, Inst, _),
		    ato_medico(_, IdUt, IdServ, _, _),
		    utente(IdUt, Nome, _, _)
		),
		L
	),
	unicos(L,R).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado recorreu: IdUt,R -> {V,F}
%
% Identificar todas as instituicoes/servicos a que um utente ja recorreu

recorreu(IdUt, R) :-
	solucoes(
		(Inst,Serv),
		(
			ato_medico(_, IdUt, IdServ, _),
		 	cuidado_prestado(IdServ, Serv, Inst, _)
		),
		L
	),
	unicos(L,R).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado atos_medicos: IdUt, Instituicao, Servico, IdPro, R -> {V,F}
%
% Identificar os atos medicos realizados por utente/instituicao/servico/profissional
%

atos_medicos(IdUt, Inst, Serv, IdPro, R) :-
	solucoes(
		(Data, (IdUt, NomeU), Serv, Inst, Custo, (IdPro, NomeP)),
		(
			cuidado_prestado(IdServ, Serv, Inst, _),
			ato_medico(Data, IdUt, IdServ, Custo, IdPro),
		  utente(IdUt, NomeU, _, _),
		  profissional(IdPro, NomeP, _, _)
		),
		R
	).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado custo: IdUt,Serv,Inst,Data,IdPro,R -> {V,F}
%
% Calcular o custo dos atos medicos por utente/servico/instituicao/data/profissional
%

custo(IdUt, Serv, Inst, Data, IdPro, R) :-
	solucoes(
		Custo,
		(
			cuidado_prestado(IdServ, Serv, Inst,_),
			ato_medico(Data, IdUt, IdServ, Custo, IdPro)
		),
		S
	),
	soma(S, R).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado
% atos_medicos_interv: IdUt, Instituicao, Servico, IdPro, DataIni, DataFim, R -> {V,F}
%
% Identificar os atos medicos realizados por utente/instituicao/servico/profissional
% num intervalo de datas

atos_medicos_interv(IdUt, Inst, Serv, IdPro, Di, Df, R) :-
	solucoes(
		(Data, (IdUt, NomeU), Serv, Inst, Custo, (IdPro, NomeP)),
		(
			cuidado_prestado(IdServ, Serv, Inst, _),
			ato_medico(Data, IdUt, IdServ, Custo, _),
			nao(cmp_datas(Data, Di, <)),
		  nao(cmp_datas(Data, Df, >)),
		  utente(IdUt, NomeU, _, _),
		  profissional(IdPro, NomeP, _, _)
		),
		R
	).

% Extensao do predicado soma: L, N -> {V,F}

soma([],0).
soma([N|Ns], T) :- soma(Ns,X), T is X+N.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado utente: X, L -> {V,F}

pertence(H,[H|T]).
pertence(X,[H|T]) :-
	X \= H,
	pertence(X,T).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado utente: L, R -> {V,F}

unicos([],[]).
unicos([H|T], R) :-
	pertence(H,T),
	unicos(T,R).
unicos([H|T], [H|R]) :-
	nao(pertence(H,T)),
	unicos(T,R).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado comprimento: L,N -> {V,F}

comprimento([], 0).
comprimento([H|T], N) :- comprimento(T,K), N is K+1.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado cmp_datas: Data1, Data2, R -> {V,F}
%
% O predicado cmp_datas compara duas datas. O resultado da comparacao e:
%   <  se a primeira data for anterior à segunda;
%   =  se as datas foram iguais;
%   >  se a primeira data for posterior à segunda.
%
% Nota: cada data é dada pelo predicado data: D,M,A -> {V,F}

cmp_datas(data(_, _, A1), data(_, _, A2), R) :-
	A1 \= A2, compare(R, A1, A2).
cmp_datas(data(_, M1, A), data(_, M2, A), R) :-
	M1 \= M2, compare(R, M1, M2).
cmp_datas(data(D1, M, A), data(D2, M, A), R) :-
	compare(R, D1, D2).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado nao: Q -> {V,F}

nao(Q) :- Q, !, fail.
nao(Q).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado solucoes: F, Q, S -> {V,F}

solucoes(F,Q,S) :- Q, assert(tmp(F)), fail.
solucoes(F,Q,S) :- construir(S, []).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado construir: S1,S2 -> {V,F}

construir(S1, S2) :-
	retract(tmp(X)), !, construir(S1, [X|S2]).
construir(S,S).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado que permite a evolucao do conhecimento
% ALTERADO PARA A PARTE II

evolucao(Termo) :-
	solucoes(Inv, +Termo::Inv, LInv),
	testa(LInv),
	assert(Termo).

inserir(Termo) :- assert(Termo).
inserir(Termo) :- retract(Termo), !, fail.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado que permite a involucao do conhecimento
% ALTERADO PARA A PARTE II

involucao(Termo) :-
	Termo,
	solucoes(Inv, -Termo::Inv, LInv),
	testa(LInv),
	retract(Termo).

remover(Termo) :- retract(Termo).
remover(Termo) :- assert(Termo), !, fail.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado que testa uma lista de invariantes

testa([]).
testa([I|T]) :- I, testa(T).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado profissional: IdPro, Nome, Idade, Morada -> {V,F,D}

profissional( 1, 'Jose',      45, 'Rua Escura').
profissional( 2, 'Manuel',    37, 'Avenida da Liberdade').
profissional( 3, 'Carlos',    62, 'Rua Augusta').
profissional( 4, 'Rodrigo',   53, 'Avenida Central').
profissional( 5, 'Eduarda',   49, 'Rua de Santa Maria').
profissional( 6, 'Francisco', 33, 'Rua dos Pescadores').
profissional( 7, 'Joao',      64, 'Avenida Carvalho').
profissional( 8, 'Maria',     58, 'Rua da Alegria').
profissional( 9, 'Carla',     47, 'Rua das Flores').
profissional(10, 'Andreia',   39, 'Rua do Saco').
profissional(11, 'Vitor',     56, 'Avenida da Memoria').
profissional(12, 'Luisa',     60, 'Rua da Tampa').
profissional(13, 'Ines',      53, 'Rua da Pedra').
profissional(14, 'Jorge Simoes', 28, 'Rua Ferreira de Castro').

% PARTE II
% Invariantes estruturais: nao permitir a insercao de conhecimento contraditorio

+profissional(IdPro, _, _, _) :: (
	solucoes(IdPro, -profissional(IdPro, _, _, _), S),
	comprimento(S, N),
	N == 1
).

% PARTE II
% Invariante estrutural: nao permitir a insercao de conhecimento repetido

+(-profissional(IdPro, Nome, Idade, Morada)) :: (
	solucoes(IdPro, -profissional(IdPro, Nome, Idade, Morada), S),
	comprimento(S, N),
	N == 1
).

% Invariante referencial: nao permitir a remocao de profissionais se estiverem atribuidos a cuidados_prestados

-profissional(Id, _, _, _) :: (
	nao(atribuido(Id, _))
).

% Invariante referencial: nao permitir a remocao de profissionais se estiverem associados a atos medicos

-profissional(Id, _, _, _) :: (
	nao(ato_medico(_, _, _, _, Id))
).

% Extensao do predicado que define a negacao forte do predicado profissional
-profissional(IdPro, Nome, Idade, Morada) :-
	nao(profissional(IdPro, Nome, Idade, Morada)),
	nao(excecao(profissional(IdPro, Nome, Idade, Morada))).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado atribuido: IdPro, IdServ -> {V,F,D}

atribuido( 1, 15).
atribuido( 1, 28).
atribuido( 2,  1).
atribuido( 2, 23).
atribuido( 2, 34).
atribuido( 3, 14).
atribuido( 3,  5).
atribuido( 4, 26).
atribuido( 5, 17).
atribuido( 5, 28).
atribuido( 5,  9).
atribuido( 5, 20).
atribuido( 6, 11).
atribuido( 6, 32).
atribuido( 7, 13).
atribuido( 7,  4).
atribuido( 7, 25).
atribuido( 8, 16).
atribuido( 8,  7).
atribuido( 8, 18).
atribuido( 9, 19).
atribuido( 9, 10).
atribuido( 9, 31).
atribuido( 9, 22).
atribuido( 9,  2).
atribuido(10, 24).
atribuido(10, 15).
atribuido(10,  6).
atribuido(10, 27).
atribuido(11,  8).
atribuido(11, 29).
atribuido(11, 10).
atribuido(11, 21).
atribuido(12, 12).
atribuido(12, 33).
atribuido(12,  3).
atribuido(12, 35).
atribuido(13, 30).
atribuido(13, 24).

% Invariantes estruturais: nao permitir a insercao de conhecimento 
%                          contraditorio

+atribuido(IdPro, IdServ) :: (
	solucoes((IdPro,IdServ), -atribuido(IdPro, IdServ), S),
	comprimento(S, N),
	N == 1
).

% Invariante estrutural: nao permitir a insercao de conhecimento repetido

+(-atribuido(IdPro, IdServ)) :: (
	solucoes((IdPro,IdServ), -atribuido(IdPro, IdServ), S),
	comprimento(S, N),
	N == 1
).

% Invariante referencial: nao permitir a remocao de atribuicoes se exisitrem atos medicos 
%                         que "dependam" dessa atribuicao

-atribuido(IdPro, IdServ) :: (
	nao(ato_medico(_, _, IdServ, _, IdPro))
).

% Extensao do predicado que define a negacao forte do predicado atribuido
-atribuido(IdPro, IdServ) :-
	nao(atribuido(IdPro, IdServ)),
	nao(excecao(atribuido(IdPro, IdServ))).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado profissionais: Inst, Serv, R -> {V,F}
% Calcula a lista de profissionais de uma instituicao/servico

profissionais(Inst, Serv, R) :-
	solucoes(
		(IdPro, Nome),
		(
			cuidado_prestado(IdServ, Serv, Inst, _),
		    atribuido(IdPro, IdServ),
		    profissional(IdPro, Nome, _, _)
		),
		L
	),
	unicos(L,R).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado selecionar_profissionais: IdPro, Nome, Idade, Morada -> {V,F}

selecionar_profissionais(IdPro, Nome, Idade, Morada, R) :- 
	solucoes((IdPro, Nome, Idade, Morada), profissional(IdPro, Nome, Idade, Morada), R).


%--------------------------------- - - - - - - - - - -  -  -  -  -
% PARTE II
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%--------------------------------- - - - - - - - - - -  -  -  -  -   -

% Extensao do meta-predicado demo :: Expressão, Resposta -> {V,F}
demo( P <=> X, V ) :- demo( P, V1 ), demo( X, V2 ), equivalencia( V1, V2, V ), !.
demo( P => X, V ) :- demo( P, V1 ), demo( X, V2 ), implicacao( V1, V2, V ), !.
demo( P $$ X, V ) :- demo( P, V1 ), demo( X, V2 ), disjuncao( V1, V2, V ), !.
demo( P && X, V ) :- demo( P, V1 ), demo( X, V2 ), conjuncao( V1, V2, V ), !.

equivalencia( X, X, verdadeiro ) :- X \= desconhecido.
equivalencia( desconhecido, Y, desconhecido ).
equivalencia( X, desconhecido, desconhecido ).
equivalencia( verdadeiro, falso, falso ). 
equivalencia( verdadeiro, falso, falso ). 

implicacao( falso, X, verdadeiro ).
implicacao( X, verdadeiro, verdadeiro ).
implicacao( verdadeiro, desconhecido, desconhecido ). 
implicacao( desconhecido, X, desconhecido ) :- X \= verdadeiro.
implicacao( verdadeiro, falso, falso ).

disjuncao( verdadeiro, X, verdadeiro ).
disjuncao( X, verdadeiro, verdadeiro ).
disjuncao( desconhecido, Y, desconhecido ) :- Y \= verdadeiro.
disjuncao( Y, desconhecido, desconhecido ) :- Y \= verdadeiro.
disjuncao( falso, falso, falso ).

conjuncao( verdadeiro, verdadeiro, verdadeiro ).
conjuncao( falso, _, falso ).
conjuncao( _, falso, falso ).
conjuncao( desconhecido, verdadeiro, desconhecido ).
conjuncao( verdadeiro, desconhecido, desconhecido ).

demo( P , verdadeiro ) :- P.
demo( P, falso ) :- -P.
demo( P, desconhecido ) :- nao( P ), nao( -P ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado doenca :: IdDoenca, Designacao, Descricao -> {V,F}
doenca(1, 'SIDA', 'Uma camisinha nunca fez mal a ninguém' ).
doenca(2, 'Traumatismo Craniano', 'Lesão na cabeça').
doenca(3, 'Ombro Deslocado', 'Deslocamento da articulação do ombro').

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado diagnostico: Data, IdUtente, IdDoenca -> {V,F,D}

diagnostico(data(1,1,2017), 2, 1).

% Invariantes estruturais: impede conhecimento contraditório relativo aos diagnosticos
+diagnostico(_, IdUt, IdDoenca) :: (
	solucoes((IdUt,IdDoenca), -diagnostico(_, IdUt, IdDoenca), S),
	comprimento(S, N),
	N == 1
).

+(-diagnostico(_, IdUt, IdDoenca)) :: (
	solucoes((IdUt,IdDoenca), -diagnositico(_, IdUt, IdDoenca), S),
	comprimento(S, N),
	N == 1
).

% Extensão do predicado que define a negação forte do predicado diagnostico
-diagnostico(Data, IdUt, IdDoenca) :-
	nao(diagnostico(Data, IdUt, IdDoenca)),
	nao(excecao(diagnostico(Data, IdUt, IdDoenca))).

% ----------------------------------------------------------------------------
% ----------------------------------------------------------------------------

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado evolucaoPerfeito: ConhecimentoPerfeito -> {V,F}

% Evolucao de conhecimento perfeito que remove conhecimento impreciso/incerto

evolucaoPerfeito(utente(IdUt,Nome,Idade,Morada)) :-
	solucoes(Inv, +utente(IdUt,Nome,Idade,Morada)::Inv, LInv),
	testa(LInv),
	removerImpreciso(utente(IdUt,Nome,Idade,Morada)),
	assert(utente(IdUt,Nome,Idade,Morada)),
	assert(perfeito(utente(IdUt))).

evolucaoPerfeito((-utente(IdUt, Nome, Idade, Morada))) :-
	solucoes(Inv, +(-utente(IdUt,Nome,Idade,Morada))::Inv, LInv),
	testa(LInv),
	removerImpreciso(utente(IdUt,Nome,Idade,Morada)),
	assert((-utente(IdUt,Nome,Idade,Morada))),
	assert(perfeito(utente(IdUt))).


% Remocao de conhecimento impreciso (nao é involucao!)
% É procedimental, tal como os predicados de evolucao, solucoes, etc.
removerImpreciso(utente(IdUt, Nome, Idade, Morada)) :-
	retract(excecao(utente(IdUt,_,_,_))),
	removerImpreciso(utente(IdUt,Nome,Idade,Morada)).

removerImpreciso(utente(IdUt,Nome,Idade,Morada)) :-
	retract(impreciso(utente(IdUt))),
	removerImpreciso(utente(IdUT,Nome,Idade,Morada)).

removerImpreciso(utente(IdUt, Nome, Idade, Morada)) :-
	removerIncerto(utente(IdUt,Nome,Idade,Morada)).


% Remocao de conhecimento incerto (nao é involucao!!)

removerIncerto(utente(IdUt,Nome,Idade,Morada)) :-
	incertoIdade(utente(IdUt,I)),
	retract((excecao(utente(Id,N,Ida,M)) :-
		utente(Id,N,I,M))),
	retract(utente(IdUt, _, _ ,_)),
	retract(incertoIdade(utente(IdUt, _))).

removerIncerto(utente(IdUt,Nome,Idade,Morada)).

% Para se remover conhecimento incerto sobre outro argumento
% basta criar uma extensao do predicado removerIncerto
% análoga à apresentada


% Invariantes que nao permitem inserir conhecimento perfeito
% se ja existir conhecimento perfeito

+utente(IdUt, Nome, Idade, Morada) :: (
	nao(perfeito(utente(IdUt)))
).

+(-utente(IdUt, Nome, Idade, Morada)) :: (
	nao(perfeito(utente(IdUt)))
).

% Evolucao de conhecimento impreciso que remove conhecimento incerto

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado evolucaoImpreciso: EnumeracaoFactosAnomalos -> {V,F}

evolucaoImpreciso([utente(IdUt, Nome, Idade, Morada)|T]) :-
	T \= [],
	mesmoUtente(T, IdUt),
	testaInvs([utente(IdUt, Nome, Idade, Morada)|T]),
	removerIncerto(utente(IdUt, Nome, Idade, Morada)),
	insereExcecoes([utente(IdUt, Nome, Idade, Morada)|T]).

mesmoUtente([], _).
mesmoUtente([utente(Id1, _, _, _) | T], Id2) :-
	Id1 == Id2,
	mesmoUtente(T, Id2).

testaInvs([]).
testaInvs([P|Ps]) :-
	solucoes(Inv, +P::Inv, LInv1),
	solucoes(Inv, +P:~:Inv, LInv2),
	testa(LInv1),	 
	testa(LInv2),
	testaInvs(Ps).

insereExcecoes([]).
insereExcecoes([utente(IdUt, Nome, Idade, Morada)|Es]) :-
	assert(excecao(utente(IdUt, Nome, Idade, Morada))),	
	assert(impreciso(utente(IdUt))),
	insereExcecoes(Es).


% Invariante que nao permite inserir conhecimento impreciso se
% ja existir conhecimento perfeito ou impreciso

+utente(IdUt, Nome, Idade, Morada) :~: (
	nao(perfeito(utente(IdUt))),
	nao(impreciso(utente(IdUt)))
).

% Evolucao de conhecimento incerto sobre a idade

evolucaoIncertoIdade(utente(IdUt, Nome, Idade, Morada)) :-
	solucoes(Inv, +utente(IdUt,Nome,Idade,Morada):-:Inv, LInv1),
	solucoes(Inv, +utente(IdUt, Nome, Idade, Morada)::Inv, LInv2),
	testa(LInv1),
	testa(LInv2),
	assert((excecao(utente(Id,N,I,M)) :-
	       utente(Id,N,Idade,M))),
	assert(utente(IdUt, Nome, Idade, Morada)),
	assert(incertoIdade(utente(IdUt,Idade))).


% Invariante que nao permite a insercao de conhecimento
% incerto/interdito se ja exisitir conhecimento.
+utente(IdUt, Nome, Idade, Morada) :-: (
	nao(perfeito(utente(IdUt))),
	nao(impreciso(utente(IdUt))),
	nao(incerto(utente(IdUt)))
).

incerto(utente(IdUt)) :-
	incertoIdade(utente(IdUt, _)).
incerto(utente(IdUt)) :-
	incertoMorada(utente(IdUt, _)).

% Evolucao de conhecimento interdito sobre a idade

evolucaoInterditoIdade(utente(IdUt, Nome, Idade, Morada)) :-
	solucoes(Inv, +utente(IdUt,Nome,Idade,Morada):-:Inv, LInv1),
	solucoes(Inv, +utente(IdUt, Nome, Idade, Morada)::Inv, LInv2),
	testa(LInv1),
	testa(LInv2),
	assert(nulo(Idade)),
	assert((excecao(utente(Id,N,I,M)) :-
	       utente(Id,N,Idade,M))),
	assert((+utente(Id,N,I,M) :: (
				       solucoes(Id,(utente(Id,_,Idade,_), nulo(Idade)),S),
				       comprimento(S,0)
				     ))),
	assert(utente(IdUt, Nome,Idade,Morada)).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado involucaoPerfeito: ConhecimentoAremover -> {V,F}

involucaoPerfeito(utente(IdUt, Nome, Idade, Morada)) :-
	utente(IdUt, Nome, Idade, Morada),
	solucoes(Inv, -utente(IdUt,Nome,Idade,Morada)::Inv, LInv),
	testa(LInv),
	retract(utente(IdUt, Nome, Idade, Morada)),
	retract(perfeito(utente(IdUt))).

involucaoPerfeito((-utente(IdUt, Nome, Idade, Morada))) :-
	utente(IdUt, Nome, Idade, Morada),
	solucoes(Inv, -(-utente(IdUt,Nome,Idade,Morada))::Inv, LInv),
	testa(LInv),
	retract(utente(IdUt, Nome, Idade, Morada)),
	retract(perfeito(utente(IdUt))).

% Involução do conhecimento incerto relativo à idade de um utente
involucaoIncertoIdade(utente(IdUt, Nome, Idade, Morada)) :-
	utente(IdUt, Nome,Idade,Morada),
	incertoIdade(utente(IdUt, I)),
	solucoes(Inv, -utente(IdUt, Nome, Idade, Morada)::Inv, LInv),
	testa(LInv),
	retract((excecao(utente(Id,N,Ida,M)) :-
		utente(Id,N,I,M))),
	retract(utente(IdUt, _, _ ,_)),
	retract(incertoIdade(utente(IdUt, _))).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado involucaoImpreciso: EnumeracaoFactosAnomalos -> {V,F}
involucaoImpreciso([utente(Id,Nome,Idade,Morada) | T]) :-
    procuraPor([utente(Id,Nome,Idade,Morada) | T]),
    mesmoUtente(T, Id),
    testaInvolInvs([utente(Id,Nome,Idade,Morada) | T]),
    removeExcecoes([utente(Id,Nome,Idade,Morada) | T]).

% Remove a excecao a cada um dos predicados de uma lista de predicados
removeExcecoes([]).
removeExcecoes([utente(IdUt,Nome,Idade,Morada)|Ps]) :-
	retract(excecao(utente(IdUt, Nome, Idade, Morada))),
	retract(impreciso(utente(IdUt))),
	removeExcecoes(Ps).

% Procura por uma excecao para cada um dos termos de uma lista de termos
procuraPor([]).
procuraPor([T|Ts]) :-
    excecao(T), procuraPor(Ts).

% Testa os invariantes de remoçã de uma lista para cada um
% dos elementos de uma lista de predicados
testaInvolInvs([]).
testaInvolInvs([P|Ps]) :- 
    solucoes(Inv, -P::Inv, LInv),
    testa(LInv),
    testaInvolInvs(Ps).

% Involucao do conhecimento interdito relativo a idade de um utente
involucaoInterditoIdade(utente(IdUt, Nome, Idade, Morada)) :-
	utente(IdUt, Nome, Idade, Morada),
	nulo(Idade),
	solucoes(Inv, -utente(IdUt, Nome, Idade, Morada)::Inv, LInv),
	testa(LInv),
	retract(nulo(Idade)),
	retract((excecao(utente(Id,N,I,M)) :-
	       utente(Id,N,Idade,M))),
	retract((+utente(Id,N,I,M) :: (
				       solucoes(Id,(utente(Id,_,Idade,_), nulo(Idade)),S),
				       comprimento(S,0)
				     ))),
	retract(utente(IdUt, Nome,Idade,Morada)).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Conhecimento perfeito negativo

% Não existe um utente com o Id 100, chamado Bernardino,
% que tenha 29 anos e more na rua do pinheiro.
-utente(100, 'Bernardino', 29, 'Rua do Pinheiro').
perfeito(utente(100)).

% Não existe um serviço de pneumologia com o id 34, no
% centro de saude de Gualtar, em Braga.
-cuidado_prestado(34, 'Pneumologia', 'Centro de Saude de Gualtar', 'Braga').

% No dia 9 de Janeiro de 2017, não foi realizado qualquer ato médico
% ao utente com o id 1, no serviço 3, que tenha tido um custo de
% 75 unidades monetárias e o profissional 2 como responsável.
-ato_medico(data(9,1,2017), 1, 3, 75, 2).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Representação de conhecimento imperfeito

% Tipo I. Conhecimento Incerto

% No dia 4 de Abril de 2017, o senhor Joaquim foi a uma
% consulta de cardiologia no “Hospital de Braga” e sabe-se que esta teve
% um custo de 50 euros, mas não se sabe quem foi o profissional responsável.

ato_medico(data(4,4,2017), 11, 6, 50, nulo1).
excecao(ato_medico(Data, IdUt, IdServ, Custo, IdPro)) :-
    ato_medico(Data, IdUt, IdServ, Custo, nulo1).

% A utente Maria de Lurdes tem 68 anos, mas desconhece-se a sua morada.

utente(12, 'Maria de Lurdes', 68, nulo2).
excecao(utente(Id, Nome, Idade, Morada)) :-
    utente(Id, Nome, Idade, nulo2).
incertoMorada(utente(12, nulo2)).


% Tipo II. Conhecimento Impreciso

% O senhor Alfredo tem 74 anos e devido aos seus problemas na fala,
% aquando do seu registo como utente, não deu para perceber se mora
% na "rua de Barros" ou na "rua de Baixo".

excecao(utente(14, 'Alfredo', 74, 'Rua de Baixo')).
excecao(utente(14, 'Alfredo', 74, 'Rua de Barros')).
impreciso(utente(14)).
impreciso(utente(14)).

% No dia 1 de Janeiro de 2017, o senhor Manuel lesionou-se a jogar futebol,
% e nesse mesmo dia foi diagnosticado, mas não se sabe se lhe foi diagnosticado
% um traumatismo craniano ou um ombro deslocado.

excecao(diagnostico(2,2)).
excecao(diagnostico(2,3)).

% Tipo III. Conhecimento Interdito

% No dia 17 de Fevereiro de 2017, o doutor José (id = 1) realizou uma cirurgia
% no hospital S. João no Porto, tendo esta custado 1200 euros, contudo
% não se pode saber que o utente operado foi o senhor Fernando (id = 6)

ato_medico(data(17,2,2017), nulo3, 28, 1200, 1).
excecao(ato_medico(Data, IdUt, IdServ, Custo, IdPro)) :-
    ato_medico(Data, nulo3, IdServ, Custo, IdPro).
nulo(nulo3).
+ato_medico(Data, IdUt, IdServ, Custo, IdPro) :: (
    solucoes(IdUtVar, (ato_medico(data(17,2,2017), IdUtVar, 28, 1200, 1), nao(nulo(IdUtVar))), S),
    comprimento(S,0)
).

% Não se pode saber a idade do utente Renato Sancho, que mora na rua do Moinho.

utente(15, 'Renato Sancho', nulo4, 'Rua do Moinho').
excecao(utente(Id, Nome, Idade, Morada)) :-
    utente(Id, Nome, nulo4, Morada).
nulo(nulo4).
+utente(Id, Nome, Idade, Morada) :: (
    solucoes(IdadeVar, (utente(15,'Renato Sancho', IdadeVar, 'Rua do Moinho'), nulo(IdadeVar)), L),
    comprimento(L,0)
).
