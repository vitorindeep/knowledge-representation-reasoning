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
:- dynamic utente/4.
:- dynamic cuidado_prestado/4.
:- dynamic profissional/4.
:- dynamic atribuido/2.
:- dynamic ato_medico/5.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado utente: IdUt, Nome, Idade, Morada -> {V,F}

utente( 0, 'Jose',     55, 'Rua dos Zecas').
utente( 1, 'Joao',     21, 'Rua de Baixo').
utente( 2, 'Manuel',   36, 'Rua Maria Albertina').
utente( 3, 'Carlos',   43, 'Rua da Fabrica').
utente( 4, 'Maria',    73, 'Avenida Camoes').
utente( 5, 'Joana',     8, 'Avenida Camoes').
utente( 6, 'Fernando', 49, 'Rua da Beira').
utente( 7, 'Joao',     29, 'Rua da Encosta').
utente( 8, 'Ana',      40, 'Avenida Soares').
utente( 9, 'Catarina', 17, 'Avenida Carneiro').
utente(10, 'Maria',    33, 'Rua da Pata').

% Invariante estrutural: nao permitir a insercao de conhecimento
%                        repetido

+utente(IdUt, _, _, _) :: (
	solucoes(IdUt, utente(IdUt, _, _, _), S),
	comprimento(S, N),
	N == 1
).

% Invariante: a idade de cada utente tem de ser inteira e
%             estar no intervalo fechado [0,150]

+utente(_, _, Idade, _) :: (
	integer(Idade),
	Idade >= 0, Idade =< 150
).

% Invariante referencial: nao permitir que se remova um utente enquanto
%                         existirem atos medicos associados a si

-utente(IdUt, _, _, _) :: (
	nao(ato_medico(_, IdUt, _, _, _))
).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado cuidado_prestado: IdServ, Descricao, Instituicao, Cidade -> {V,F}

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

% Invariante estrutural: nao permitir a insercao de conhecimento repetido

+cuidado_prestado(IdServ, _, _, _) :: (
	solucoes(IdServ, cuidado_prestado(IdServ, _, _, _), S),
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
% Extensao do predicado ato_medico: Data, IdUt, IdServ, Custo, IdPro -> {V,F}

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

% Invariante: o primeiro argumento do ato_medico tem de ser o predicado data

+ato_medico(X, _, _, _, _) :: (e_data(X)).

% Invariante ???????: o custo dos atos medicos tem de ser um numero
%                     nao negativo

+ato_medico(_, _, _, Custo, _) :: (number(Custo), Custo >= 0).

% Invariante referencial: nao permitir a insercao de atos medicos relativos a profissionais inexistentes

+ato_medico(_, _, _, _, IdPro) :: (profissional(IdPro, _, _, _)).

% Invariante referencial: nao permitir a insercao de atos medicos relativos a profissionais que nao estejam
% atribuidos ao cuidado prestado.
% Com este invariante, o invariante de cima torna-se desnecessario, ja que para um profissional estar atribuido a 
% um cuidado, o profissional tem de existir

+ato_medico(_, _, IdServ, _, IdPro) :: (atribuido(IdPro, IdServ)).

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
% Extensao do predicado e_data: X -> {V,F}

e_data(data(D, M, A)) :- data(D, M, A).

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

evolucao(Termo) :-
	solucoes(Inv, +Termo::Inv, LInv),
	inserir(Termo),
	testa(LInv).

inserir(Termo) :- assert(Termo).
inserir(Termo) :- retract(Termo), !, fail.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado que permite a involucao do conhecimento

involucao(Termo) :-
	Termo,
	solucoes(Inv, -Termo::Inv, LInv),
	remover(Termo),
	testa(LInv).

remover(Termo) :- retract(Termo).
remover(Termo) :- assert(Termo), !, fail.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado que testa uma lista de invariantes

testa([]).
testa([I|T]) :- I, testa(T).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado profissional: IdPro, Nome, Idade, Morada -> {V,F}

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

% Invariante estrutural: nao permitir a insercao de conhecimento repetido

+profissional(Id, _, _, _) :: (
	solucoes(Id, profissional(Id, _, _, _), S),
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

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado atribuido: IdPro, IdServ -> {V,F}

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


% Invariante referencial: nao permitir a insercao de atribuicoes relativas
%                         a profissionais e/ou cuidados prestados inexistentes

+atribuido(IdPro, IdServ) :: (
	profissional(IdPro, _, _, _),
	cuidado_prestado(IdServ, _, _, _)
).

% Invariante estrutural: nao permitir a insercao de conhecimento repetido

+atribuido(IdPro, IdServ) :: (
	solucoes((IdPro, IdServ), atribuido(IdPro, IdServ), L),
	comprimento(L, N),
	N == 1
).

% 

-atribuicao(IdPro, IdServ) :: (
    nao(ato_medico(_, _, IdServ, _, IdPro)
).


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
