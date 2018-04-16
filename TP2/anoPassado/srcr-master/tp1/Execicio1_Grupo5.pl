%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SIST. REPR. CONHECIMENTO E RACIOCINIO - MiEI/3


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SICStus PROLOG: Declaracoes iniciais


:- set_prolog_flag( discontiguous_warnings,off ).
:- set_prolog_flag( single_var_warnings,off ).
:- set_prolog_flag( unknown,fail ).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SICStus PROLOG: definicoes iniciais
% isto serve para definir o :: dos invatiantes

:- op( 900,xfy,'::' ).
:- dynamic instituicao/1.
:- dynamic servico/1.
:- dynamic utente/1.
:- dynamic profissional/1.
:- dynamic e_servico/2.
:- dynamic e_utente/3.
:- dynamic profissional_servico/2.
:- dynamic profissional_instituicao/2.
:- dynamic consultas/4.


% --------------------------------------------------------------------
% Instituições

instituicao( cspovoa ).
instituicao( csbarca ).
instituicao( csmontalegre ).
instituicao( csplima ).
instituicao( csbraga ).


% --------------------------------------------------------------------
% Serviços

servico( oftalmologia ).
servico( pediatria ).
servico( ginecologia ).
servico( ortopedia ).
servico( dermatologia ).
servico( psicologia ).
servico( cirurgia ).
servico( fisioterapia ).
servico( analises ).
servico( servicogeral ).
servico( dentista ).


% --------------------------------------------------------------------
% Utentes

utente( zeca).
utente( chico ).
utente( ramada ).
utente( rafa ).
utente( jessica ).
utente( alex ).
utente( joana ).
utente( lucinda ).
utente( tino ).
utente( catarina ).
utente( marcos ).
utente( renato ).
utente( melia ).
utente( cinda ).
utente( manuela ).
utente( conceicao ).
utente( manuel ).
utente( andre ).


% --------------------------------------------------------------------
% Profissionais

profissional( mario ).
profissional( elvira ).
profissional( ivone ).
profissional( jose ).
profissional( romario ).
profissional( joao ).
profissional( jorge ).
profissional( luisa ).
profissional( teresa ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado e_servico: Servico,Instituicao -> {V,F}

e_servico( oftalmologia, csbraga ).
e_servico( ginecologia,csbraga ).
e_servico( ginecologia,cspovoa ).
e_servico( pediatria,csbraga ).
e_servico( pediatria,csbarca ).
e_servico( servicogeral,cspovoa ).
e_servico( dermatologia, csplima).
e_servico( oftalmologia, csplima ).
e_servico( cirurgia, csplima ).
e_servico( dentista, csbarca ).
e_servico( oftalmologia, cspovoa ).
e_servico( ortopedia, csbarca ).
e_servico( psicologia, csmontalegre ).
e_servico( fisioterapia, csmontalegre ).
e_servico( dentista, csmontalegre ).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado e_utente: Utente, Servico, Instituicao -> {V,F}

e_utente( zeca,oftalmologia,csbraga ).
e_utente( zeca,pediatria,csbraga ).
e_utente( zeca,pediatria,csbarca ).
e_utente( rafa,pediatria,csbraga ).
e_utente( rafa,dentista,csbarca ).
e_utente( rafa,oftalmologia,cspovoa ).
e_utente( chico,servicogeral,cspovoa ).
e_utente( chico,oftalmologia,cspovoa ).
e_utente( chico,pediatria,csbraga ).
e_utente( chico,cirurgia,csplima ).
e_utente( catarina,ginecologia,cspovoa ).
e_utente( catarina,fisioterapia,csmontalegre ).
e_utente( catarina,dentista,csmontalegre ).
e_utente( jessica,dermatologia,csplima ).
e_utente( jessica,oftalmologia,csplima ).
e_utente( jessica,cirurgia,csplima ).
e_utente( marcos,servicogeral,cspovoa ).
e_utente( marcos,ortopedia,csbarca ).
e_utente( ramada,psicologia,csmontalegre ).
e_utente( ramada,oftalmologia,csmontalegre ).
e_utente( ramada,dentista,csmontalegre ).
e_utente( ramada,servicogeral,cspovoa ).
e_utente( ramada,oftalmologia,cspovoa ).
e_utente( lucinda,dentista,csbarca ).
e_utente( lucinda,ortopedia,csbarca ).
e_utente( lucinda,ginecologia,csbraga ).
e_utente( tino,oftalmologia,csbraga ).
e_utente( tino,ortopedia,csbarca ).
e_utente( andre,cirurgia,csplima ).
e_utente( andre,psicologia,csmontalegre ).
e_utente( andre,oftalmologia,cspovoa ).
e_utente( conceicao,ginecologia,csbraga ).
e_utente( conceicao,ginecologia,cspovoa ).
e_utente( conceicao,oftalmologia,csbraga ).
e_utente( manuel,oftalmologia,csbraga ).
e_utente( manuel,dermatologia,csplima ).
e_utente( manuel,cirurgia,csplima ).
e_utente( joana,ginecologia,cspovoa ).
e_utente( joana,dentista,csmontalegre ).
e_utente( joana,ortopedia,csbarca ).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado profissional_servico: Profissional,Servico -> {V,F}

profissional_servico( mario,pediatria ).
profissional_servico( elvira,oftalmologia ).
profissional_servico( ivone,ginecologia ).
profissional_servico( jose,oftalmologia ).
profissional_servico( romario,oftalmologia ).
profissional_servico( joao,dermatologia ).
profissional_servico( luisa,cirurgia ).
profissional_servico( teresa,dentista ).
profissional_servico( jorge,ortopedia ).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado profissional_servico: Profissional,Instituicao -> {V,F}

profissional_instituicao( mario,csbraga ).
profissional_instituicao( mario,csbarca ).
profissional_instituicao( elvira,cspovoa ).
profissional_instituicao( elvira,csplima ).
profissional_instituicao( ivone,csbraga ).
profissional_instituicao( ivone,cspovoa ).
profissional_instituicao( jose,csbraga ).
profissional_instituicao( jose,csplima ).
profissional_instituicao( romario,csbraga ).
profissional_instituicao( joao,csplima ).
profissional_instituicao( luisa,csplima ).
profissional_instituicao( teresa,csmontalegre ).
profissional_instituicao( jorge,csbarca ).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado consultas: Utente,Profissional,Servico,Instituicao -> {V,F}

consultas( zeca,jose,oftalmologia,csbraga ).
consultas( zeca,mario,pediatria,csbarca ).
consultas( chico,elvira,oftalmologia,cspovoa ).
consultas( chico,mario,pediatria,csbraga ).
consultas( ramada,teresa,dentista,csmontalegre ).
consultas( rafa,elvira,oftalmologia,cspovoa ).
consultas( jessica,joao,dermatologia,csplima ).
consultas( jessica,elvira,oftalmologia,csplima ).
consultas( conceicao,ivone,ginecologia,csbraga ).
consultas( lucinda,ivone,ginecologia,csbraga ).
consultas( lucinda,jorge,ortopedia,csbarca ).
consultas( marcos,jorge,ortopedia,csbarca ).
consultas( catarina,teresa,dentista,csmontalegre ).
consultas( joana,teresa,dentista,csmontalegre ).
consultas( andre,luisa,cirurgia,csplima ).
consultas( andre,elvira,oftalmologia,cspovoa ).
consultas( manuel,luisa,cirurgia,csplima ).
consultas( manuel,jose,oftalmologia,csbraga ).
consultas( manuel,joao,dermatologia,csplima ).
consultas( manuel,romario,oftalmologia,csbraga ).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% INVARIANTES

% Invariantes Estruturais:  nao permitir a insercao de conhecimento repetido

+instituicao( I ) :: (findall( I,(instituicao( I )),L ),
                  comprimento( L,N ), 
				  N == 1
                  ). 

+servico( S ) :: (findall( S,(servico( S )),L ),
                  comprimento( L,N ), 
				  N == 1
                  ).

+utente( U ) :: (findall( U,(utente( U )),L ),
                  comprimento( L,N ), 
				  N == 1
                  ).

+profissional( P ) :: (findall( P,(profissional( P )),L ),
                  comprimento( L,N ), 
				  N == 1
                  ).

+e_servico( S,I ) :: (findall( (S,I),(e_servico( S,I )),R ),
                  comprimento( R,N ), 
				  N == 1
                  ).

+e_utente( U,S,I ) :: (findall( (U,S,I),(e_utente( U,S,I )),R ),
                  comprimento( R,N ), 
				  N == 1
                  ).

+profissional_servico( P,S ) :: (findall( (P,S),(profissional_servico( P,S )),R ),
                  comprimento( R,N ), 
				  N == 1
                  ).

+profissional_instituicao( P,I ) :: (findall( (P,I),(profissional_instituicao( P,I )),R ),
                  comprimento( R,N ), 
				  N == 1
                  ).

+consultas( U,P,S,I ) :: (findall( (U,P,S,I),(consultas( U,P,S,I )),R ),
                  comprimento( R,N ), 
				  N == 1
                  ).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Invariantes de Integridade: nao permitir a insercao de dados nao correspondentes com a base de conhecimento

+e_servico( S,I ) :: ( findall( Srv,servico(Srv),L1 ), 
						findall( Ins,instituicao( Ins ),L2 ),
						pertence( S,L1 ),
						pertence( I,L2 ) ).

+e_utente( U,S,I ) :: ( findall( Uts,utente( Uts ),L1 ), 
						findall( Srvs,servico( Srvs ),L2 ),
						findall( Ins,instituicao( Ins ),L3 ),
						pertence( U,L1 ),
						pertence( S,L2 ),
						pertence( I,L3 ) ).

+profissional_servico( P,S ) :: ( findall( Prfs,profissional( Prfs ),L1 ), 
						findall( Srv,servico( Srv ),L2 ),
						pertence( P,L1 ),
						pertence( S,L2 ) ).

+profissional_instituicao( P,I ) :: ( findall( Prfs,profissional( Prfs ),L1 ), 
						findall( Ins,instituicao( Ins ),L2 ),
						pertence( P,L1 ),
						pertence( I,L2 ) ).

+consultas( U,P,S,I ) :: ( findall( Uts,utente( Uts ),L1 ), 
						findall( Prfs,profissional( Prfs ),L2 ),
						findall( Srvs,servico( Srvs ),L3 ),
						findall( Ins,instituicao( Ins ),L4 ),
						findall( (Srvs,Ins),e_servico( Srvs,Ins ),L5 ),
						findall( (Uts,Srvs,Ins),e_utente( Uts,Srvs,Ins ),L6 ),
						findall( (Prfs,Srvs),profissional_servico( Prfs,Srvs ),L7 ),
						findall( (Prfs,Ins),profissional_instituicao( Prfs,Ins ),L8 ),
						pertence( U,L1 ),
						pertence( P,L2 ),
						pertence( S,L3 ),
						pertence( I,L4 ),
						pertence( (S,I),L5 ),
						pertence( (U,S,I),L6 ),
						pertence( (P,S),L7 ),
						pertence( (P,I),L8 ) ).


%---------------------------------
% Invariante: Não permitir a insercao de profissionais em mais do que um servico

+profissional_servico( P,S ) :: (findall( srvs,(profissional_servico( P,Serv )),R ),
                  comprimento( R,N ),
                  N =< 1).

%---------------------------------
% Invariante: Não permitir a insercao de Servicos e Instituicoes que nao estejam no e_servico

+e_utente( U,S,I ) :: ( findall( (Srv,Ins),e_servico( Srv,Ins ),L1 ),
						pertence( (S,I),L1 )).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado servsInst que verifica os servicos de uma dada instituicao
% servsInst: Instituicao, ListaServicos -> {V,F}

servsInst( I,L ) :-
		findall( Srvs,e_servico(Srvs,I),L ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado utentesInst que verifica os utentes de uma dada instituicao
% utentesInst: Instituicao, ListaUtentes -> {V,F}

utentesInst( I,L1 ) :-
		findall( Utentes,e_utente(Utentes,S,I),L ),
		removeDup(L,L1).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado utentesServ que verifica os utentes de um desterminado servico
% utentesServ: Servico, ListaUtentes -> {V,F}

utentesServ( S,L1 ) :-
		findall( Utentes,e_utente(Utentes,S,I),L ),
		removeDup(L,L1).
		
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado utentes_serv_inst que verifica os utentes de um determinado servico numa instituicao
% utentes_serv_inst: Servico, Instuicao, ListaUtente -> {V,F}

utentes_serv_inst( S, I, L ) :- 
		findall(Utentes,e_utente(Utentes,S,I),L).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado servicos que verifica as instituicoes onde seja prestado um dado serviço ou conjunto de serviços 
% servicos: ListaServicos, ListaInstituicao -> {V,F}

servicos(LS, LIns) :-
		findall(R, temServicos(LS, R), L1),removeDup(L1, LIns).

temServicos([], _).
temServicos([H|T], X) :- e_servico(H, X),
						 temServicos(T, X).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado servicosInexistentes que verifica os servicos inexistentes numa dada instituicao
% servicosInexistentes: Instituicao, ListaServicos -> {V,F}

servicosInexistentes(I,L) :-
		findall(S, e_servico(S,I),LServicos),
		findall(Srvs, servico(Srvs), LTodosServicos),
		retiraComuns(LTodosServicos,LServicos,L).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado instProfServico que verifica as instituições onde um profissional presta serviço
% instProfServico: Profissional,ListaInstituicoes -> {V,F}

instProfServico( P,R ) :-
		findall( Ins, profissional_instituicao(P,Ins), R ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado utente_recorreu_insts, utente_recorreu_profi, utente_recorreu_servs que verifica as instituições (ou profissionais, ou serviço ) a que um utente já recorreu;

% utente_recorreu_insts: Utente,ListaInstituicoes -> {V,F}
utente_recorreu_insts(U,LInsts) :- findall(Inst,consultas(U,M,S,Inst),LInsts).

% utente_recorreu_profi: Utente,ListaProfissionais -> {V,F}
utente_recorreu_profi(U,LProfis) :- findall(Profi,consultas(U,Profi,S,Inst),LProfis).

% utente_recorreu_servs: Utente,ListaServicos -> {V,F}
utente_recorreu_servs(U,LServs) :- findall(Serv,consultas(U,M,Serv,Inst),LServs).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado evolucao que permite a evolucao do conhecimento

% evolucao: Termo -> {V,F}
evolucao( Termo ) :-
	findall(I, +Termo::I, Li),
	insiro(Termo),
	verifico(Li).

% insiro: T -> {V,F}
insiro(T) :- assert(T).
insiro(T) :- retract(T), !, fail.

% verifico: L -> {V,F}
verifico([]).
verifico([I|L]) :- I, verifico(L).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado retirar que permite remover conhecimento da base de conhecimento
% retira: Termo -> {V,f}

retira( utente( U ) ):- 	
	retract( utente( U ) ),		
	retiraUtenteInstituicao( U,S,I ),
	retiraConsultas( U,Pro,Se,Ins ).

retira( instituicao( I ) ) :-
		retract( instituicao( I ) ),
		retiraServicoInstituicao( S,I ),
		retiraUtenteInstituicao( U,S,I ),
		retiraProfissionalInstituicao( P,I ),
		retiraConsultas( Ut,Pro,S,I ).

retira( servico( S ) ):- 
	retract( servico( S ) ),
	retiraServicoInstituicao( S,I ),
	retiraUtenteInstituicao( U,S,I ),
	retiraProfissionalServico( P,S ),
	retiraConsultas( Ut,Pro,S,Inst ).

retira( profissional( P ) ) :- 
	retract( profissional( P ) ),
	retiraProfissionalServico( P,S ),
	retiraProfissionalInstituicao( P,I ),
	retiraConsultas( U,P,S,Inst ).

retira( e_servico( S,I ) ):-
	retract( e_servico( S,I ) ),
	retiraUtenteInstituicao( U,S,I ),
	retiraConsultas( Ut,Pro,S,I ).

retira( e_utente( U,S,I ) ):-
		retract( e_utente( U,S,I ) ),
		retiraConsultas( U,Pro,S,I ).

retira( profissional_servico( P,S ) ):-	
		retract( profissional_servico( P,S ) ),
		retiraConsultas( U,P,S,Inst ).

retira( profissional_instituicao( P,I ) ):-
	retract( profissional_instituicao( P,I ) ),
	retiraConsultas( U,P,S,I ).

% Auxiliares para remocao ----------------------

% retiraUtenteInstituicao: Utente, Instituicao -> {V}
retiraUtenteInstituicao(Ut,S,I):-
	findall(e_utente(Ut,S,I),e_utente(Ut,S,I),L),
	retractL(L).
	
% retiraConsultas: Utente, Profissional, Servico, Instituicao -> {V}
retiraConsultas(U,P,S,I) :-
	findall(consultas( U,P,S,I ),consultas( U,P,S,I ),L),
	retractL(L).

% retiraProfissionalInstituicao: Profissional, Instituicao -> {V}
retiraProfissionalInstituicao(P,I):-
	findall(profissional_instituicao( P,I ),profissional_instituicao( P,I),L),
	retractL(L).			

% retiraServicoInstituicao: Servico, Instituicao -> {V}
retiraServicoInstituicao(S,I):-
		findall(e_servico(S,I), e_servico(S,I),L),
	retractL(L).

%retiraProfissionalServico: Profissional, Servico -> {V}
retiraProfissionalServico(P,S):-
		findall(profissional_servico(P,S),
		profissional_servico(P,S),L),
	retractL(L).				


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Funcionalidades extra

% Extensao do predicado utente_vezes_inst que determina quantas vezes um utente recorreu a uma instituicao
% utente_vezes_inst: Utente,Instituicao,Resultado -> {V,F}
utente_vezes_inst( U,I,R ) :- findall(U,consultas(U,P,S,I),L),
							comprimento(L,R).


% Extensao do predicado utente_vezes_serv que determina quantas vezes um utente recorreu a um servico
% utente_vezes_serv: Utente,Servico,Resultado -> {V,F}
utente_vezes_serv( U,S,R ) :- findall(U,consultas(U,P,S,I),L),
							comprimento(L,R).
							
% Extensao do predicado utente_vezes_profi que determina quantas vezes um utente recorreu a um profissional
% utente_vezes_profi: Utente,Profissional,Resultado -> {V,F}
utente_vezes_profi( U,P,R ) :- findall(U,consultas(U,P,S,I),L),
							comprimento(L,R).	

%---------------------------------

% Extensao do predicado rankingConsultas que determinar o ranking de consultas de utentes
% rankingConsultas: Lista -> {V,F}
rankingConsultas( L ) :-
		findall( Ut,consultas(Ut,Prof,Serv,Ins),L1 ),
		contaElementos(L1,L2),
		bubble_sort(L2,L),
		ranking(L).

% ranking: ListaTuplos -> {V,F}
ranking([]).
ranking([(U,N)|T]) :-
		write('Utente: '),write(U),
		write(', Nr consultas: '),write(N),write('\n'),
		ranking(T).								

%---------------------------------

% Extensao do predicado instituicoesMaisFreq que determina o ranking de instituicoes mais frequentadas
% instituicoesMaisFreq: Lista -> {V,F}
instituicoesMaisFreq(R1) :- findall(I, consultas(X,Y,S,I),L),
			 contaElementos(L,R), 
			 bubble_sort(R,R1),
			 ranking(R1).

% Extensao do predicado servicosMaisFreq que determina o ranking de servicos mais solicitados
% servicosMaisFreq: Lista -> {V,F}
servicosMaisFreq(R2) :- 
			findall(S, consultas(X,Y,S,I),L),
			 contaElementos(L,R), 
			 bubble_sort(R,R2),
			 ranking(R2).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -			
% Auxiliares------------------------------

% Extensao do predicado retract de uma lista
% retract: Lista -> {V,F}
retractL([]).
retractL([X|L]):- 
		retract(X), retractL(L).

% Extensao do predicado comprimento que determina comprimentod de uma lista
% comprimento: Lista,Resultado -> {V,F}
comprimento( [],0 ).
comprimento( [X|L],R ) :-
	comprimento( L,N ),
	R is N+1.

% Extensao do predicado pertence que verifica se um dado elemento pertence a uma determinada lista
% pertence: Elemento,Lista -> {V,F}
pertence( X,[X|L] ).
pertence( X,[Y|L] ) :- X \= Y,
					   pertence( X,L ).

% Extensao do meta-predicado nao: Questao -> {V,F}
nao( Questao ) :-
    Questao, !, fail.
nao( Questao ).

% Extensao do predicado retiraComuns que remove os elementos da primeira lista que são comuns à segunda lista
% retiraComuns: Lista,Lista,Lista -> {V,F}
retiraComuns( [], X, []).
retiraComuns( [X|T],L ,[X|T1] ) :- nao(pertence(X,L)),
								   retiraComuns(T,L,T1).
retiraComuns([H|T], L, T1) :- pertence(H, L),
							  retiraComuns(T,L,T1).

% Extensao do predicado removeDup que remove os elementos duplicados de uma determinada lista
% removeDup: Lista,Lista -> {V,F}
removeDup([], []).
removeDup([X|T], R) :- pertence(X,T),
					   removeDup(T, R).
removeDup([X|T], [X|R]) :- nao(pertence(X, T)),
						   removeDup(T, R).	

% Extensao do predicado concatenar que concatena duas listas
% concatenar: Lista,Lista,Lista ->{V,F}
concatenar([],L2,L2).
concatenar([X|L1],L2,[X|L]) :- concatenar(L1,L2,L).

% Extensao do predicado remover que remove determinado elemento de uma lista
% remover: Lista,Elemento,Lista ->{V,F}
remove([],X,[]).
remove([X|L],X,L1) :- remove(L,X,L1).
remove([Y|L],X,[Y|L1]) :- remove(L,X,L1).

% Extensao do predicado conta: Elemento,Lista,NrAparições -> {V,F}
conta(_,[],0). 
conta(X,[X|Y],N) :- conta(X,Y,N1),N is N1+1. 
conta(X,[_|Y],N) :- conta(X,Y,N). 

% Extensao do predicado contar: Lista,ListaTuplos -> {V,F}
contar([],[]). 
contar([X|L1],[(X,N)]) :- conta(X,L1,N1), N is N1+1.

% Extensao do predicado contaElementos: Lista,Lista -> {V,F}
contaElementos([],[]). 
contaElementos( [H|T],R ) :- 
		contar([H|T],L), 
		remove([H|T],H,L2),
		contaElementos(L2,L3),
		concatenar(L,L3,R).

% Extensao do predicado bubble_sort que ordena uma lista
%bubble_sort: Lista,Lista -> {V,F}
bubble_sort(List,Sorted) :-
 		b_sort(List,[],Sorted).

b_sort([],Acc,Acc).
b_sort([H|T],Acc,Sorted) :-
		bubble(H,T,NT,Max),
 		b_sort(NT,[Max|Acc],Sorted).

bubble(X,[],[],X).
bubble((U1,N1),[(U2,N2)|T],[(U2,N2)|NT],Max):-
 		N1 < N2,
 		bubble((U1,N1),T,NT,Max).
bubble((U1,N1),[(U2,N2)|T],[(U1,N1)|NT],Max) :-
 		N1 >= N2,
 		bubble((U2,N2),T,NT,Max).