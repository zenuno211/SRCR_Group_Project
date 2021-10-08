%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% PROLOG: Declaracoes iniciais

:- set_prolog_flag( discontiguous_warnings,off ).
:- set_prolog_flag( single_var_warnings,off ).
:- set_prolog_flag( unknown,fail ).

:-dynamic utente/10.
:-dynamic centroSaude/5.
:-dynamic staff/4.
:-dynamic vacinacao/5.
:-dynamic criterio/4.
:-dynamic '-'/1.
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% PROLOG: definicoes iniciais

:- op( 900,xfy,'::' ).

% Extensao do predicado solucoes
solucoes(X,Y,Z):- findall(X,Y,Z).

% Extensao do predicado que permite a evolucao do conhecimento
evolucao( Termo ) :-
    solucoes( Invariante, +Termo::Invariante,Lista ),insercao( Termo ),teste( Lista ).

insercao( Termo ) :-assert( Termo ).
insercao( Termo ) :-retract( Termo ), !,fail.

teste( [] ).
teste( [R|LR] ) :- R,teste( LR ).

% Extensao do predicado que permite a involucao do conhecimento
involucao( Termo ) :-
    solucoes( Invariante,-Termo::Invariante,Lista ), remove( Termo ), teste( Lista ).

remove(Termo) :- retract(Termo).
remove(Termo) :- assert(Termo),!,fail.

nao( Questao ) :-Questao, !, fail.
nao(_).

member1(D,[D|_]).
member1(D,[_|T]):-member1(D,T).

%qualquer 1 elemento da 1ª lista pertence à 2ª lista
memberList([H|_],[H|_]).
memberList([H|T],[X|XS]):- member1(H,[X|XS]); memberList(T,[X|XS]).

subtrair([], _, []).
subtrair([Head|Tail], L2, L3) :-
        member1(Head, L2),
        !,
        subtrair(Tail, L2, L3).
subtrair([Head|Tail1], L2, [Head|Tail3]) :-
        subtrair(Tail1, L2, Tail3).

eliminaRepetidos(X, R) :- eliminaRepAux(X,[],R), !.

eliminaRepAux([],Acc,Acc).
eliminaRepAux([X|XS],Acc,R) :- member1(X,Acc), eliminaRepAux(XS,Acc,R).
eliminaRepAux([X|XS],Acc,R) :- eliminaRepAux(XS,[X|Acc],R).

%Base de Conhecimento

utente(1,12345678901,'Rui',data(15,03,1969),'rui@gmail.com',914972655,'Guimaraes','Cantor',['Diabetes','Insuficiencia Renal'],1).
utente(2,12345678902,'Manuel',data(15,03,1971),'batshecks@gmail.com',91497261,'Regua','Enfermeiro',['Insuficiencia Cardiaca'],2).
utente(3,12345678903,'Luisa',data(25,04,1978),'luisa@gmail.com',914972656,'Braga','Medico',['Asma','Reumatismo'],2).
utente(4,12345678904,'Fernando',data(30,05,2000),'fernando@gmail.com',914972657,'Porto','Estudante',[],1).
utente(5,12345678905,'Marta',data(30,05,1950),'marta@gmail.com',914972658,'Guimaraes','Reformado',['Conjuntivite'],1).
utente(6,12345678906,'Pedro',data(3,03,2004),'pedro@gmail.com',914972659,'Lisboa','Estudante',['DPOC'],1).
utente(7,12345678907,'Jose',data(1,03,2005),'jose@gmail.com',914972662,'Braga','Estudante',[],2).
utente(8,12345678908,'Goncalo',data(15,03,1941),'goncalo@gmail.com',914972663,'Barcelos','Reformado',['Hipertensao Arterial'],2).
utente(9,12345678909,'Maria',data(15,03,1956),'maria@gmail.com',914972664,'Guimaraes','Militar',[],1).
utente(10,12345678910,'Sofia',data(15,03,1958),'sofia@gmail.com',914972665,'Guimaraes','Reformado',['Obesidade'],1).
utente(11,12345678911,'Nuno',data(15,03,1957),'nuno@gmail.com',914972666,'Braga','Pescador',[],2).
utente(12,12345678912,'Joaquim',data(15,03,1959),'joaquim@gmail.com',914972667,'Alentejo','Pastor',['Neoplasia Maligna Ativa'],2).
utente(13,123456789123,'António',data(05,07,1965),'antonio@gmail.com',914972668,'Alentejo','Agricultor',['Neoplasia Maligna Ativa'],2).


centroSaude(1,'Centro de Saude de Guimaraes','Guimaraes',253519923,'centroSaudeGuimaraes@gmail.com').
centroSaude(2,'Centro de Saude de Braga','Braga',253209230,'centroSaudeBraga@gmail.com').

staff(1,1,'Mel','mel@gmail.com').
staff(2,1,'Nina','nina@gmail.com').
staff(3,2,'Francisca','francisca@gmail.com').
staff(4,2,'Alberta','alberta@gmail.com').

vacinacao(1,1,'10-03-2021','Pfizer',1).
vacinacao(1,1,'07-04-2021','Pfizer',2).
vacinacao(3,2,'15-03-2021','Pfizer',1).
vacinacao(3,2,'08-03-2021','Pfizer',2).
vacinacao(4,3,'10-03-2021','AstraZeneca',1).
vacinacao(4,3,'07-04-2021','AstraZeneca',2).
vacinacao(1,4,'17-03-2021','AstraZeneca',1).
vacinacao(2,10,'10-03-2021','Pfizer',1).
vacinacao(2,10,'07-04-2021','Pfizer',2).
vacinacao(3,7,'10-03-2021','Pfizer',1).
vacinacao(1,9,'10-03-2021','Pfizer',1).
vacinacao(1,9,'07-04-2021','Pfizer',2).
vacinacao(4,8,'10-03-2021','AstraZeneca',1).
vacinacao(4,8,'07-04-2021','AstraZeneca',2).

criterio(1, data(01,01,1971),['Insuficiencia Cardiaca', 'Insuficiencia Renal', 'Doenca Coronaria', 'DPOC'], []).
criterio(1, data(01,01,2021),[], ['Medico','Bombeiro','Militar']).
criterio(2, data(01,01,1971),['Diabetes','Neoplasia Maligna Ativa','Doenca Renal Cronica','Insuficiencia Hepatica','Hipertensao Arterial','Obesidade'], []).


utenteID(Id, R) :- solucoes(utente(Id,NS,N,D,E,T,M,P,DC,C), utente(Id,NS,N,D,E,T,M,P,DC,C), R).

faseTerminada(1) :- solucoes(ID, (vacinacao(_,ID,_,_,1), vacinacao(_,ID,_,_,2), utenteFV(ID,1)), LV), fase1Vacinacao(L1), subtrair(L1, LV, R), length(R, 0).
faseTerminada(2) :- solucoes(ID, (vacinacao(_,ID,_,_,1), vacinacao(_,ID,_,_,2), utenteFV(ID,2)), LV), fase2Vacinacao(L1), subtrair(L1, LV, R), length(R, 0).
faseTerminada(3) :- solucoes(ID, (vacinacao(_,ID,_,_,1), vacinacao(_,ID,_,_,2), utenteFV(ID,3)), LV), fase3Vacinacao(L1), subtrair(L1, LV, R), length(R, 0).

vacinacaoIndevida(R) :- nao(faseTerminada(1)), solucoes(ID, (vacinacao(_,ID,_,_,_), (utenteFV(ID,2);utenteFV(ID,3))), R1), eliminaRepetidos(R1, R).
vacinacaoIndevida(R) :- faseTerminada(1), nao(faseTerminada(2)), solucoes(ID, (vacinacao(_,ID,_,_,_), utenteFV(ID,3)), R1), eliminaRepetidos(R1, R).
vacinacaoIndevida([]).

candidatosVac(R) :- nao(faseTerminada(1)), solucoes(ID, (utenteNV(ID), utenteFV(ID,1)), L), eliminaRepetidos(L,R).
candidatosVac(R) :- faseTerminada(1), nao(faseTerminada(2)), solucoes(ID, (utenteNV(ID), utenteFV(ID,2)), L), eliminaRepetidos(L,R).
candidatosVac(R) :- faseTerminada(2), solucoes(ID, (utenteNV(ID), utenteFV(ID,3)), L), eliminaRepetidos(L,R).

nrUtentesReg(R):-solucoes(ID,utente(ID,_,_,_,_,_,_,_,_,_),L),length(L,R).
nrUtentesVacinados(R):-solucoes(utente(ID,_,_,_,_,_,_,_,_,_),vacinacao(_,ID,_,_,_),L1), eliminaRepetidos(L1, L2), length(L2,R).
nrVacinacoes(R):-solucoes(ID,vacinacao(_,ID,_,_,_),L2), length(L2,R).

%utente/utentes com alguma vacinação registada
utenteV(Id):-vacinacao(_,Id,_,_,_), !.
utentesV(R):- solucoes(X,vacinacao(_,X,_,_,_), R1), eliminaRepetidos(R1, R).

%utente/utentes com 2 tomas registadas
utenteV2(Id):-vacinacao(_,Id,_,_,1), vacinacao(_,Id,_,_,2).
utentesV2(R):- solucoes(X,(vacinacao(_,X,_,_,1),vacinacao(_,X,_,_,2)), R1), eliminaRepetidos(R1, R).

%utente/utentes sem nenhuma vacinação registada
utenteNV(Id) :- utentesNV(R), member1(Id, R).
utentesNV(R):- solucoes(Id,utente(Id,_,_,_,_,_,_,_,_,_),L),solucoes(X,vacinacao(_,X,_,_,_),LI), subtrair(L,LI,R).

%utente/utentes sem 2 tomas registadas
utenteNV2(Id) :- utentesNV2(R), member1(Id, R).
utentesNV2(R):- solucoes(Id,utente(Id,_,_,_,_,_,_,_,_,_),L), solucoes(X,vacinacao(_,X,_,_,2),LI), subtrair(L,LI,R).

%utentes a quem falta a segunda toma da vacina
segundaToma(R):-solucoes(X,vacinacao(_,X,_,_,1),L1), solucoes(X2,vacinacao(_,X2,_,_,2),L2), subtrair(L1,L2,R).

%relaciona um utente com a sua fase de vacinação
utenteFV(Id,1):- cumpreCriterios(Id, 1).
utenteFV(Id,2):- cumpreCriterios(Id, 2), nao(utenteFV(Id,1)).
utenteFV(Id,3):- solucoes(I, (utente(I,_,_,_,_,_,_,_,_,_), nao(utenteFV(I,2)), nao(utenteFV(I,1))), L), member1(Id, L).


dataA(data(_,_,A),A).
dataM(data(_,M,_),M).
dataD(data(D,_,_),D).

%utentes da 1ª fase de vacinação
fase1Vacinacao(R):-solucoes(Id, cumpreCriterios(Id, 1), L), eliminaRepetidos(L,R).
%utentes da 1ª fase de vacinação
fase2Vacinacao(R):- solucoes(Id, (cumpreCriterios(Id, 2), nao(cumpreCriterios(Id, 1))), L), eliminaRepetidos(L,R).
%utentes da 1ª e da 2ª fase de vacinação
fase12Vacinacao(R):- solucoes(Id, (cumpreCriterios(Id, 2); cumpreCriterios(Id, 1)), L), eliminaRepetidos(L,R).
%utentes da 3ª fase de vacinação
fase3Vacinacao(R):-solucoes(Id, (utente(Id,_,_,_,_,_,_,_,_,_)), L), eliminaRepetidos(L,L2), fase12Vacinacao(L12), subtrair(L2,L12, R).

%Critérios
regCriterio(Fase, data(D, M,A), Doenca, Profissao) :-
    evolucao(criterio(Fase, data(D, M, A), Doenca, Profissao)).
removeCriterio(Fase, data(D,M,A), Doenca, Profissao) :-
    involucao(criterio(Fase, data(D, M, A), Doenca, Profissao)).

%mostra lista de todos os critérios
mostrarCriterios(S) :-
    solucoes((F,Data, Doenca, Profissao), criterio(F, Data, Doenca, Profissao), S).

cumpreCriterios(Id, F) :-
    utente(Id,_,_,D,_,_,_,P,DC,_),
    criterio(F, data(_, _, AnoC), Doenca, Profissao),
    dataA(D,A),A=<AnoC, (memberList(DC, Doenca) ; member1(P, Profissao)).



%Insercão de conhecimento
regUtente(Id,Ns,N,D,E,T,M,P,DC,C):- evolucao(utente(Id,Ns,N,D,E,T,M,P,DC,C)).
regVacinacao(IDS,IDU,DN,NV,NT):- evolucao(vacinacao(IDS,IDU,DN,NV,NT)).
regStaff(IDS,IDCS,N,M):-evolucao(staff(IDS,IDCS,N,M)).
regCentroSaude(ID,N,L,Nu,M):-evolucao(centroSaude(ID,N,L,Nu,M)).

%Invariantes

%invariante que verifica que nao se repete os ids dos utentes
+utente(Id,_,_,_,_,_,_,_,_,_)::(solucoes((Id),utente(Id,_,_,_,_,_,_,_,_,_),R),length(R,1)).
%invariante que verifica que nao se repete os numeros de segurança social  dos utentes
+utente(_,Ns,_,_,_,_,_,_,_,_)::(solucoes(Ns,utente(_,Ns,_,_,_,_,_,_,_,_),R), length(R,1)).
%garante que mes valido numa data
+utente(_,_,_,D,_,_,_,_,_,_)::(dataM(D,R),R>0,R<13).
%garante que o centro de saude está registado
+utente(_,_,_,_,_,_,_,_,_,CS)::centroSaude(CS,_,_,_,_).
%invariante que verifica que nao se repete informacao de staff
+staff(Id,_,_,_)::(solucoes(Id,staff(Id,_,_,_),R),length(R,1)).
% invariante que verifica que nao se repete informacao de centro de saude
+centroSaude(Id,_,_,_,_)::(solucoes(Id,centroSaude(Id,_,_,_,_),R),length(R,1)).
%invariante que verifica que nao se repete informacao de vacinacao
+vacinacao(_,IDU,_,_,NT)::(solucoes((IDU,NT),vacinacao(_,IDU,_,_,NT),R),length(R,1)).
%garante que numero de toma valido
+vacinacao(_,_,_,_,NT)::(NT>0,NT<3).
%garante que toma das vacinas é feita por ordem e com a mesma vacina
+vacinacao(_,IDU,_,NV,2)::vacinacao(_,IDU,_,NV,1).
%garante que o utente é vacinado no seu centro de saude(staff é do mesmo centro de saude)
+vacinacao(IDS,IDU,_,_,_)::(solucoes((CS),(utente(IDU,_,_,_,_,_,_,_,_,CS),staff(IDS,CS,_,_)),R),length(R,1)).

%Remoção de conhecimento
%garante que um utente que já foi vacinado não pode ser removido
-utente(Id,_,_,_,_,_,_,_,_,_)::(solucoes(Id, vacinacao(_,Id,_,_,_), L), length(L,0)).
%garante que a primeira toma não é removida se existir uma segunda toma registada
-vacinacao(_,IDU,_,_,1)::nao(vacinacao(_,IDU,_,_,2)).
%garante que um staff não é removido se houver vacinacoes realizadas por ele
-staff(IDS,_,_,_)::(solucoes(IDS, vacinacao(IDS,_,_,_,_), L), length(L,0)).
%garante que um centro saude não é removido se houver staff ou utentes desse centro de saude
-centroSaude(CS,_,_,_,_)::(solucoes(Id,(staff(Id,CS,_,_);utente(Id,_,_,_,_,_,_,_,_,CS)), L), length(L,0)).

removeUtente(Id):- utente(Id,Ns,N,D,E,T,M,P,DC,C), involucao(utente(Id,Ns,N,D,E,T,M,P,DC,C)).
removeVacinacao(IDU,NT):- vacinacao(IDS,IDU,DN,NV,NT), involucao(vacinacao(IDS,IDU,DN,NV,NT)).
removeStaff(IDS):-staff(IDS,ICS,Nome,Email), involucao(staff(IDS,ICS,Nome,Email)).
removeCentroSaude(ID):- centroSaude(ID,N,L,Nu,M), involucao(centroSaude(ID,N,L,Nu,M)).

si(Questao, verdadeiro) :- Questao.
si(Questao, falso) :- nao(Questao).

siLista([],[]).
siLista([Questao|Qs],[R|Rs]):- si(Questao,R),siLista(Qs,Rs).