%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Ejemplos
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mapaHamiltoniano([
	bicisenda(arenales, retiro, 30),
	bicisenda(arenales, libertad, 20),
	bicisenda(retiro, libertad, 10)
]).

mapaNoHamiltoniano([
	bicisenda(arenales, retiro, 30),
	bicisenda(arenales, libertad, 20),
	bicisenda(retiro, libertad, 10),
	bicisenda(libertad, cabildo, 10)
]).


mapaNoConexo([
	bicisenda(arenales, retiro, 30),
	bicisenda(arenales, libertad, 20),
	bicisenda(retiro, libertad, 10),
	bicisenda(mendoza, colegiales, 15)
]).

mapaConSelfLoops([
	bicisenda(arenales, arenales, 30),
	bicisenda(arenales, libertad, 20),
	bicisenda(retiro, libertad, 10),
	bicisenda(libertad, cabildo, 10)
]).

mapaConCiclosTriviales([
	bicisendas(arenales, retiro, 30),
	bicisendas(retiro, arenales, 25)
]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Ejercicio 1
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%sinElem(+XS, +X, -YS)
sinElem([], _, []).
sinElem([X | XS ], X, YS) :- sinElem(XS, X, YS).
sinElem([Y | XS ], X, [Y | YS]) :- 
	X \= Y,
	sinElem(XS, X, YS).

%todosDistintos(+XS, -YS).
sinRepetidos([], []).
sinRepetidos( [ X | XS ] , [ X | Res ] ) :- 
 	sinElem(XS, X, ZS),
 	sinRepetidos(ZS, Res).

% estacionesALoBruto(M,L) es true si L es la lista en la que una estación 
% aparece una vez por cada eje de M incidente.
%estacionesALoBruto(+M, -L)
estacionesALoBruto([],[]).
estacionesALoBruto( [ bicisenda( Estacion1, Estacion2, _ ) | RestoMapa ], [ Estacion1, Estacion2 | Res ] ) :-
	estacionesALoBruto( RestoMapa, Res ).

%estaciones(+M, -Es)
estaciones(Mapa, Res) :- estacionesALoBruto(Mapa, Lista), sinRepetidos(Lista, Res).

%Consultas de test:
%
%		mapaNoHamiltoniano(M),estaciones(M,ES).
%
%		ES = [arenales, retiro, libertad, cabildo];
%		false.
%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Ejercicio 2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%estacionesVecinas(+M, +E, -Es)
esVecina(M, E, E1) :-  member(bicisenda(E, E1, _), M).
esVecina(M, E, E1) :-  member(bicisenda(E1, E, _), M).

%todosVecinos(+M, +E, +Es, -V)
todosVecinos( _, _, [], []).
todosVecinos(M, E, [E1 | T ], [ E1 | Res ] ) :- 
	esVecina(M, E, E1),
	todosVecinos(M, E, T, Res).
todosVecinos(M, E, [E1 | T ],   Res  ) :- 
	not(esVecina(M, E, E1)),
	todosVecinos(M, E, T, Res).

%estacionesVecinas(+M, +E, -Res)
estacionesVecinas(M, E, Res) :- 
	estaciones(M, K),
	todosVecinos(M, E, K, Res).

%Consultas de test:
%
%		mapaNoHamiltoniano(M),estacionesVecinas(M,retiro,ES).
%
%		ES = [arenales, libertad];
%		false.
%
%		mapaNoHamiltoniano(M),estacionesVecinas(M,libertad,ES).
%
%		ES = [arenales, retiro, cabildo];
%		false.
%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Ejercicio 3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%distanciaVecinas(+M, +E1, +E2, -N)
distanciaVecinas(M, E1, E2, N) :- member(bicisenda(E1, E2, N), M).
distanciaVecinas(M, E1, E2, N) :- member(bicisenda(E2, E1, N), M).

%Consultas de test:
%
%		mapaNoHamiltoniano(M),distanciaVecinas(M,arenales,retiro,N).
%
%		N = 40;
%		false.
%
%		mapaNoHamiltoniano(M),distanciaVecinas(M,arenales,cabildo,ES).
%
%		false.
%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Ejercicio 4  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%caminoSimpleUsando(+M,+O,+D, +ES, ?C)
% M = Es el mapa del que queremos sacar el camino.
% O = Es el primer nodo del camino.
% D = Es el último nodo del camino.
% ES = Es la lista de estaciones que puede tener C.
% C = Es un camino entre O y D.

%% En cada paso de la inferencia usamos un ES que contiene a todos los nodos que todavia no fueron usados en C.
%% Por eso, cuando hacemos el caso recursivo nos aseguramos que la cola del camino no contenga a O.

caminoSimpleUsando(_, D, D, ES, [D]) :-
	member(D, ES).
caminoSimpleUsando(M, O, D, ES, [O, X | C]) :-
	esVecina(M, O, X),
	member(O, ES),
	sinElem(ES, O, ES1),
	caminoSimpleUsando(M, X, D, ES1, [X | C]).

%caminoSimple(+M, +O, +D, ?C)
caminoSimple(M, O, D, C) :- 
	estaciones(M, ES),
	caminoSimpleUsando(M,O,D,ES,C).


%Consultas de test:
%
%		mapaNoHamiltoniano(M),caminoSimple(M,arenales,cabildo,C).
%
%		C = [arenales, retiro, libertad, cabildo] ;
% 		C = [arenales, libertad, cabildo] ;
% 		false.
%
%		mapaNoHamiltoniano(M),caminoSimple(M,zabala,cabildo,ES).
% 	
%		false.
%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Ejercicio 5
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% a)

%todasEstanConectadas(+M, +ES)
tieneCamino(_, _, []).
tieneCamino(M, E, [ X | ES ]) :-
	caminoSimple(M, E, X, _),!,
	tieneCamino(M, E, ES).


%esConexo(+M)
esConexo(M) :-
	estaciones(M, ES),
	forall(member(E, ES), tieneCamino(M, E, ES)).

% b)

%noTieneSelfLoops(+M)
noTieneSelfLoops(M) :-
	estaciones(M, ES),
	forall(member(E, ES), not(esVecina(M, E,E))).


% c)
%dejanDeSerVecinosAlSacar(+M, +B)
dejanDeSerVecinosAlSacar(M, bicisenda(E1, E2, _)) :-
	select(bicisenda(E1, E2, _), M, L),!,
	not(esVecina(L, E1, E2)).

%noTieneCiclosTriviales(+M)
noTieneCiclosTriviales(M) :-
	forall(member(B, M), dejanDeSerVecinosAlSacar(M, B)).

%mapaVálido(+Bs)
mapaValido(M) :- 
	esConexo(M),
	noTieneSelfLoops(M),
	noTieneCiclosTriviales(M).


%Consultas de test:
%
%		mapaNoHamiltoniano(M),mapaValido(M).
%		true.
%
%		mapaNoConexo(M),mapaValido(M).
% 	
%		false.
%
%
%		mapaConSelfLoops(M),mapaValido(M).
% 	
%		false.
%
%		mapaConCiclosTriviales(M),mapaValido(M).
% 	
%		false.
%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Ejercicio 6
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%caminoHamiltoniano(+M, +O, +D, ?C)
caminoHamiltoniano(M, O, D, C) :-
	estaciones(M, ES),
	caminoSimple(M,O,D,C),
	length(C, N),
	length(ES, N).

%Consultas de test:
%
%		mapaHamiltoniano(M),caminoHamiltoniano(M,libertar,arenales,ES).
%
%		ES = [libertad, retiro, arenales] ;
%		false.
%
%		mapaNoHamiltoniano(M),caminoHamiltoniano(M,libertad,cabildo,C).
% 	
%		false.
%




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Ejercicio 7
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%caminosHamiltonianos(+M, ?C)
caminosHamiltonianos(M, C) :-
	estaciones(M, ES),
	member(E1, ES),
	member(E2, ES),
	E1 \= E2,
	caminoHamiltoniano(M, E1, E2, C).


%Consultas de test:
%
%		mapaHamiltoniano(M),caminosHamiltonianos(M, C).
%
%		C = [arenales, libertad, retiro] ;
%		C = [arenales, retiro, libertad] ;
%		C = [retiro, libertad, arenales] ;
%		C = [retiro, arenales, libertad] ;
%		C = [libertad, retiro, arenales] ;
%		C = [libertad, arenales, retiro] ;
%		false.
%
%		mapaNoHamiltoniano(M),caminosHamiltonianos(M ,C).
%
%		C = [arenales, retiro, libertad, cabildo] ;
%		C = [retiro, arenales, libertad, cabildo] ;
%		C = [cabildo, libertad, retiro, arenales] ;
%		C = [cabildo, libertad, arenales, retiro] ;
%		false.
%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Ejercicio 8
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%pesoCamino(+M, +C, ?N)
pesoCamino(_, [_], 0).
pesoCamino(M, [ O, D | C ], N) :-
	pesoCamino(M,[D | C], N1),
	distanciaVecinas(M, O, D, P),
	N is P + N1.

%Consultas de test:
%
%		mapaNoHamiltoniano(M),caminoSimple(M,retiro,cabildo, C),pesoCamino(M, C, N).
%
%		N = 20 ;
%		N = 60 ;

%
%caminoMinimo(+M, +O, +D, ?C, ?N)
caminoMinimo(M, O, D, C, N) :-
	caminoSimple(M, O, D, C),
	pesoCamino(M, C, N),
	forall(caminoSimple(M,O,D,C1), (pesoCamino(M, C1, N1), N =< N1)).
	
%Consultas de test:
%
%		mapaNoHamiltoniano(M),caminoMinimo(M,retiro,cabildo, C,N).
%
%		C = [retiro, libertad, cabildo],
%		N = 20 ;



