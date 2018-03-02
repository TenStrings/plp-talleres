%Ejemplo

mapaEjemplo([
	bicisenda(arenales, retiro, 30),
	bicisenda(arenales, libertad, 20),
	bicisenda(retiro, libertad, 10),
	bicisenda(libertad, cabildo, 10)
]).


%cantApariciones(+X, -L, -Z)
cantApariciones( _ , [], 0).
cantApariciones( X, [ X | T ], N ) :- cantApariciones( X, T, Z ), N is 1 + Z.
cantApariciones( X, [ Y | T ], Z ) :- X \= Y, cantApariciones( X, T, Z ).


%listaEstaciones(+M, -L)
estaciones( [], [] ).
estaciones( [ bicisenda( Estacion1, Estacion2, _ ) | RestoMapa ], Res ) :-
	estaciones( RestoMapa, Res ),
	member( Estacion1, Res ),
	member( Estacion2, Res ).
estaciones( [ bicisenda( Estacion1, Estacion2, _ ) | RestoMapa ], [ Estacion1 | Res ] ) :-
	estaciones( RestoMapa, Res ),
	not( member( Estacion1, Res ) ),
	member( Estacion2, Res ).
estaciones( [ bicisenda( Estacion1, Estacion2, _ ) | RestoMapa ], [ Estacion2 | Res ] ) :-
	estaciones( RestoMapa, Res ),
	member( Estacion1, Res ),
	not( member( Estacion2, Res ) ).
estaciones( [ bicisenda( Estacion1, Estacion2, _ ) | RestoMapa ], [ Estacion1, Estacion2 | Res ]) :-
	estaciones( RestoMapa, Res ),
	not( member( Estacion1, Res ) ),
	not( member( Estacion2, Res ) ).

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

%% estacionesVecinas( [], _, [] ).
%% estacionesVecinas( [ bicisenda(Estacion1, Estacion2, _ ) | RestoMapa ], Estacion, Res ) :-
%% 	Estacion1 \= Estacion,
%% 	Estacion2 \= Estacion,
%% 	estacionesVecinas(RestoMapa, Estacion, Res).
%% estacionesVecinas( [ bicisenda(Estacion, Estacion2, _ ) | RestoMapa ], Estacion, [ Estacion2 | Res ] ) :-
%% 	estacionesVecinas( RestoMapa, Estacion, Res ).
%% estacionesVecinas( [ bicisenda(Estacion1, Estacion, _ ) | RestoMapa ], Estacion, [ Estacion1 | Res ] ) :-
%% 	estacionesVecinas( RestoMapa, Estacion, Res ).



%distanciaVecinas(+M, +E1, +E2, -N)
distanciaVecinas(M, E1, E2, N) :- member(bicisenda(E1, E2, N), M).
distanciaVecinas(M, E1, E2, N) :- member(bicisenda(E2, E1, N), M).


% caminoSimpleConOrigen(+M, +O, +OAux, +D, ?C)
caminoSimpleConOrigen(_, _, D, D, [D]).
caminoSimpleConOrigen(M, O, Aux, D, [ Aux , S | T ]) :-
	Aux \= O,
	esVecina(M,Aux,S),
	caminoSimpleConOrigen(M, O, S, D, [S | T]).
caminoSimpleConOrigen(M, O, O, D, [ O , S | T ]) :-
	esVecina(M,O,S),
	caminoSimpleConOrigen(M, O, S, D, [S | T]).


% caminoSimple(+M, +O, +D, ?C)
caminoSimple(M, O, D, C) :- caminoSimpleConOrigen(M, O, O, D, C).
%% caminoSimple(M, O, D, [ O, S | T ]) :-
%% 	not(member(O, T)),
%% 	esVecina(M,O,S),
%% 	caminoSimple(M, S, D, [S | T]).