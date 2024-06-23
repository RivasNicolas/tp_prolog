%%%%%%%%%%%%%%%%%%%%%%%%
%% Tablero
%%%%%%%%%%%%%%%%%%%%%%%%

%% Ejercicio 1
%% tablero(+Filas,+Columnas,-Tablero) instancia una estructura de tablero en blanco
%% de Filas x Columnas, con todas las celdas libres.
%%FA = filaActualizada
tablero(0,_,[]).
tablero(Filas,Columnas,[Lista|Tablero]) :- Filas > 0, FilasActualizadas is Filas - 1, length(Lista, Columnas), tablero(FilasActualizadas,Columnas,Tablero).

%% listaNelementos(+Largo, -Lista) instancia una lista de N elementos.
%%listaNelementos(0,[]).
%%listaNelementos(N, [_|XS]) :- N > 0, N1 is N - 1, listaNelementos(N1, XS).

%% Ejercicio 2
%% ocupar(+Pos,?Tablero) será verdadero cuando la posición indicada esté ocupada.
%%FM = filaMatriz
%%FSM = filasMatriz
%%CSM = columnasMatriz
%%CM = columnaMatriz
%%F = cantidadFilas
%%C = cantidadColumnas
ocupar(pos(0,0), T) :- nonvar(T), T = [[ocupada|_]|_].
ocupar(pos(0, C), [[_|CSM]|FSM]) :- C > 0, C1 is C - 1, ocupar(pos(0, C1), [CSM|FSM]). %%Es necesario pedir nonvar(C)? Creo que no
ocupar(pos(F, C), [_|FSM]) :- F > 0, F1 is F - 1, ocupar(pos(F1, C), FSM).             %%Es necesario pedir ground(pos(F, C))? Creo que no
ocupar(pos(Fila, Columna), Tablero) :- var(Tablero), ground(pos(Fila, Columna)),CasillasMin is (Fila + 1) * (Columna + 1),desde(CasillasMin, Casillas), 
                                        tablero2(Casillas, Tablero, Filas, Columnas), Filas > Fila, Columnas > Columna, ocupar(pos(Fila, Columna), Tablero).
                                        %%Es necesario pedir ground(pos(Fila, Columna))? Creo que no

%%Generación infinita de tableros.
tablero2(Casillas, Tablero, Filas, Columnas) :- between(1, Casillas, Filas), Columnas is Casillas / Filas, Casillas is Filas * Columnas, 
                                                tablero(Filas, Columnas, Tablero).

tablero(ej5x5, T) :- tablero(5, 5, T), ocupar(pos(1, 1), T), ocupar(pos(1, 2), T).
tablero(ej4x4, T) :- tablero(4, 4, T), ocupar(pos(1, 1), T), ocupar(pos(1, 2), T).
tablero(ej3x3, T) :- tablero(3, 3, T), ocupar(pos(1, 1), T), ocupar(pos(1, 2), T).
tablero(ej2x2, T) :- tablero(2, 2, T).

%% Ejercicio 3
%% vecino(+Pos, +Tablero, -PosVecino) será verdadero cuando PosVecino sea
%% un átomo de la forma pos(F', C') y pos(F',C') sea una celda contigua a
%% pos(F,C), donde Pos=pos(F,C). Las celdas contiguas puede ser a lo sumo cuatro
%% dado que el robot se moverá en forma ortogonal.
%% cantColumnas(+Tabler, -Columnas)
cantColumnas([X|_],C) :- length(X,C).

%% posicionCorrecta(+Pos, +Tablero)
posicionCorrecta(pos(F, C), T) :- length(T,TF), F >= 0, F < TF, cantColumnas(T, TC), C < TC, C >= 0.

vecino(pos(F,C),T,pos(F2,C)):- posicionCorrecta(pos(F, C), T), F2 is F + 1, posicionCorrecta(pos(F2, C), T).
vecino(pos(F,C),T,pos(F2,C)):- posicionCorrecta(pos(F, C), T), F2 is F - 1, posicionCorrecta(pos(F2, C), T).
vecino(pos(F,C),T,pos(F,C2)):- posicionCorrecta(pos(F, C), T), C2 is C + 1, posicionCorrecta(pos(F, C2), T).
vecino(pos(F,C),T,pos(F,C2)):- posicionCorrecta(pos(F, C), T), C2 is C - 1, posicionCorrecta(pos(F, C2), T).

%% Ejercicio 4
%% vecinoLibre(+Pos, +Tablero, -PosVecino) idem vecino/3 pero además PosVecino
%% debe ser una celda transitable (no ocupada) en el Tablero
%% estaOcupada(+Pos, +Tablero)
estaOcupada(pos(0,0),[[X|_]|_]) :- nonvar(X), X = ocupada.
estaOcupada(pos(0,C),[[_|CSM]|FSM]) :- C > 0, C1 is C-1, estaOcupada(pos(0,C1),[CSM|FSM]).
estaOcupada(pos(F,C),[_|FSM]) :- F > 0,F1 is F-1, estaOcupada(pos(F1,C),FSM).

vecinoLibre(Pos,Tablero,PosVecino) :- vecino(Pos,Tablero,PosVecino), not(estaOcupada(PosVecino, Tablero)).
%% Utiliza la técnica de Generate&Test. Se generan todos los vecinos, para luego testear cuáles están libres.

%%%%%%%%%%%%%%%%%%%%%%%%
%% Definicion de caminos
%%%%%%%%%%%%%%%%%%%%%%%%

%% Ejercicio 5
%% camino(+Inicio, +Fin, +Tablero, -Camino) será verdadero cuando Camino sea una lista
%% [pos(F1,C1), pos(F2,C2),..., pos(Fn,Cn)] que denoten un camino desde Inicio
%% hasta Fin pasando solo por celdas transitables.
%% Además se espera que Camino no contenga ciclos.
%% Notar que la cantidad de caminos es finita y por ende se tiene que poder recorrer
%% todas las alternativas eventualmente.
%% Consejo: Utilizar una lista auxiliar con las posiciones visitadas
camino(I, F, T, L) :- caminoAux(I, F, T, L, []).

%%caminoAux(+Inicio, +Fin, +Tablero, -Camino, +Visitados)
caminoAux(F, F, T, L, LAUX) :- posicionCorrecta(F, T), not(estaOcupada(F,T)), append(LAUX, [F], L).
caminoAux(I, F, T, L, LAUX) :- I \= F, posicionCorrecta(F, T), not(estaOcupada(F,T)), posicionCorrecta(I, T), not(estaOcupada(I,T)), vecinoLibre(I, T, V), 
                  not(member(V, LAUX)), append(LAUX, [I], LAUX1), caminoAux(V, F, T, L, LAUX1).

%% 5.1. Analizar la reversibilidad de los parámetros Fin y Camino justificando adecuadamente en cada
%% caso por qué el predicado se comporta como lo hace

%%Análisis de reversibilidad:
%% Fin no es reversible. Esto se debe a que en nuestro predicado estamos usando I\=F, al tratarse de una operación aritmética tanto las variables de Inicio como de Fin deben estar instanciadas.
%% Camino es reversible y al pasarle este instanciado al predicado camino lo que va a hacer es devolver True si el camino es un camino valido en ese tablero

%%ERROR RARO QUE VIMOS CON MATE: CUANDO HACEMOS LA CONSULTA CON TABLERO DEL CASO DE TEST NOS DA FALSE, PERO CUANDO HARDCODEAMOS EN LA CONSULTA EL TABLERO NOS DA TRUE COMO ESTAMOS ESPERANDO.


%% Ejercicio 6
%% camino2(+Inicio, +Fin, +Tablero, -Camino) ídem camino/4 pero que las soluciones
%% se instancien en orden creciente de longitud.
desde(X, X).
desde(X, Y) :- N is X + 1, desde(N, Y).

camino2(I,F,T,C) :- cantColumnas(T, NC), length(T, NF), MAX is NC * NF, between(1, MAX, N), camino(I, F, T, C), length(C, N).

%% 6.1. Analizar la reversibilidad de los parámetros Inicio y Camino justificando adecuadamente en
%% cada caso por qué el predicado se comporta como lo hace.
%%Inicio

%% Ejercicio 7
%% caminoOptimo(+Inicio, +Fin, +Tablero, -Camino) será verdadero cuando Camino sea un
%% camino óptimo sobre Tablero entre Inicio y Fin. Notar que puede no ser único.
caminoOptimo(I,F,T,C) :- camino(I, F, T, C), not((camino(I, F, T, C1), length(C, L), length(C1, L1), L1 < L)).
%% Utiliza la técnica de Generate&Test. Se generan los caminos del tablero, para luego testear cuáles cumplen con la propiedad de ser óptimos.

%%%%%%%%%%%%%%%%%%%%%%%%
%% Tableros simultáneos
%%%%%%%%%%%%%%%%%%%%%%%%

%% Ejercicio 8
%% caminoDual(+Inicio, +Fin, +Tablero1, +Tablero2, -Camino) será verdadero
%% cuando Camino sea un camino desde Inicio hasta Fin pasando al mismo tiempo
%% sólo por celdas transitables de ambos tableros.
caminoDual(I,F,T1,T2,C) :- camino(I, F, T1, C), camino(I, F, T2, C).
%% Utiliza la técnica de Generate&Test. Se generan los caminos del tablero 1, para luego testear cuáles son caminos del tablero 2.

%%%%%%%%
%% TESTS
%%%%%%%%

cantidadTestsTablero(2). % Actualizar con la cantidad de tests que entreguen
testTablero(1) :- tablero(0,0,[]).
testTablero(2) :- ocupar(pos(0,0), [[ocupada]]).
% Agregar más tests

cantidadTestsVecino(1). % Actualizar con la cantidad de tests que entreguen
testVecino(1) :- vecino(pos(0,0), [[_,_]], pos(0,1)).
% Agregar más tests

cantidadTestsCamino(0). % Actualizar con la cantidad de tests que entreguen
% Agregar más tests

cantidadTestsCaminoOptimo(0). % Actualizar con la cantidad de tests que entreguen
% Agregar más tests

cantidadTestsCaminoDual(0). % Actualizar con la cantidad de tests que entreguen
% Agregar más tests

tests(tablero) :- cantidadTestsTablero(M), forall(between(1,M,N), testTablero(N)).
tests(vecino) :- cantidadTestsVecino(M), forall(between(1,M,N), testVecino(N)).
tests(camino) :- cantidadTestsCamino(M), forall(between(1,M,N), testCamino(N)).
tests(caminoOptimo) :- cantidadTestsCaminoOptimo(M), forall(between(1,M,N), testCaminoOptimo(N)).
tests(caminoDual) :- cantidadTestsCaminoDual(M), forall(between(1,M,N), testCaminoDual(N)).

tests(todos) :-
  tests(tablero),
  tests(vecino),
  tests(camino),
  tests(caminoOptimo),
  tests(caminoDual).

tests :- tests(todos).