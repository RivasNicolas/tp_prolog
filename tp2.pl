%%%%%%%%%%%%%%%%%%%%%%%%
%% Tablero
%%%%%%%%%%%%%%%%%%%%%%%%

%% Ejercicio 1
%% tablero(+Filas,+Columnas,-Tablero) instancia una estructura de tablero en blanco
%% de Filas x Columnas, con todas las celdas libres.
tablero(0,_,[]).
tablero(Filas,Columnas,[Lista|Tablero]) :- Filas > 0, Columnas > 0, FilasAnteriores is Filas - 1, length(Lista, Columnas), tablero(FilasAnteriores, Columnas, Tablero).

%% Ejercicio 2
%% ocupar(+Pos,?Tablero) será verdadero cuando la posición indicada esté ocupada.
%%FM = filaMatriz
%%FSM = filasMatriz
%%CSM = columnasMatriz
%%CM = columnaMatriz
%%F = cantidadFilas
%%C = cantidadColumnas

%% Generación infinita de tableros.
%% tablero2(+Casillas, -Tablero, -Filas, -Columnas) será verdadero cuando la cantidad de celdas del Tablero sea igual a Casillas, con tantas filas como Filas y tantas columnas como Columnas.
tablero2(Casillas, Tablero, Filas, Columnas) :- between(1, Casillas, Filas), Columnas is Casillas / Filas, Casillas is Filas * Columnas, tablero(Filas, Columnas, Tablero).

ocupar(pos(0,0), T) :- nonvar(T), T = [[ocupada|_]|_].
ocupar(pos(0, C), [[_|CSM]|FSM]) :- C > 0, C1 is C - 1, ocupar(pos(0, C1), [CSM|FSM]).
ocupar(pos(F, C), [_|FSM]) :- F > 0, F1 is F - 1, ocupar(pos(F1, C), FSM).
ocupar(pos(Fila, Columna), Tablero) :- var(Tablero), CasillasMin is (Fila + 1) * (Columna + 1), desde(CasillasMin, Casillas), 
                                        tablero2(Casillas, Tablero, Filas, Columnas), Filas > Fila, Columnas > Columna, ocupar(pos(Fila, Columna), Tablero).

%% Ejercicio 3
%% vecino(+Pos, +Tablero, -PosVecino) será verdadero cuando PosVecino sea
%% un átomo de la forma pos(F', C') y pos(F',C') sea una celda contigua a
%% pos(F,C), donde Pos=pos(F,C). Las celdas contiguas puede ser a lo sumo cuatro
%% dado que el robot se moverá en forma ortogonal.

%% cantColumnas(+Tablero, -Columnas) será verdadero cuando Columnas sea la cantidad de columnas del tablero.
cantColumnas([X|_],Columnas) :- length(X,Columnas).

%% posicionCorrecta(+Pos, +Tablero) será verdadero cuando Pos sea una posición posible del tablero.
posicionCorrecta(pos(Fila, Columna), Tablero) :- length(Tablero, TotalFilas), Fila >= 0, Fila < TotalFilas, cantColumnas(Tablero, TotalColumnas), Columna < TotalColumnas, Columna >= 0.

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
estaOcupada(pos(F,C),[_|FSM]) :- F > 0, F1 is F-1, estaOcupada(pos(F1,C),FSM).

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

%%caminoAux(+Inicio, +Fin, +Tablero, -Camino, +Visitados)
caminoAux(Fin, Fin, Tablero, Camino, Visitados) :- posicionCorrecta(Fin, Tablero), not(estaOcupada(Fin,Tablero)), append(Visitados, [Fin], Camino).
caminoAux(Inicio, Fin, Tablero, Camino, Visitados) :- Inicio \= Fin, posicionCorrecta(Fin, Tablero), not(estaOcupada(Fin,Tablero)), 
                  posicionCorrecta(Inicio, Tablero), not(estaOcupada(Inicio,Tablero)), vecinoLibre(Inicio, Tablero, Vecino), 
                  not(member(Vecino, Visitados)), append(Visitados, [Inicio], Visitados1), caminoAux(Vecino, Fin, Tablero, Camino, Visitados1).

camino(Inicio, Fin, Tablero, Camino) :- caminoAux(Inicio, Fin, Tablero, Camino, []).

%% 5.1. Analizar la reversibilidad de los parámetros Fin y Camino justificando adecuadamente en cada
%% caso por qué el predicado se comporta como lo hace

%%Análisis de reversibilidad:
%% Fin no es reversible. Esto se debe a que en nuestro predicado estamos usando I\=F, al tratarse de una operación aritmética tanto las variables de Inicio como de Fin deben estar instanciadas.
%% Camino es reversible y al pasarle este instanciado al predicado camino lo que va a hacer es devolver True si el camino es un camino valido en ese tablero //REVISAR


%% Ejercicio 6
%% camino2(+Inicio, +Fin, +Tablero, -Camino) ídem camino/4 pero que las soluciones
%% se instancien en orden creciente de longitud.

camino2(I,F,T,C) :- cantColumnas(T, NC), length(T, NF), MAX is NC * NF, between(1, MAX, N), camino(I, F, T, C), length(C, N).

%% ComentarioDelGrupo: 
%% Se crea una cota máxima. Va a ser el total de casillas disponibles(Filas x Columnas), ya que este es el largo máximo que puede tener un camino 
%% pues por definición este no repite casillas.

%% 6.1. Analizar la reversibilidad de los parámetros Inicio y Camino justificando adecuadamente en
%% cada caso por qué el predicado se comporta como lo hace.

%%Analisis de reversibilidad:
%%Inicio: No es reversible ya que I se usa en el predicado camino y en este se puede observar que en caminoAux se necesita que este instanciado ya que estaria realizando una operacion aritmetica (I\=F).
%%Camino

%% Ejercicio 7
%% caminoOptimo(+Inicio, +Fin, +Tablero, -Camino) será verdadero cuando Camino sea un
%% camino óptimo sobre Tablero entre Inicio y Fin. Notar que puede no ser único.

caminoOptimo(Inicio,Fin,Tablero,Camino) :- camino(Inicio, Fin, Tablero, Camino), length(Camino, L), not((camino(Inicio, Fin, Tablero, Camino1), length(Camino1, L1), L1 < L)).

%% ComentarioDelGrupo:
%% Utiliza la técnica de Generate&Test. Se generan los caminos del tablero, para luego testear cuáles cumplen con la propiedad de ser óptimos.

%%%%%%%%%%%%%%%%%%%%%%%%
%% Tableros simultáneos
%%%%%%%%%%%%%%%%%%%%%%%%

%% Ejercicio 8
%% caminoDual(+Inicio, +Fin, +Tablero1, +Tablero2, -Camino) será verdadero
%% cuando Camino sea un camino desde Inicio hasta Fin pasando al mismo tiempo
%% sólo por celdas transitables de ambos tableros.

caminoDual(Inicio,Fila,Tablero1,Tablero2,Camino) :- camino(Inicio, Fila, Tablero1, Camino), camino(Inicio, Fila, Tablero2, Camino).
%% Utiliza la técnica de Generate&Test. Se generan los caminos del tablero 1, para luego testear cuáles son caminos del tablero 2.

%%%%%%%%
%% TESTS
%%%%%%%%

% Tableros
tablero(ej5x5, T) :- tablero(5, 5, T), ocupar(pos(1, 1), T), ocupar(pos(1, 2), T).
tablero(ej4x4, T) :- tablero(4, 4, T), ocupar(pos(1, 1), T), ocupar(pos(1, 2), T), ocupar(pos(0, 0),T).
tablero(ej4x3, T) :- tablero(4, 3, T), ocupar(pos(2, 0), T), ocupar(pos(2, 1), T).
tablero(ej3x3, T) :- tablero(3, 3, T), ocupar(pos(1, 1), T), ocupar(pos(1, 2), T).
tablero(ej3x3sinVecLib, T) :- tablero(3, 3, T), ocupar(pos(1, 0), T), ocupar(pos(0, 1), T), ocupar(pos(1, 2), T), ocupar(pos(2, 1), T).
tablero(ej2x2, T) :- tablero(2, 2, T).

cantidadTestsTablero(8). % Actualizar con la cantidad de tests que entreguen
testTablero(1) :- tablero(0,0,[]). 
testTablero(2) :- ocupar(pos(0,0), [[ocupada]]).
% Tablero generico
testTablero(3) :- tablero(ej4x4, T).
% Que ocupe una posicion fuera del tablero
testTablero(4) :- tablero(ej4x4, T), ocupar(pos(4, 3), T). 
% Que ocupe una posicion que ya esta ocupada
testTablero(5) :- tablero(ej4x4, T), ocupar(pos(1, 1), T).
% Crear un tablero con filas y/o columnas negativas.
testTablero(6) :- tablero(-2,-2,T).
% Tablero con una Fila/Columna entera ocupada (Sirve para testear camino)
testTablero(7) :- tablero(4,4,T), ocupar(pos(2,0), T), ocupar(pos(2,1),T), ocupar(pos(2,2),T), ocupar(pos(2,3),T).
% Tablero todo ocupado.
testTablero(8) :- tablero(2,2,T), ocupar(pos(0,0), T), ocupar(pos(1,0), T), ocupar(pos(0,1), T), ocupar(pos(1,1),T).
% Tablero no cuadrado

% Tests Ideas:
% Tablero generico
% Que ocupe una que no exista
% Que ocupe una que ya esta ocupada
% Que ocupe una que no esta ocupada, Ya Fue testeado con tablero Generico
% Crear un tablero con filas y/o columnas negativas.
% Tablero con una Fila/Columna entera ocupada (Sirve para testear camino)
% Tablero todo ocupado

% Agregar más tests

cantidadTestsVecino(1). % Actualizar con la cantidad de tests que entreguen
testVecino(1) :- vecino(pos(0,0), [[_,_]], pos(0,1)).
%Testear vecinos en los bordes  ---> Respuesta esperada: V=pos(0,1), V=pos(1,0)
testVecino(2) :- tablero(ej2x2, T) , vecino(pos(1,1), T, V)
%Posición inválida ---> Respuesta esperada: false
testVecino(3) :- tablero(ej2x2, T), vecino(pos(3,3), T, V).
%Caso con 4 respuestas posibles (incluso si algún vecino esta ocupado) ---> Respuesta esperada: V=pos(0,1), V=pos(1,0), V=pos(1,2), V=pos(2,1).
testVecino(4) :- tablero(ej3x3, T), vecino(pos(1,1), T, V).

%Tests Ideas Vecinos:
%Testear vecinos en los bordes
%Posición inválida
%Genérico

%TESTS VECINO LIBRE
%Posición sin vecinos libres al estar todos ocupados ---> Respuesta esperada: false.
testVecino(5) :- tablero(ej3x3sinVecLib, T), vecinoLibre(pos(1,1), T, VL).
%Posición con algún vecinos libres y vecinos ocupados, además se prueba que pasa si el origen está ocupado ---> Respuesta esperada: VL=pos(0,1), VL=pos(1,0), VL=pos(2,1).
testVecino(6) :- tablero(ej3x3, T), vecinoLibre(pos(1,1), T, VL).
%Posición con todos los vecinos libres ---> Respuesta esperada: VL=pos(0,1), VL=pos(1,0), VL=pos(1,2), VL=pos(2,1).
testVecino(7) :- tablero(3, 3, T), vecinoLibre(pos(1,1),T,VL).

%Tests Ideas Vecino Libres: (Con los anteriores ya esta, solo agregamos un par)
%Posicion sin vecinos libres.
%Posicion con algun vecino libre.
%Posicion con todos libres.
%Posicion de origen ocupada


% Agregar más tests

cantidadTestsCamino(0). % Actualizar con la cantidad de tests que entreguen

% Tests Ideas:
/*Tests camino(+Inicio, +Fin, +Tablero, -Camino) ---> 
  Respuesta esperada: 
  C=[pos(0, 1), pos(1, 1), pos(1, 0)], 
  C=[pos(0, 1), pos(0, 2), pos(1, 2), pos(1, 1), pos(1, 0)],
  C=[pos(0, 1), pos(0, 0), pos(1, 0)].*/
testCamino(1):- tablero(ej4x3,T), camino(pos(0,1), pos(1,0), T, C).
% Camino donde Inicio = Fin
% Camino donde Inicio /= Fin Generico
% Camino que empiece desde un Inicio ocupado.
% Camino que termine en un Fin ocupado.
% No existe camino.
% Inicio o Fin fuera del tablero OJO

% Tests camino(+Inicio, +Fin, +Tablero, +Camino).
% Pasarle un Inicio, Fin, Tablero y un camino valido.
% Pasarle un Inicio, Fin, Tablero y un camino no valido.

% IDEM los de camino2

% Agregar más tests

cantidadTestsCaminoOptimo(0). % Actualizar con la cantidad de tests que entreguen

% Tests Ideas:
% Caso generico sin camino instanciado
% IDEM LOS DE CAMINO


% Agregar más tests

cantidadTestsCaminoDual(0). % Actualizar con la cantidad de tests que entreguen

% Tests Ideas:
% Test con dos tableros que no tengan camino posible entre los dos
% Test con dos tableros que si tengan camino posible y tengan mas de uno
% Test con los dos tableros iguales
% Test con tableros de distintos tamaños

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
