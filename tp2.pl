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

%% desde(+X, -Y) será verdadero cuando X <= Y
desde(X, X).
desde(X, Y) :- N is X + 1, desde(N, Y).

ocupar(pos(0,0), Tablero) :- nonvar(Tablero), Tablero = [[ocupada|_]|_].
ocupar(pos(0, Columna), Tablero) :- nonvar(Tablero), Tablero = [[_|ColumnasMatriz]|FilasMatriz], Columna > 0, Columna1 is Columna - 1, ocupar(pos(0, Columna1), [ColumnasMatriz|FilasMatriz]).
ocupar(pos(Fila, Columna), Tablero) :- nonvar(Tablero), Tablero = [_|FilasMatriz], Fila > 0, Fila1 is Fila - 1, ocupar(pos(Fila1, Columna), FilasMatriz).
ocupar(pos(Fila, Columna), Tablero) :- var(Tablero), CasillasMin is (Fila + 1) * (Columna + 1), desde(CasillasMin, Casillas), 
                                        tablero2(Casillas, Tablero, Filas, Columnas), Filas > Fila, Columnas > Columna, ocupar(pos(Fila, Columna), Tablero).

%% Ejercicio 3
%% vecino(+Pos, +Tablero, -PosVecino) será verdadero cuando PosVecino sea
%% un átomo de la forma pos(F', C') y pos(F',C') sea una celda contigua a
%% pos(F,C), donde Pos=pos(F,C). Las celdas contiguas puede ser a lo sumo cuatro
%% dado que el robot se moverá en forma ortogonal.

%% cantColumnas(+Tablero, -Columnas) será verdadero cuando Columnas sea la cantidad de columnas del tablero. No tomamos como válido un tablero de 0 filas.
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

%% estaOcupada(+Pos, +Tablero). Será verdadero cuando la posición indicada esté ocupada y falso cuando esto no ocurra.
estaOcupada(pos(0,0),[[X|_]|_]) :- nonvar(X), X = ocupada.
estaOcupada(pos(0,Columna),[[_|ColumnasMatriz]|FilasMatriz]) :- Columna > 0, Columna1 is Columna-1, estaOcupada(pos(0,Columna1),[ColumnasMatriz|FilasMatriz]).
estaOcupada(pos(Fila,Columna),[_|FilasMatriz]) :- Fila > 0, Fila1 is Fila-1, estaOcupada(pos(Fila1,Columna),FilasMatriz).

vecinoLibre(Pos,Tablero,PosVecino) :- vecino(Pos,Tablero,PosVecino), not(estaOcupada(PosVecino, Tablero)).
%% ComentarioDelGrupo: 
%% Utiliza la técnica de Generate&Test. Se generan todos los vecinos, para luego testear cuáles no están ocupados.

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

%% caminoAux(+Inicio, +Fin, +Tablero, ?Camino, +Visitados)
%% Visitados se instancia con la lista auxiliar con las posiciones visitadas. 
caminoAux(Fin, Fin, Tablero, Camino, Visitados) :- posicionCorrecta(Fin, Tablero), not(estaOcupada(Fin,Tablero)), append(Visitados, [Fin], Camino).
caminoAux(Inicio, Fin, Tablero, Camino, Visitados) :- Inicio \= Fin, posicionCorrecta(Fin, Tablero), not(estaOcupada(Fin,Tablero)), 
                  posicionCorrecta(Inicio, Tablero), not(estaOcupada(Inicio,Tablero)), vecinoLibre(Inicio, Tablero, Vecino), 
                  not(member(Vecino, Visitados)), append(Visitados, [Inicio], Visitados1), caminoAux(Vecino, Fin, Tablero, Camino, Visitados1).

camino(Inicio, Fin, Tablero, Camino) :- caminoAux(Inicio, Fin, Tablero, Camino, []).

%% 5.1. Analizar la reversibilidad de los parámetros Fin y Camino justificando adecuadamente en cada
%% caso por qué el predicado se comporta como lo hace

%%Análisis de reversibilidad:
%% Si Fin no está instanciada, caminoAux/5 instancia Fin en Inicio producto de la primera cláusula. 
%% Luego, si Inicio es una posición correcta del Tablero que no está ocupada, append\3 arrojará el resultado Camino = [Inicio] debido a que Visitados está instanciado en [].  
%% Por ende, este también será el resultado de camino/4 si Camino no se encuentra instanciada. Si se encuentra instanciada, devolverá True si es igual al resultado del append.
%% Al pedirle más resultados, y entrar en la segunda cláusula de caminoAux/5, el resultado es false pues se utiliza el operador \= y Fin, que no se encuentra instanciada, es uno de sus términos.
%% Recordamos, X \= Y: X no unifica con Y. Ambos términos deben estar instanciados.
%% En caso de que Inicio no sea una posición correcta del Tablero, la consulta será false. Es la respuesta de la primera cláusula debido al predicado posicionCorrecta\2 y 
%% en la segunda cláusula por lo explicado anteriormente del operador \=.
%% Entendemos que este comportamiento no sería el esperado, el predicado camino\4 llamado con una posición instanciada de Inicio correcta para el Tablero instanciado y 
%% un Camino también instanciado con un camino válido para el Tablero con su primera posición en Inicio, arroja false si no se instancia Fin. Y si no se instancia Camino, sólo se arroja un
%% resultado posible que es [Inicio], si Inicio es una posición válida.

%% Si Fin está instanciada y reduce con Inicio, entra por la primera cláusula de caminoAux/5 y la respuesta dependerá de si Fin es una posición correcta del tablero o no.
%% En el primer caso, si Camino no se encuentra instanciada arrojará el resultado del append entre Visitados, inicialmente instanciada en [], y [Fin].
%% Si Camino sí se encuentra instanciada, el predicado será true si el resultado del append coincide con Camino. 
%% En el segundo caso, false pues este también es el resultado al entrar en la segunda cláusula ya que no vale Inicio \= Fin.
%% Este es el comportamiento esperado.
%% Si Fin está instanciada y no reduce con Inicio, no entra en la primera cláusula. Al entrar en la segunda, el predicado será false si alguno de los siguientes lo es
%% posicionCorrecta(Fin, Tablero), not(estaOcupada(Fin,Tablero)), posicionCorrecta(Inicio, Tablero), not(estaOcupada(Inicio,Tablero)). En caso de que ninguno lo sea, 
%% se instancia un vecino libre a través del predicado vecinoLibre\3. Si este cumple que no pertenece a Visitados se llama nuevamente a caminoAux\5 con el vecino en el primer argumento.
%% Este proceso se repetirá hasta que Vecino reduzca con Fin, caso que ya fue explicado que funciona correctamente. Esto puede no suceder nunca, en el caso de que no exista un camino entre
%% las dos casillas enviadas. Este caso eventualmente dará false ya que por la instanciación de las variables el único predicado que cuenta con varios resultados posibles es vecinoLibre\3. 
%% El otro podría ser append\3 pero Visitados e [Inicio] siempre se encuentran instanciados, llevando a un resultado único que el append instancia en Visitados1.
%% Retomando, el máximo de soluciones de vecinoLibre\3 son 4 debido a las características del tablero. Además, la cantidad de casillas en un tablero son finitas y, como no se repiten estas en
%% una búsqueda de resultado debido a not(member(Vecino, Visitados)), el árbol de búsqueda de caminoAux\5 con la instanciación caminoAux(+Inicio, +Fin, +Tablero, ?Camino, +Visitados) es finito.
%% Por lo que si no existe un camino, independiemente de la instanciación de Camino, la respuesta será false.
%% Entonces, el comportamiento de camino/4 con la instanciación camino(+Inicio, +Fin, +Tablero, ?Camino) es correcto.

%% Concluimos que Camino es reversible y Fin no.


%% Ejercicio 6
%% camino2(+Inicio, +Fin, +Tablero, -Camino) ídem camino/4 pero que las soluciones
%% se instancien en orden creciente de longitud.

camino2(Inicio,Fin,Tablero,Camino) :- cantColumnas(Tablero, NumColumnas), length(Tablero, NumFilas), MAX is NumColumnas * NumFilas, between(1, MAX, N), 
                                      camino(Inicio, Fin, Tablero, Camino), length(Camino, N).

%% ComentarioDelGrupo: 
%% Se crea una cota máxima. Va a ser el total de casillas disponibles(Filas x Columnas), ya que este es el largo máximo que puede tener un camino 
%% pues por definición este no repite casillas. Sería el camino que recorre todo el tablero. De esta manera, el predicado no se cuelga.

%% 6.1. Analizar la reversibilidad de los parámetros Inicio y Camino justificando adecuadamente en
%% cada caso por qué el predicado se comporta como lo hace.

%% Ambos parámetros son argumentos en el llamado al predicado camino\4. Por lo que su reversibilidad estará regida por este.
%% Similar a lo que sucedía con Fin en el ejercicio anterior, 
%% si Inicio no está instanciada, caminoAux/5 instancia Inicio en Fin producto de la primera cláusula.
%% Ya fue explicado que el resultado de camino/4 si Inicio, ahora instanciada en Fin, es una posición correcta del Tablero que no está ocupada y Camino no está instanciada es Camino = [Inicio].
%% Por lo que length(Camino, N) es verdadera si sólo si N = 1. Esto sucede con el primer resultado que arroja between(1, MAX, N). Ya que tanto 1 como MAX siempre están instanciados.
%% El último debido a que NumColumnas y NumFilas siempre están instanciado porque Tablero debe estarlo. 
%% Entonces, camino2\4 arrojará un único resultado que es [Inicio] en este caso.
%% Si al instanciar Inicio con Fin, esta no es una posición correcta del Tablero, por lo explicado en el ejercicio anterior el resultado de camino\4 será false y por ende el de camino2\4 también.
%% Al igual que lo sucedido en el ejercicio anterior, entendemos que este comportamiento no sería el esperado.

%% Si tanto Inicio, Fin y Tablero están instanciadas al llamar camino\4 ya fue explicado por qué Camino es reversible. Esto implica que siempre length(Camino, N) tendrá ambos argumentos 
%% instanciados. 
%% Si Camino está instanciada, y el resto de los parámetros también, la respuesta de camino2\4 será verdadera si la de camino\4 lo es. Esto porque, por lo ya explicado, el largo del Camino
%% será entre 1 y el total de casillas del tablero. A este valor una única vez estará instanciado en N debido al between, logrando que valga length(Camino, N) y el predicado sea verdadero.
%% Como la respuesta de camino\4 con Camino instanciado es única y sólo una vez se cumplirá length(Camino, N), la respuesta de camino2\4 no tendrá repetidos y será verdadera cuando
%% Camino es un camino válido en Tablero que une Inicio y Fin. Este es el comportamiento esperado.
%% Si Camino no está instanciada, ya fue explicado que camino\4 arrojará todos los caminos válidos de Inicio a Fin en el Tablero. Por lo que camino\2 arrojará estos mismos caminos a medida que
%% vayan cumpliendo length(Camino, N). El valor de N es instanciado por between(1, MAX, N). A medida que existan, camino\2 arrojará los caminos de largo 1, al pedir más resultado los de largo 2,
%% luego los de largo 3 y así sucesivamente hasta el largo máximo, que es el camino que cubre todo el tablero. Este es el comportamiento esperado.

%% Concluimos que Camino es reversible e Inicio no.

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

%% ComentarioDelGrupo:
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
