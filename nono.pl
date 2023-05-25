/*Predicado para imprimir el nonograma.*/
/*Si la lista está vacía, se imprime un salto de línea.*/
escriuNonograma([]) :- nl. 
/*Se toma el primer elemento X de la lista y se pasa al siguiente predicado.*/
escriuNonograma([X|L1]) :- 
    /*Se imprime X y se hace un salto de línea.*/
    write(X), nl, 
    /*Se llama recursivamente con el resto de la lista.*/
    escriuNonograma(L1). 

/*Predicado para mostrar el nonograma en pantalla*/
mostraNonograma(L, Files, Columnas, IncFiles, IncColumnes) :-
    cls(), 
    /*Llamada al predicado auxiliar que permite mostrar el nonograma*/
    mostraNonogramaAux(L, Files, Columnas, IncFiles, IncColumnes).

/*Predicado auxiliar para mostrar el nonograma en pantalla*/
/*Si la lista está vacía, no hay nada más que mostrar.*/
mostraNonogramaAux([], _, _, _, _). 
mostraNonogramaAux([F|L], Files, Columnas, IncFiles, IncColumnes) :-
    /*Se llama a una función gotoXY(Columnas, Files) para colocarnos en la posición Columnas,Filas*/
    gotoXY(Columnas, Files), 
    /*Se llama a un predicado para mostrar una fila del nonograma.*/
    mostraNonoFila(F, Files, Columnas, IncColumnes),
    /*Se actualiza la posición para la próxima fila.*/ 
    Files2 is Files + IncFiles, 
    /*Se llama recursivamente con el resto de la lista.*/
    mostraNonogramaAux(L, Files2, Columnas, IncFiles, IncColumnes). 

/*Predicado para mostrar una fila del nonograma*/
/*Si la lista está vacía, no hay nada más que mostrar.*/
mostraNonoFila([], _, _, _). 
mostraNonoFila([X|L1], Fila, Columna, IncColumnes) :-
    /*Se llama a una función gotoXY(Columnas, Files) para colocarnos en la posición Columnas,Filas*/
    gotoXY(Columna, Fila),
    /*Se llama a una función color(X) para establecer el color en función del valor de X.*/
    color(X), 
    /*Se imprime "x".*/
    write("x"), 
    /*Se actualiza la posición de la columna*/
    Columna1 is Columna + IncColumnes, 
    /*Se llama recursivamente con el resto de la lista y los valores actualizados.*/
    mostraNonoFila(L1, Fila, Columna1, IncColumnes). 

/*Predicado principal para generar un nonograma aleatorio con los valores de Colors*/
ferNonograma(Colors, Files, Columnes, Nono) :-
    /*Se genera una matriz aleatoria de colores.*/
    generarMatrizAleatoria(Colors, Files, Columnes, [], Nono),
    /*Se imprime el nonograma generado.*/
    escriuNonograma(Nono), 
    /*Se muestra el nonograma dibujado en pantalla.*/
    mostraNonograma(Nono, 3, 5, 1, 3). 

/*Predicado para generar una matriz aleatoria de colores*/
/*Si el número de filas es 0, se termina la generación de la matriz.*/
generarMatrizAleatoria(_, 0, _, Nono, Nono). 
generarMatrizAleatoria(Colors, Files, Columnes, L, Nono) :-
    /*Debe cumplirse que el numero de filas aún sea mayor a 0*/
    Files > 0,
    /*Se genera una fila aleatoria de colores.*/
    generarFilaAleatoria(Colors, Columnes, Fila), 
    /*Se actualiza el número de filas restantes.*/
    FilesRestantes is Files - 1, 
    /*Se llama recursivamente con el resto de las filas.*/
    generarMatrizAleatoria(Colors, FilesRestantes, Columnes, [Fila|L], Nono). 

/*Predicado para generar una fila aleatoria de colores*/
/*Si el número de columnas es 0, se termina la generación de la fila.*/
generarFilaAleatoria(_, 0, []). 
generarFilaAleatoria(Colors, Columnes, [Color|L]) :-
    /*Debe cumplirse que el numero de columnas aún sea mayor a 0*/
    Columnes > 0,
    /*Se obtiene la longitud de la lista de colores.*/
    length(Colors, N), 
    /*Se genera un número aleatorio entre 0 y N-1.*/
    random(0, N, Index), 
    /*Se obtiene el color correspondiente al índice generado.*/
    nth0(Index, Colors, Color),
    /*Se actualiza el número de columnas restantes.*/ 
    ColumnesRestantes is Columnes - 1, 
    /*Se llama recursivamente con el resto de las columnas.*/
    generarFilaAleatoria(Colors, ColumnesRestantes, L). 

/*Predicado para extraer las pistas de un Nonograma*/
/*Si la lista está vacía finaliza*/
treuPistes([],[]).
treuPistes([X|L1],[Y|L2]):-
    /* Función auxiliar para extraer las pistas de una fila */
    extreu(X,Y),
    /* Se llama recursivamente con el resto de las listas */
    treuPistes(L1,L2).

/*Si la lista está vacía finaliza*/
extreu([],[]).
extreu([marca|L1],L2):-
    extreu(L1,L2).
extreu([X|L1],[[seguits,X,1]|L2]):-
    vegades(X,[X|L1],1),!,
    extreu(L1,L2).

extreu([X|L1],[[seguits,X,N]|L2]):-
    vegades(X,[X|L1],N),
    seguidos(X,N,[X|L1]),!,
    borrar(X,[X|L1],L3),
    extreu(L3,L2).

extreu([X|L1],[[no_seguits,X,N]|L2]):-
    vegades(X,[X|L1],N),!,
    poner_marca(X,L1,L3),
    extreu(L3,L2).

poner_marca(_, [], []).
poner_marca(X, [X|L1], [marca|L2]) :-
    poner_marca(X, L1, L2).
poner_marca(X, [Y|L1], [Y|L2]) :-
    X \= Y,
    poner_marca(X, L1, L2).

vegades(_, [], 0).
vegades(X, [Y|L], N):-
    X \= Y,
    vegades(X, L, N).
vegades(X, [X|L], N):-
    vegades(X, L, N1),
    N is N1 + 1.

seguidos(_,_,[]):-fail.
seguidos(X,N,[X|L1]):-verificarSeguidos(X,N,[X|L1]),!.
seguidos(X,N,[_|L1]):-seguidos(X,N,L1).

verificarSeguidos(X,1,[X|_]).
verificarSeguidos(X,N,[X|L1]):-N1 is N-1, verificarSeguidos(X,N1,L1).

/* Si la lista está vacía, el resultado también es una lista vacía */
borrar(_, [], []).

/* Si el elemento a borrar es igual al primer elemento de la lista, se descarta y se realiza una llamada recursiva */
borrar(X, [X|L1], L2) :-
    borrar(X, L1, L2).

/* Si el elemento a borrar no es igual al primer elemento de la lista,
   se mantiene el primer elemento y se realiza una llamada recursiva para procesar el resto de la lista */
borrar(X, [Y|L1], [Y|L2]) :-
    X \= Y, /* Verifica que X no sea igual a Y */
    borrar(X, L1, L2).

 /*Predicados para obtener la traspuesta de una matriz dada*/
traspuesta([], []).
traspuesta([[]|_], []).
traspuesta(Matriz, [Col|Columnas]) :-
    obtener_columna(Matriz, Col, RestoMatriz),
    traspuesta(RestoMatriz, Columnas).

obtener_columna([], [], []).
obtener_columna([[X|RestoFila]|Filas], [X|Col], [RestoFila|RestoFilas]) :-
    obtener_columna(Filas, Col, RestoFilas).

/*Predicado para obtener las pistas de un nonograma*/
descriuNonograma(Nono, L):-
    traspuesta(Nono, L2),
    treuPistes(L2, L3),
    treuPistes(Nono, L1),
    L = [L1,L3],
    !.

/*Predicado para visualizar las pistas obtenidas*/
mostraPistesHoritzontals([], _, _, _, _).
mostraPistesHoritzontals([Fila|Resto], F, C, FInc, CInc):-
    mostrarFilasP(Fila, F, C, CInc),
    /* Se actualiza la posición para la próxima fila. */ 
    F2 is F + FInc,
    /* Se llama recursivamente con el resto de la lista. */
    mostraPistesHoritzontals(Resto, F2, C, FInc, CInc). 

/* Si la lista está vacía, no hay nada más que mostrar. */
mostrarFilasP([], _, _, _). 
mostrarFilasP([[S,C,N]|L1], Fila, Columna, IncColumnes):-
    /* Se llama a una función gotoXY(Columnas, Files) para colocarnos en la posición Columnas,Filas */
    gotoXY(Columna, Fila),
    mostrarElemento(S,C,N),
    /* Se actualiza la posición de la columna */
    Columna1 is Columna + IncColumnes, 
    /* Se llama recursivamente con el resto de la lista y los valores actualizados. */
    mostrarFilasP(L1, Fila, Columna1, IncColumnes). 
    
mostrarElemento(no_seguits, Y, Z):-
    color(Y),
    write(Z).

mostrarElemento(seguits, Y, 1):-
    color(Y),
    write(1).

mostrarElemento(seguits, Y, Z):-
    Z\=1,
    color(Y),
    write(" <"), write(Z), write("> ").

/*Predicado para visualizar las pistas obtenidas*/
mostraPistesVerticals([], _, _, _, _).
mostraPistesVerticals([Fila|Resto], F, C, FInc, CInc):-
    mostrarFilasP2(Fila, F, C, FInc),
    /* Se actualiza la posición para la próxima fila. */ 
    C2 is C + CInc,
    /* Se llama recursivamente con el resto de la lista. */
    mostraPistesVerticals(Resto, F, C2, FInc, CInc). 

/* Si la lista está vacía, no hay nada más que mostrar. */
mostrarFilasP2([], _, _, _):- nl. 
mostrarFilasP2([[S,C,N]|L1], Fila, Columna, IncFilas):-
    /* Se llama a una función gotoXY(Columnas, Files) para colocarnos en la posición Columnas,Filas */
    gotoXY(Columna, Fila),
    mostrarElemento(S,C,N),
    /* Se actualiza la posición de la columna */
    Fila1 is Fila + IncFilas, 
    /* Se llama recursivamente con el resto de la lista y los valores actualizados. */
    mostrarFilasP2(L1, Fila1, Columna, IncFilas). 

/*Dada la descripcion de un nonograma, nos da la primera posible solución que encuentre*/
resolNonograma([L1,L2], Nono):-
    resolFiles(L1,Nono),
    traspuesta(Nono,NonoT),
    verificaTotesPistes(L2,NonoT),
    !.

resolFiles([],[]).
resolFiles([X|L1], [C1|L2]):-
    extreuColor(X,C),
    permutacio(C,C1),
    verificaPistes(X,C1),
    resolFiles(L1,L2).

/*Extrae los colores de una descripción*/
extreuColor([],[]).
extreuColor([[_,C,N]|L], L3):-
    replicar(N,C,X),
    extreuColor(L,L2),
    append(X,L2,L3).

replicar(1,X,[X]):-!.
replicar(N,X,[X|L]):-N1 is N-1, replicar(N1,X,L).

permutacio([],[]).
permutacio([X|Y],Z):-permutacio(Y,L),
inserir(X,L,Z).
inserir(E,L,[E|L]).
inserir(E,[X|Y],[X|Z]):-inserir(E,Y,Z).

/*Verifica si el nonograma cumple las pistas*/
verificaPistes([],_).
verificaPistes([[seguits,C,1]|L], L2):-
    aplanar(L2,L3),member(C,L3),!,verificaPistes(L,L2).
    
verificaPistes([[seguits,C,N]|L], L2):-
    seguidos(C,N,L2),!,verificaPistes(L,L2).

verificaPistes([[no_seguits,C,_]|L], L2):-
    vegades(C,L2,N2),not(seguidos(C,N2,L2)),!,verificaPistes(L,L2).

verificaTotesPistes([],_).
verificaTotesPistes([X|L], [Y|L2]):-
    verificaPistes(X,Y),
    verificaTotesPistes(L,L2).

/*Convierte una lista de listas, en una única lista*/
aplanar([],[]).
aplanar([X|L1],L2):-is_list(X), aplanar(X,L3), aplanar(L1,L4), append(L3,L4,L2).
aplanar([X|L1],[X|L2]):-aplanar(L1,L2).


color(negre):-write("\e[1;90m").
color(vermell):-write("\e[1;91m").
color(verd):-write("\e[1;92m").
color(groc):-write("\e[1;93m").
color(blau):-write("\e[1;94m").
color(lila):-write("\e[1;95m").
color(cel):-write("\e[1;96m").

gotoXY(C,F):-write('\e['),write(C),write(";"),write(F),write("H").

cls:-write('\e[2J'), gotoXY(0,0).

colorsValids([negre,vermell,verd,groc,blau,lila,cel]).

nono([[verd,lila,vermell,vermell],
[blau,verd,blau,blau],
[lila,blau,verd,verd],
[verd,blau,vermell,verd]]).