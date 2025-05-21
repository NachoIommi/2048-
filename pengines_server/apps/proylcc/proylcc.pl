:- module(proylcc, [
    init/2,
    randomBlock/2,
    shoot/5
]).

:- use_module(init).           % init(Grid, NumOfColumns)
:- use_module(library(random)).
:- use_module(library(lists)).
:- use_module(library(clpfd)).  % for transpose/2

/* randomBlock(+Grid, -Block)
   Elige aleatoriamente un valor dentro del rango permitido
   según el máximo actual de la grilla. */
randomBlock(Grid, Block) :-
    max_in_grid(Grid, Max),
    allowed_range(Max, Range),
    random_member(Block, Range).

randomBlock([
	4,2,8,64,32,
	2,-,-,4,16,
	-,-,-,-,2,
	-,-,-,-,16,
	-,-,-,-,2,
	-,-,-,-,-,
	-,-,-,-,-
], 2).

randomBlock(_Grid, 4).

/**
 * shoot(+Block, +Column, +Grid, +NumOfColumns, -Effects) 
 * RGrids es la lista de grillas representando el efecto, en etapas, de combinar las celdas del camino Path
 * en la grilla Grid, con número de columnas NumOfColumns. El número 0 representa que la celda está vacía. 
 */

shoot(2, 1, 
	[
		4,2,8,64,32,
		2,-,-,4,16,
		-,-,-,-,2,
		-,-,-,-,16,
		-,-,-,-,2,
		-,-,-,-,-,
		-,-,-,-,-
	], 5,
	[
		effect([
			4,2,8,64,32,
			2,-,-,4,16,
			2,-,-,-,2,
			-,-,-,-,16,
			-,-,-,-,2,
			-,-,-,-,-,
			-,-,-,-,-
		], []),
		effect([
			4,2,8,64,32,
			4,-,-,4,16,
			-,-,-,-,2,
			-,-,-,-,16,
			-,-,-,-,2,
			-,-,-,-,-,
			-,-,-,-,-
		], [newBlock(4)]),
		effect([
			8,2,8,64,32,
			-,-,-,4,16,
			-,-,-,-,2,
			-,-,-,-,16,
			-,-,-,-,2,
			-,-,-,-,-,
			-,-,-,-,-
		], [newBlock(8)])
	]
).

shoot(2, 2, 
	[
		4,2,8,64,32,
		2,-,-,4,16,
		-,-,-,-,2,
		-,-,-,-,16,
		-,-,-,-,2,
		-,-,-,-,-,
		-,-,-,-,-
	], 5,
	[
		effect([
			4,2,8,64,32,
			2,2,-,4,16,
			-,-,-,-,2,
			-,-,-,-,16,
			-,-,-,-,2,
			-,-,-,-,-,
			-,-,-,-,-
		], []),
		effect([
			4,-,8,64,32,
			-,8,-,4,16,
			-,-,-,-,2,
			-,-,-,-,16,
			-,-,-,-,2,
			-,-,-,-,-,
			-,-,-,-,-
		], [newBlock(8)]),
		effect([
			4,8,8,64,32,
			-,-,-,4,16,
			-,-,-,-,2,
			-,-,-,-,16,
			-,-,-,-,2,
			-,-,-,-,-,
			-,-,-,-,-
		], []),
		effect([
			4,16,-,64,32,
			-,-,-,4,16,
			-,-,-,-,2,
			-,-,-,-,16,
			-,-,-,-,2,
			-,-,-,-,-,
			-,-,-,-,-
		], [newBlock(16)])
	]
).

shoot(2, 3, 
	[
		4,2,8,64,32,
		2,-,-,4,16,
		-,-,-,-,2,
		-,-,-,-,16,
		-,-,-,-,2,
		-,-,-,-,-,
		-,-,-,-,-
	], 5,
	[
		effect([
			4,2,8,64,32,
			2,-,2,4,16,
			-,-,-,-,2,
			-,-,-,-,16,
			-,-,-,-,2,
			-,-,-,-,-,
			-,-,-,-,-
		], [])
	]
).

shoot(2, 4, 
	[
		4,2,8,64,32,
		2,-,-,4,16,
		-,-,-,-,2,
		-,-,-,-,16,
		-,-,-,-,2,
		-,-,-,-,-,
		-,-,-,-,-
	], 5,
	[
		effect([
			4,2,8,64,32,
			2,-,-,4,16,
			-,-,-,2,2,
			-,-,-,-,16,
			-,-,-,-,2,
			-,-,-,-,-,
			-,-,-,-,-
		], []),
		effect([
			4,2,8,64,32,
			2,-,-,4,16,
			-,-,-,4,-,
			-,-,-,-,16,
			-,-,-,-,2,
			-,-,-,-,-,
			-,-,-,-,-
		], [newBlock(4)]),
		effect([
			4,2,8,64,32,
			2,-,-,4,16,
			-,-,-,4,16,
			-,-,-,-,2,
			-,-,-,-,-,
			-,-,-,-,-,
			-,-,-,-,-
		], []),
		effect([
			4,2,8,64,32,
			2,-,-,8,32,
			-,-,-,-,2,
			-,-,-,-,-,
			-,-,-,-,-,
			-,-,-,-,-,
			-,-,-,-,-
		], [newBlock(8), newBlock(32)]),
		effect([
			4,2,8,64,64,
			2,-,-,8,2,
			-,-,-,-,-,
			-,-,-,-,-,
			-,-,-,-,-,
			-,-,-,-,-,
			-,-,-,-,-
		], [newBlock(64)]),		
		effect([
			4,2,8,-,128,
			2,-,-,8,2,
			-,-,-,-,-,
			-,-,-,-,-,
			-,-,-,-,-,
			-,-,-,-,-,
			-,-,-,-,-
		], []),			
		effect([
			4,2,8,8,128,
			2,-,-,-,2,
			-,-,-,-,-,
			-,-,-,-,-,
			-,-,-,-,-,
			-,-,-,-,-,
			-,-,-,-,-
		], [newBlock(128)]),			
		effect([
			4,2,-,16,128,
			2,-,-,-,2,
			-,-,-,-,-,
			-,-,-,-,-,
			-,-,-,-,-,
			-,-,-,-,-,
			-,-,-,-,-
		], [newBlock(16)])
	]
).

shoot(2, 5, 
	[
		4,2,8,64,32,
		2,-,-,4,16,
		-,-,-,-,2,
		-,-,-,-,16,
		-,-,-,-,2,
		-,-,-,-,-,
		-,-,-,-,-
	], 5,
	[
		effect([
			4,2,8,64,32,
			2,-,-,4,16,
			-,-,-,-,2,
			-,-,-,-,16,
			-,-,-,-,2,
			-,-,-,-,2,
			-,-,-,-,-
		], []),
		effect([
			4,2,8,64,32,
			2,-,-,4,16,
			-,-,-,-,2,
			-,-,-,-,16,
			-,-,-,-,4,
			-,-,-,-,-,
			-,-,-,-,-
		], [newBlock(4)])
	]
).


/* shoot(+Block, +Lane, +Grid, +NumCols, -Effects)
   Inserta el bloque en la columna Lane y resuelve las fusiones en cadena. */
shoot(Block, Lane, Grid, NumCols, Effects) :-
    insert_block(Grid, NumCols, Lane, Block, G1),
    resolve(G1, NumCols, Effects).

/* max_in_grid(+Grid, -Max)
   Encuentra el valor máximo (ignorando entradas no numéricas). */
max_in_grid(Grid, Max) :-
    include(number, Grid, Numbers),
    ( Numbers = [] -> Max = 0 ; max_list(Numbers, Max) ).

/* allowed_range(+Max, -Range)
   Rangos de generación según el bloque máximo alcanzado. */
allowed_range(Max, Range) :-
    ( Max <   16 -> Range = [2,4]
    ; Max <   64 -> Range = [2,4,8]
    ; Max <  256 -> Range = [2,4,8,16]
    ; Max < 1024 -> Range = [2,4,8,16,32,64]
    ;               Range = [4,8,16,32,64,128]
    ).

/* insert_block(+Grid, +NumCols, +Lane, +Block, -NewGrid)
   Inserta Block en la primera posición vacía (0) de la columna Lane. */
insert_block(Grid, NumCols, Lane, Block, NewGrid) :-
    column_index(Lane, NumCols, Grid, Col),
    append(Prefix, [0|Suffix], Col),
    !,
    append(Prefix, [Block|Suffix], Col1),
    set_column(Lane, NumCols, Grid, Col1, NewGrid).

/* resolve(+Grid0, +NumCols, -Effects)
   Genera lista de effect(Grid, Info) hasta que no haya más fusiones. */
resolve(Grid0, NumCols, Effects) :-
    ( merge_once(Grid0, NumCols, Grid1, Info)
    -> Effects = [effect(Grid1, Info)|Rest],
       resolve(Grid1, NumCols, Rest)
    ;  Effects = [effect(Grid0, [])]
    ).

/* merge_once(+G, +NumCols, -G2, -Info)
   Aplica mezcla+gravedad en todas las columnas, produce G2 e Info. */
merge_once(G, NumCols, G2, Info) :-
    rows(G, NumCols, Rows),
    transpose(Rows, Cols),
    maplist(process_column, Cols, ColResults),
    extract_merged(ColResults, Info, NewCols),
    Info \= [],
    transpose(NewCols, NewRows),
    append(NewRows, G2).

/* process_column(+Column, -merged(NewCol, InfoList)) */
process_column(Column, merged(Column, [])) :-
    \+ has_pair(Column), !.
process_column(Column, merged(ColOut, Info)) :-
    merge_column(Column, ColMerged, NewVals),
    gravity(ColMerged, ColOut),
    findall(newBlock(V), member(V, NewVals), Info).

/* has_pair(+Col) detecta adyacentes iguales no cero */
has_pair([A,B|T]) :-
    A \= 0, A == B, !;
    has_pair([B|T]).

/* merge_column(+In, -Out, -NewValues) */
merge_column([], [], []).
merge_column([A,A|T], [B|R], [B|NV]) :-
    A \= 0,
    B is A*2,
    !,
    merge_column(T, R, NV).
merge_column([H|T], [H|R], NV) :-
    merge_column(T, R, NV).

/* gravity(+In, -Out) desplaza ceros al final */
gravity(ColIn, ColOut) :-
    include(\=(0), ColIn, NonZero),
    length(ColIn, Len),
    length(NonZero, NZ),
    Zeros is Len - NZ,
    length(Pad, Zeros),
    maplist(=(0), Pad),
    append(NonZero, Pad, ColOut).

/* column_index(+Lane, +NumCols, +Grid, -Col) */
column_index(Lane, NumCols, Grid, Col) :-
    findall(
      V,
      ( nth0(I, Grid, V),
        J is I mod NumCols,
        J+1 =:= Lane
      ),
      Col
    ).

/* set_column(+Lane, +NumCols, +Grid, +NewCol, -NewGrid) */
set_column(Lane, NumCols, Grid, NewCol, NewGrid) :-
    rows(Grid, NumCols, Rows),
    maplist(replace_in_row(Lane, NewCol), Rows, RowsOut),
    append(RowsOut, NewGrid).

rows([], _, []).
rows(Grid, NumCols, [Row|Rest]) :-
    length(Row, NumCols),
    append(Row, Tail, Grid),
    rows(Tail, NumCols, Rest).

replace_in_row(Lane, NewCol, Row, RowOut) :-
    nth1(Lane, NewCol, CVal),
    !,
    replace_nth1(Lane, Row, CVal, RowOut).
replace_in_row(_, _, Row, Row).

replace_nth1(N, List, Val, Out) :-
    append(P, [_|S], List),
    length(P, K), K+1 =:= N,
    append(P, [Val|S], Out),
    same_length(List, Out).

/* extract_merged(+ColResults, -InfoAll, -NewCols) */
extract_merged([], [], []).
extract_merged([merged(Col, Info)|Rs], AllInfo, [Col|Cs]) :-
    extract_merged(Rs, RestInfo, Cs),
    append(Info, RestInfo, AllInfo).
