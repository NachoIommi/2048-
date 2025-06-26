:- module(proylcc,
    [ randomBlock/2,
      shoot/5
    ]).

:- discontiguous merge_cluster/8.

empty_cell('-').        % Constante para la celda vacía

/*--------------------------------------------------------------------
  AUX: posiciones y utilidades sobre la grilla
--------------------------------------------------------------------*/

%% idx(+Fila,+Col,+NCols,-Idx)
idx(F,C,NCols,Idx) :- Idx is (F-1)*NCols + C.

%% dims(+Grid,+NCols,-NRows)
dims(Grid,NCols,NRows) :- length(Grid,L), NRows is L//NCols.

%% nth1_update(+Idx,+List,+Value,-NewList)
nth1_update(1,[_|Xs],V,[V|Xs]).
nth1_update(N,[X|Xs],V,[X|Ys]) :- N>1, N1 is N-1, nth1_update(N1,Xs,V,Ys).

%% get_cell(+Idx,+Grid,-Value)
get_cell(Idx,Grid,Val) :- nth1(Idx,Grid,Val).

%% vecinos4(+Idx,+NCols,+NRows,-IdxVec)
vecinos4(Idx,NCols,NRows,I2) :-
    (   IdxUp is Idx-NCols, IdxUp >= 1, I2 = IdxUp
    ;   IdxDown is Idx+NCols, IdxDown =< NCols*NRows, I2 = IdxDown
    ;   IdxLeft is Idx-1, Idx mod NCols =\= 1, I2 = IdxLeft
    ;   IdxRight is Idx+1, Idx mod NCols =\= 0, I2 = IdxRight
    ).

/*--------------------------------------------------------------------
  1. Colocar el bloque en la columna (spawn arriba)
--------------------------------------------------------------------*/

drop_block(Block,Col,Grid,NCols,NewGrid,Row) :-
    dims(Grid,NCols,NRows),
    place_from_top(Block,Col,1,NRows,Grid,NCols,NewGrid,Row).

place_from_top(Block,Col,Row,NRows,Grid,NCols,NewGrid,Row) :-
    Row =< NRows,
    idx(Row,Col,NCols,Idx),
    empty_cell(Empty),
    get_cell(Idx,Grid,Empty), !,
    nth1_update(Idx,Grid,Block,NewGrid).
place_from_top(Block,Col,Row,NRows,Grid,NCols,NewGrid,RowPlaced) :-
    Row < NRows,
    Row1 is Row+1,
    place_from_top(Block,Col,Row1,NRows,Grid,NCols,NewGrid,RowPlaced).
place_from_top(_,_,Row,NRows,_,_,_,_) :-
    Row > NRows,
    throw(error(column_full)).

/*--------------------------------------------------------------------
  2. Detectar clústeres y fusionar
--------------------------------------------------------------------*/

cluster_same(Idx,Grid,NCols,NRows,Val,Cluster) :-
    get_cell(Idx,Grid,Val),
    bfs_same([Idx],[],Grid,NCols,NRows,Val,Cluster).

bfs_same([],Visited,_,_,_,_,Visited).
bfs_same([I|Rest],Visited,Grid,NCols,NRows,Val,Cluster) :-
    (   member(I,Visited)
    ->  bfs_same(Rest,Visited,Grid,NCols,NRows,Val,Cluster)
    ;   findall(V,(vecinos4(I,NCols,NRows,V), get_cell(V,Grid,Val)),Vs),
        append(Rest,Vs,Front),
        bfs_same(Front,[I|Visited],Grid,NCols,NRows,Val,Cluster)
    ).

merge_cluster(Cluster,Idx,Grid,Val,NCols,GridFinal,MergedVal,IdxResult) :-
    length(Cluster,Size), Size>=2, !,
    merged_value(Val,Size,MergedVal),
    empty_cells(Cluster,Grid,Temp1),
    nth1_update(Idx,Temp1,MergedVal,Temp2),
    bubble_up(Idx,Temp2,NCols,Temp3,IdxAfter),
    compact_grid_up(Temp3,NCols,GridFinal),
    col_of_idx(IdxAfter,NCols,Col),
    find_block_in_column(GridFinal,NCols,Col,MergedVal,IdxResult).
merge_cluster(_,Idx,Grid,Val,_,Grid,Val,Idx).

merged_value(V,Size,Out) :- Exp is Size-1, Out is V * 2^Exp.

empty_cells([],G,G).
empty_cells([I|Is],Grid,GOut) :-
    empty_cell(Empty),
    nth1_update(I,Grid,Empty,G1),
    empty_cells(Is,G1,GOut).

bubble_up(Idx,Grid,NCols,GridOut,IdxOut) :-
    IdxUp is Idx-NCols,
    empty_cell(Empty),
    (   IdxUp >= 1, get_cell(IdxUp,Grid,Empty)
    ->  get_cell(Idx,Grid,Val),
        nth1_update(Idx,Grid,Empty,T1),
        nth1_update(IdxUp,T1,Val,T2),
        bubble_up(IdxUp,T2,NCols,GridOut,IdxOut)
    ;   GridOut = Grid, IdxOut = Idx
    ).

/*--------------------------------------------------------------------
  3. Compactar columnas (eliminar huecos)
--------------------------------------------------------------------*/

compact_grid_up(Grid,NCols,GridOut) :-
    dims(Grid,NCols,NRows),
    compact_columns(1,NCols,NRows,Grid,NCols,GridOut).

compact_columns(Col,MaxCol,_,Grid,_,Grid) :- Col > MaxCol, !.
compact_columns(Col,MaxCol,NRows,Grid,NCols,GridOut) :-
    column_indices(Col,NCols,NRows,Idxs),
    maplist({Grid}/[I,V]>>nth1(I,Grid,V),Idxs,Vals),
    empty_cell(Empty),
    include(\=(Empty),Vals,NonEmpty),
    length(NonEmpty,LNon),
    LEmpty is NRows-LNon,
    length(EmptyList,LEmpty), maplist(=(Empty),EmptyList),
    append(NonEmpty,EmptyList,NewVals),
    update_indices_with_vals(Idxs,NewVals,Grid,Grid1),
    Col1 is Col+1,
    compact_columns(Col1,MaxCol,NRows,Grid1,NCols,GridOut).

update_indices_with_vals([],[],G,G).
update_indices_with_vals([I|Is],[V|Vs],Grid,GOut) :-
    nth1_update(I,Grid,V,G1),
    update_indices_with_vals(Is,Vs,G1,GOut).

column_indices(Col,NCols,NRows,Idxs) :-
    findall(I,(between(1,NRows,R), idx(R,Col,NCols,I)),Idxs).

col_of_idx(Idx,NCols,Col) :- Col is ((Idx-1) mod NCols) + 1.

find_block_in_column(Grid,NCols,Col,Val,Idx) :-
    dims(Grid,NCols,NRows),
    between(1,NRows,R), idx(R,Col,NCols,Idx), nth1(Idx,Grid,Val), !.

/*--------------------------------------------------------------------
  4. Cascada de fusiones y efectos
--------------------------------------------------------------------*/

shoot(Block,Col,Grid,NCols,Effects) :-
    drop_block(Block,Col,Grid,NCols,G1,RowPlaced),
    idx(RowPlaced,Col,NCols,IdxPlaced0),
    get_cell(IdxPlaced0,G1,Val0),
    cascade_fuse(IdxPlaced0,G1,NCols,Val0,GridsAsc),
    grids_to_effects(GridsAsc,Effects).

cascade_fuse(Idx,Grid,NCols,Val,GridsAsc) :-
    cascade_fuse_(Idx,Grid,NCols,Val,[Grid],Rev),
    reverse(Rev,GridsAsc).

cascade_fuse_(Idx,Grid,NCols,Val,Acc,AccOut) :-
    dims(Grid,NCols,NRows),
    cluster_same(Idx,Grid,NCols,NRows,Val,Cluster),
    length(Cluster,Size), Size>=2, !,
    merge_cluster(Cluster,Idx,Grid,Val,NCols,G2,Val2,Idx2),
    % Llamada a la regla para actualizar los bloques prohibidos (acumulativo)
    update_forbidden_blocks_accumulated(Val2),
    cascade_fuse_(Idx2,G2,NCols,Val2,[G2|Acc],AccOut).
cascade_fuse_(_,_,_,_,Acc,Acc).

/*--------------------------------------------------------------------
  5. Convertir grillas → effects
--------------------------------------------------------------------*/

grids_to_effects([G],[effect(G,[])]).
grids_to_effects([Prev,Next|Rest],[effect(Prev,Ef)|Tail]) :-
    new_block_between(Prev,Next,Val),
    (   Val == none -> Ef = [] ; Ef = [newBlock(Val)]),
    grids_to_effects([Next|Rest],Tail).

new_block_between(G1,G2,Val) :-
    empty_cell(Empty),
    findall(V,(nth1(I,G2,V), nth1(I,G1,Empty), number(V)),Vs),
    (   Vs = [Val|_] -> true ; Val = none).

/*--------------------------------------------------------------------
  6. randomBlock (con cambios para bloques prohibidos)
--------------------------------------------------------------------*/

% Declaramos un hecho dinámico para almacenar la lista de bloques prohibidos.
% Inicializamos como una lista vacía.
:- dynamic(forbidden_blocks_accumulated/1).
forbidden_blocks_accumulated([]).

randomBlock(Grid, 2) :-
    forall(member(X, Grid), empty_cell(X)).

randomBlock(Grid, Block) :-
    maximoNumero(Grid, Max),
    MaxBlock is max(2, Max // 2),        
    potenciaDe2(MaxBlock, AllPotencias),
    % Filtramos las potencias para excluir los bloques prohibidos
    forbidden_blocks_accumulated(Forbidden),
    exclude(member_of_forbidden(Forbidden), AllPotencias, AvailablePotencias),
    (   AvailablePotencias = [] % Si no hay bloques disponibles, podemos manejar un caso de error o default
    ->  random_member(Block, AllPotencias) % En este caso, genera cualquiera si no hay opciones válidas
    ;   random_member(Block, AvailablePotencias)
    ).

% Predicado auxiliar para verificar si un elemento está en la lista de prohibidos
member_of_forbidden(ForbiddenList, Element) :-
    member(Element, ForbiddenList).

maximoNumero(Grid, Max) :-
    include(number, Grid, Numeros),
    (   Numeros = [] -> Max = 0
    ;   max_list(Numeros, Max)
    ).

potenciaDe2(Max, Lista) :-
    potencia_de2(1, Max, [], Lista).

potencia_de2(N, Max, Acc, Lista) :-
    Valor is 2^N,
    (   Valor =< Max
    ->  potencia_de2(N + 1, Max, [Valor|Acc], Lista)
    ;   reverse(Acc, Lista)
    ).
    
% Utilidad: log base 2 usando logaritmo natural
log2(X, L) :- L is log(X) / log(2).

% Regla para actualizar bloques prohibidos acumulativamente
update_forbidden_blocks_accumulated(MergedVal) :-
    forbidden_blocks_accumulated(CurrentForbidden),
    (   MergedVal >= 1024
    ->  log2(MergedVal, ExpF),
        Exponent is round(ExpF),       % Obtener el exponente de la potencia
        MaxIndex is Exponent - 9,
        findall(P, (between(1, MaxIndex, I), P is 2^I), BlocksToProhibit),
        append(BlocksToProhibit, CurrentForbidden, TempNewForbidden),
        list_to_set(TempNewForbidden, UniqueForbidden),
        retractall(forbidden_blocks_accumulated(_)),
        assertz(forbidden_blocks_accumulated(UniqueForbidden))
    ;   true  % Si MergedVal < 1024, no hacer nada
    ).
