:- use_module(library(lists)).

:- op(900, xfy, @).
:- op(800, xfx, $).
:- dynamic((@)/2).
:- discontiguous((@)/2).


odd(X) :- X rem 2=:=1.
even(X) :- \+(odd(X)).

Col$Row :- between(1, 10, Row), between(1, 10, Col).
within_board(C$R) :- C$R, between(1, 8, C), between(1, 8, R).

bg_color(C$R, yellow) :- within_board(C$R), can_move('♙', 6$7, C$R), !.
bg_color(C$R, grey) :- within_board(C$R), (odd(C), even(R); even(C), odd(R)), !.
bg_color(C$R, white).


color(Col$Row, black).

%V @ A$2 :-
%    V1 @ A$1,
%    V is V1 + 2.
%
%V @ 4$1 :-
%    cells_left_of(4$1, Cells),
%    cells_value(Cells, Vs),
%    sum_list(Vs, V).
%
%V @ 5$1 :-
%    range(1$1, 4$1, Cells),
%    cells_value(Cells, Vs),
%    sum_list(Vs, V).

0 @ 1$1.

reset_board :-
    forall(within_board(C$R), retractall(V @ C$R)),
    asserta('♙' @ 1$7),
    asserta('♙' @ 2$7),
    asserta('♙' @ 3$7),
    asserta('♙' @ 4$7),
    asserta('♙' @ 5$7),
    asserta('♙' @ 6$7),
    asserta('♙' @ 7$7),
    asserta('♙' @ 8$7),
    asserta('♖' @ 1$8),
    asserta('♘' @ 2$8),
    asserta('♗' @ 3$8),
    asserta('♕' @ 4$8),
    asserta('♔' @ 5$8),
    asserta('♗' @ 6$8),
    asserta('♘' @ 7$8),
    asserta('♖' @ 8$8),
    asserta('♟' @ 1$2),
    asserta('♟' @ 2$2),
    asserta('♟' @ 3$2),
    asserta('♟' @ 4$2),
    asserta('♟' @ 5$2),
    asserta('♟' @ 6$2),
    asserta('♟' @ 7$2),
    asserta('♟' @ 8$2),
    asserta('♜' @ 1$1),
    asserta('♞' @ 2$1),
    asserta('♝' @ 3$1),
    asserta('♛' @ 4$1),
    asserta('♚' @ 5$1),
    asserta('♝' @ 6$1),
    asserta('♞' @ 7$1),
    asserta('♜' @ 8$1).

can_move('♙', C$R1, C$R2) :- '♙' @ C$R1, R2 is R1 - 1, C$R2.
can_move('♙', C$R1, C$R2) :- '♙' @ C$R1, R2 is R1 - 2, C$R2.
can_move('♗', C1$R1, C2$R2) :- '♗' @ C1$R1, 0 is abs(R2 - R1) - abs(C2 - C1), C2$R2.

move(PieceType, C$R) :- can_move(PieceType, CurrCol$CurrRow, C$R), asserta(PieceType @ C$R), retract(PieceType @ CurrCol$CurrRow).

atom_number(A, N) :- atom_codes(A, Codes), number_codes(N, Codes).

parse_chess_cell_ref(C, R, Col$Row) :- char_code(C, N), Col is N - 96, atom_number(R, Row).

parse_algebraic_notation(Input, move('♙', C$R)) :-
    atom_chars(Input, [C_, R_]), parse_chess_cell_ref(C_, R_, C$R).

parse_algebraic_notation(Input, move('♗', C$R)) :-
    atom_chars(Input, ['B', C_, R_]), parse_chess_cell_ref(C_, R_, C$R).


%V @ 10$1 :- V @ 9$1.

side_effects :-
    reset_board,
    parse_algebraic_notation('d6', Command1), Command1,
    parse_algebraic_notation('Bf5', Command2), Command2.

range(Col1$Row1, Col2$Row2, R) :- findall(C$R, (between(Col1, Col2, C), between(Row1, Row2, R)), R).

cells_left_of(Col$Row, Cells) :-
    L is Col - 1,
    findall(C$Row, between(1, L, C), Cells).

cells_value(Cells, Vs) :- findall(V, (member(C$R, Cells), V @ C$R), Vs).

%findall(V @ A$B, V @ A$B, Z).
