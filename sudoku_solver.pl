% Index conversion
get_index(Row, Col, Index) :- Index is Row * 9 + Col.
get_column(Index, Col) :- Col is Index mod 9.
get_row(Index, Row) :- Row is Index div 9.
get_block(Index, Block) :-
	get_row(Index, R), get_column(Index, C),
	Block is (R div 3) * 3 + C div 3.
get_position(Index, Row, Col, Block) :-
	get_row(Index, Row), get_column(Index, Col), get_block(Index, Block).

% Find the available number with constraints
available(Row, Col, Block, X) :-
	between(1, 9, X),
	not(row_used(Row, X)),
	not(col_used(Col, X)),
	not(block_used(Block, X)).

% Initialize the constraints with the original problem
init_constraints([], _).

% Non-empty element
init_constraints([Num|Remain], Index) :-
	between(1, 9, Num),
	get_position(Index, R, C, B),
	% Add the constraints into database
	asserta(row_used(R, Num)),
	asserta(col_used(C, Num)),
	asserta(block_used(B, Num)),
	Index1 is Index + 1, init_constraints(Remain, Index1).

% Skip empty element
init_constraints([Num|Remain], Index) :-
	not(between(1, 9, Num)),
	Index1 is Index + 1, init_constraints(Remain, Index1).

% Dynamic add/delete constraints
modify_cons(Cons) :- asserta(Cons).
modify_cons(Cons) :- retract(Cons), fail.

% Solve
solve_sudoku(Prob, Sol) :-
	init_constraints(Prob, 0),
	solve(Prob, Sol, 0).

% Key procedure
solve([],Sol,_) :- Sol = [].

% Skip non-empty element
solve([Num|Remain], Sol, Index) :-
	between(1, 9, Num),
	Index1 is Index + 1, solve(Remain, Sol1, Index1), Sol = [Num|Sol1].

% Enumerate available number
solve([Num|Remain], Sol, Index) :-
	not(between(1, 9, Num)),
	get_position(Index, R, C, B),
	available(R, C, B, X),
	modify_cons(row_used(R, X)),
	modify_cons(col_used(C, X)),
	modify_cons(block_used(B, X)),
	Index1 is Index + 1, solve(Remain, Sol1, Index1), Sol = [X|Sol1].

sudoku_solver :-
	readln(Sudoku_prob),
	solve_sudoku(Sudoku_prob, Sol),
	forall(member(X, Sol), (write(X), tab(1))), halt.

:- initialization(sudoku_solver).

% sample input: 0 0 5 3 0 0 0 0 0 8 0 0 0 0 0 0 2 0 0 7 0 0 1 0 5 0 0 4 0 0 0 0 5 3 0 0 0 1 0 0 7 0 0 0 6 0 0 3 2 0 0 0 8 0 0 6 0 5 0 0 0 0 9 0 0 4 0 0 0 0 3 0 0 0 0 0 0 9 7 0 0
% sample output: 1 4 5 3 2 7 6 9 8 8 3 9 6 5 4 1 2 7 6 7 2 9 1 8 5 4 3 4 9 6 1 8 5 3 7 2 2 1 8 4 7 3 9 5 6 7 5 3 2 9 6 4 8 1 3 6 7 5 4 2 8 1 9 9 8 4 7 6 1 2 3 5 5 2 1 8 3 9 7 6 4

% sample input: 0 8 0 0 2 6 5 0 1 0 0 0 0 0 0 0 0 2 1 0 3 8 0 0 0 7 0 0 0 6 0 0 0 0 1 0 4 1 7 0 5 0 8 0 9 0 9 0 0 0 0 4 0 0 0 4 1 0 0 9 2 0 7 5 0 0 0 0 0 0 0 0 0 0 8 1 6 0 0 0 0
% sample output: 7 8 4 3 2 6 5 9 1 9 6 5 4 7 1 3 8 2 1 2 3 8 9 5 6 7 4 8 5 6 9 4 2 7 1 3 4 1 7 6 5 3 8 2 9 3 9 2 7 1 8 4 5 6 6 4 1 5 8 9 2 3 7 5 7 9 2 3 4 1 6 8 2 3 8 1 6 7 9 4 5