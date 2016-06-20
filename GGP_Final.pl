% MIT License

% Copyright (c) 2016 Johannes Körner

% Permission is hereby granted, free of charge, to any person obtaining a copy
% of this software and associated documentation files (the "Software"), to deal
% in the Software without restriction, including without limitation the rights
% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
% copies of the Software, and to permit persons to whom the Software is
% furnished to do so, subject to the following conditions:

% The above copyright notice and this permission notice shall be included in all
% copies or substantial portions of the Software.

% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
% SOFTWARE.

%---General Remarks-------------------------------------------------------------

% Generally, Pablo calculates the minimax value of states. However, we have
% added the following improvements over naive minimax:
% - alpha-beta pruning
% - iterative deepening
% - transposition table
% - 'entropic heuristic'
% (In case we can not reach a leaf node, we use the number of legal moves
% available as an estimate for the goodness of a state.)
% - loop prevention
% (If we are in a loop, which happened several times with the reference player
% in the game Balone, we add the move that would continue the loop to a
% blacklist and do not consider it next time.)
%
% What Pablo is good at:
% - tic tac toe (seriously, best_2013 does not stand a chance)
% - games with small branching factors and flat game trees
%
% What Pablo is bad at:
% - games with big branching factors
% (If Pablo can not reach a leaf node, the transposition table can not be used.
% Thus the next time, we start at the beginning again. At the beginning this
% lead to always using the first available move. However, the
% 'entropic heuristic' helps a little bit with this.)
%
% Ideas that we had:
% - Move ordering (killer heuristic lead to worse results for some reason)
% - Searching for partial goal states (Did not really work well, complete goal
% 	states would have been nice)
% - MTD(f) (zero window search, did not work that well)

%---Header----------------------------------------------------------------------

:- module('Pablo',['ggp_Pablo_player'/5]).

%---Dynamic Predicate Declarations----------------------------------------------

% used to store the player role
:- dynamic(my_player/1).

% used to store some key-value pairs
:- dynamic(pablo_data/2).

% used to store the value of terminal nodes
:- dynamic(pablo_data/3).

% used to store the value of non-terminal nodes along with the best move
:- dynamic(pablo_data/4).

% used to store the number of legal moves available for states
:- dynamic(legal_moves/3).

%---Main------------------------------------------------------------------------

% initialize
'ggp_Pablo_player'([], _Role, _Move, _Time, _MovesList).

% If there is only one legal move, do it without searching.
% This is generally the case if it is not our turn and the only legal move is
% 'noop'. We do not need to initiate a search and waste computing time for that.
'ggp_Pablo_player'(State, Role, Move, _Time, _MovesList) :-
	findall(Role:M, gdl_legal(Role,M,State), [Move]).

% Blacklist a move to break a loop if applicable.
% The opponent did O1, we did P1, the opponent moved back to the previous state
% using O2, we did P2 and did the same. By choosing P1, we would continue the
% loop, so we choose to put it on the blacklist.
'ggp_Pablo_player'(_State, _Role, _Move, _Time, [O1,_P2,_O2,P1,O1|_List]) :-
	ggp_db(1, 'Pablo is feeling like Bill Murray...'), % (Groundhog Day)
	assert(pablo_data(blacklist,P1)),fail.

% Begin search
'ggp_Pablo_player'(State, Role, Move, _Time, _MovesList) :-
	ggp_store(Role, db_flag(0)),                       % set debug level to 0
	assert(my_player(Role)), % stores the player role
	% start iterative deepening at depth 1, the current state, current best value
	% -1000, anonymous current best move
	iterative_deepening(1,State,-1000,_BestMove0,Value,Move),
	ggp_db(1, 'Pablo chose':Move:Value).

%---Iterative Deepening---------------------------------------------------------

% iterative_deepening(+Depth,+State,+BestValue0,?BestMove0,-BestValue,-BestMove)

% If there is no time left, return the current best move and abort.
% no_time seemed to be too late in some cases, so we went with time_2.
iterative_deepening(_Depth,_State,BestValue,BestMove,BestValue,BestMove) :-
	time_2,ggp_db(1, 'Pablo ran out of time':BestMove),!.

% If there is a winning move, return it.
iterative_deepening(_Depth,_State,100,BestMove,100,BestMove) :-
	ggp_db(1, 'Pablo might win.'),!.

% Else, deepen the alpha beta search iteratively
iterative_deepening(Depth,State,BestValue0,BestMove0,BestValue,BestMove) :-
	alpha_beta(Depth, State, 1, -1000, 1000, Value, [Move0|_]),
	% [Move0|_] because BestMove contains opponent:noop aswell
	(Value > BestValue0 -> % we found a better move, prepare to pass it
		Val = Value,
		Move = Move0
	;
		Val = BestValue0, % we did not find a better move, keep the old one
		Move = BestMove0
	),
	D1 is Depth+1, % increase depth
	(pablo_data(exploring,1) -> % there is still something left to explore
		ggp_db(1, 'Pablo is exploring at depth':D1:'Best Move':Move:Val),
		retractall(pablo_data(exploring,_)), % reset the 'exploring' value
		iterative_deepening(D1,State,Val,Move,BestValue,BestMove) % recursive call
	;
		% we only read from the table, abort and return the current best move
		ggp_db(1, 'Pablo has nothing left to explore.'),
		BestValue = Val,
		BestMove = Move
	).

%---Alpha Beta Search-----------------------------------------------------------

% alpha_beta(+Depth,+State,+Player,+Alpha,+Beta,-BestValue,-BestMove)
% Finds the minimax value and best move for the state using alpha beta pruning
% and a transposition table. Player is 1 if the next move is by us, -1 if it is
% by the opponent.

% Depth limit reached and we have a stored value available
alpha_beta(0,State,Player,_,_,Value,_) :-
	pablo_get(Player,State,Value),!. % get the Value from pablo_data and abort

% Depth limit reached and we have no stored value available
alpha_beta(0,State,Player,_,_,Value,_) :-
	% We are not looking something up from the transposition table, so store that
	% we are still exploring the game tree.
	assert(pablo_data(exploring,1)),
	value(Player,State,Value). % get the value

% We know this State -> use the transposition table and abort
alpha_beta(D,State,Player,_,_,Value,Move) :-
	D > 0,
	pablo_get(Player,State,Value,Move),!.

% We have to explore this state
alpha_beta(D,State,Player,Alpha,Beta,Value,Move) :-
	D > 0,
	% We are not looking something up from the transposition table, so store that
	% we are still exploring the game tree.
	assert(pablo_data(exploring,1)),
	(gdl_terminal(State) -> % terminal state
		(pablo_get(Player,State,Value) -> % use stored value if available
			true
		;
			value(Player,State,Value) % else get the value
		)
	; % not terminal, so search normally
		D1 is D - 1, % decrease Depth
		role(Player,PlayerRole), % find out the corresponding role (we have 1 or -1)
		other_role(PlayerRole,OtherPlayerRole), % find out the opponent role
		% get all legal moves
		findall([PlayerRole:M,OtherPlayerRole:noop],
						gdl_legal(PlayerRole,M,State), PMoves),
		% subtract the blacklisted move if existent
		(pablo_data(blacklist,B) ->
			subtract(PMoves,[B],Moves),
			ggp_db(1, 'Not using blacklisted move':B),
			retractall(pablo_data(blacklist,_)) % we only want to avoid the move once
		;
			PMoves = Moves % else use all moves
		),
		% get the number of possible moves
		length(Moves,L),
		% store the number of legal moves for our 'entropic heuristic'
		legal_moves_put(Player,State,L),
		% recursively go through all moves to find the best
		alpha_beta(Moves, State, D1, Player, Alpha, Beta, nil, Value, Move),
		% store the found value and move, only succeeds for 'real' value
		% (meaning values we got from gdl_goal(...), not our estimates)
		ignore(pablo_put(Player,State,Value,Move))
	).

% alpha_beta(+Moves,+State,+Depth,+Player,+Alpha,+Beta,
%	+Move0,-BestValue,-BestMove)
% Finds the best move from a list of moves for the current state using alpha
% beta pruning and a transposition table. Player is 1 if the next move is by us,
% -1 if it is by the opponent. Move0 is the best move so far.

% no moves left, return the best move/value so far
alpha_beta([],_,_,_,Value,_,Best,Value,Best).

% we still have moves to process
alpha_beta([Move|Moves],State,D,Player,Alpha,Beta,Move0,BestValue,BestMove):-
	% We are not looking something up from the transposition table, so store that
	% we are still exploring the game tree.
	assert(pablo_data(exploring,1)),
	ggp_next_state(State,Move,State1), % get the next state for the move
	Opponent is -Player, % using 1 and -1
	Alpha1 is -Beta,
	Beta1 is -Alpha,
	alpha_beta(D, State1, Opponent, Alpha1, Beta1, Value1, _Move),
	Value is -Value1,
	(Value >= Beta -> % move to good to be true
		BestValue = Value,
		BestMove = Move
	;
		(time_2 -> % no time left -> abort
			BestValue = Value,
			BestMove = Move
		;
			(Value > Alpha ->
				% increase Alpha, update Move0
				alpha_beta(Moves,State,D,Player,Value,Beta,Move,BestValue,BestMove)
			;
				% else search normally
				alpha_beta(Moves,State,D,Player,Alpha,Beta,Move0,BestValue,BestMove)
			)
		)
	).

%---Helpers---------------------------------------------------------------------

% other_role(+First,-Role)
% for two player games, finds the second role given the first
other_role(First,Role) :-
	findall(Rola,gdl_role(Rola),Rolas),
	select(First,Rolas,[Role]).

% role(+R,-P)
% We use 1 for the player role and -1 for the opponent role, this predicate
% gives us the actual role for the representative number.
role(1,P) :-
	my_player(P).
role(-1,O) :-
	my_player(P),
	other_role(P,O).

% A heuristic used at the depth limit. Better than losing, but as we do not
% know more, assume it is worse than draw
base_value(25).

% legal_moves_put(+Role,+State,+Value)
% Stores the number of legal moves for a role/state combination
legal_moves_put(Role,State,Value) :-
	retractall(legal_moves(Role,State,_)),
	assert(legal_moves(Role,State,Value)).

% legal_moves_get(+Role,+State,-Value)
% Returns the stored number of legal moves for a role/state combination
legal_moves_get(Role,State,Value) :-
	legal_moves(Role,State,Value),!. % we only want one return value
legal_moves_get(_,_,0). % this way we do not need a fallback

% pablo_put(+Role,+Key,+Value)
% Stores values (overwriting old values).
% This is used to store terminal node values.
pablo_put(Role,Key,Value) :-
	retractall(pablo_data(Role,Key,_)),
	assert(pablo_data(Role,Key,Value)).

% pablo_get(+Role,+Key,-Value)
% Returns stored values
pablo_get(Role,Key,Value) :-
	pablo_data(Role,Key,Value).

% pablo_put(+Role,+Key,+Value,+Move)
% Stores values (overwriting old values).
% This is used to store non-terminal node values and corresponding best moves.
pablo_put(Role,Key,Value,Move) :-
	A is abs(Value),
	% we do not want the base value stored, only 'real' found values
	(A=0;A=50;A=100),
	retractall(pablo_data(Role,Key,_,_)),
	assert(pablo_data(Role,Key,Value,Move)).

% pablo_get(+Role,+Key,-Value,-Move)
% Returns stored values and moves
pablo_get(Role,Key,Value,Move) :-
	pablo_data(Role,Key,Value,Move).

% value(+Player,+State,-Value)
% Returns the utility for terminal states.
% For other states, returns the base estimate of 25 plus the number of available
% legal moves for the state. However, as this is not a stable estimate of the
% goodness of a state, we do not want the extimated value to surpass 49, so it
% is not rated better than draw states. In practice, we did not have this many
% moves available at any time.
value(Player,State,Value) :-
	(pablo_get(Player,State,Value) -> % use stored value if available
		true
	;
		my_player(P),
		(gdl_terminal(State) -> % If this is a terminal state, get the utility.
			gdl_goal(P,Val,State),
			Value is Player*Val, % Player is 1 for us and -1 for the opponent
			% update the store entry
			pablo_put(Player,State,Value) % we just got a value -> store it
		;
			base_value(Val), % If we just hit the depth limit, return the base value
			% look up the number of legal moves for the state if available
			legal_moves_get(Player,State,L),
			% multiply with the player factor (1 or -1)
			L1 is Player*L,
			% add up the values, L1 is 0 if we do not know the number of legal moves
			Val1 is Val+L1,
			% we do not want values > 49 as the number of legal moves is not a really
			% good estimate.
			Val2 is min(Val1,49),
			% multiply with the player factor (1 or -1)
			Value is Player*Val2
		)
	).
