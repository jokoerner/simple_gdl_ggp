# simple_gdl_ggp

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
