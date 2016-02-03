-module(srm648_div2_lv1).
-export([num_bought/2, test/1]).

num_bought(K, T) -> inner_loop (K, T, 1).
inner_loop(_, Acc, I) when Acc == 0 -> (I - 1);
inner_loop(K, Acc, I) ->
	inner_loop(K, Acc - K*math:pow(2, (I - 1)), (I+1)).

% 100, 300, 1 
% 100, (300-100*2^0), 2
% 100, (200-100*2^1), 3

test(1) -> num_bought(100,300); 
test(2) -> num_bought(150, 1050);
test(3) -> num_bought(160, 163680).

