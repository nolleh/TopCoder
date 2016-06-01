-module(srm649_div2_lv1).
-export([check/2, test/1]).

check(S, T) when ((length(T) + 1) /= length(S)) -> "Impossible";
check(S, T) ->
  case lists:any ((fun(S2) when S2 == T -> true; (_) -> false end), 
  	  [[S2 || S2 <- delete(S, I)] || I <- lists:seq(0, length(T))]) of
  	true -> "Possible";
  	false -> "Impossible"
  end.

delete(List, N) ->
  {L1, [_|L2]} = lists:split(N, List),
  L1 ++ L2.

test(1) -> check("sunuke", "snuke"); 
test(2) -> check("snuke", "snuke");
test(3) -> check("snuke", "skue");
test(4) -> check("snukent", "snuke");
test(5) -> check("aaaaa", "aaaa");
test(6) -> check("aaaaa", "aaa");
test(7) -> check("topcoder", "tpcoder");
test(8) -> check("singleroundmatch", "singeroundmatc").