%%%-------------------------------------------------------------------
%%% @author User
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. февр. 2024 15:12
%%%-------------------------------------------------------------------
-module(mobius).
-author("User").

%% API
-export([result/0, is_prime/1, prime_factors/1, is_square_multiple/1, find_square_multiples/2]).

is_prime(N, M) when M*M > N -> true;
is_prime(N, M) -> Remainder = N rem M,
  if
    Remainder == 0 -> false;
    true -> is_prime(N, M+1)
  end.
is_prime(1) -> false;
is_prime(N) when N > 1 -> is_prime(N, 2).

prime_factors(N, Rest, M) when M*M > N -> [N|Rest];
prime_factors(N, Rest, M) -> ChkPrime = is_prime(M),
  if
    ChkPrime == true, (N rem M) == 0 -> prime_factors(N div M, [M|Rest], M);
    M == 2 -> prime_factors(N, Rest, 3);
    true -> prime_factors(N, Rest, M+2)
  end.
prime_factors(N) -> lists:reverse(prime_factors(N, [], 2)).

is_square_multiple(N) -> List = prime_factors(N),
  ListSize = erlang:length(List), SetSize = sets:size(sets:from_list(List)),
  if
    ListSize == SetSize -> false;
    true -> true
  end.

find_square_multiples(_N, _MaxN, List, Len, Count) when Len == Count -> NewList = lists:reverse(List), lists:nth(1, NewList);
find_square_multiples(N, MaxN, _List, _Len, _Count) when N > MaxN -> fail;
find_square_multiples(N, MaxN, List, Len, Count) -> ChkMultiple = is_square_multiple(N),
  if
    ChkMultiple == true -> find_square_multiples(N+1, MaxN, [N|List], Len+1, Count);
    true -> find_square_multiples(N+1, MaxN, [], 0, Count)
  end.
find_square_multiples(Count, MaxN) -> find_square_multiples(2, MaxN, [], 0, Count).
result() -> Start = os:timestamp(),
  io:format("~w~n", [find_square_multiples(6, 30000)]),
  io:format("total time taken ~f seconds~n", [timer:now_diff(os:timestamp(), Start) / 1000000]).

