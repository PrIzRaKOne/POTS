%%%-------------------------------------------------------------------
%%% @author User
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. февр. 2024 15:03
%%%-------------------------------------------------------------------
-module(fib).
-author("User").

%% API
-export([fib_p/1, fib_g/1, result/0, tail_fib/1]).

% Функция fib_p/1 вычисляет n-е число Фибоначчи рекурсивно.
fib_p(0) -> 0;  % Если n равно 0, результат равен 0.
fib_p(1) -> 1;  % Если n равно 1, результат равен 1.
fib_p(N) -> fib_p(N-1)+fib_p(N-2).  % В остальных случаях результат - сумма двух предыдущих чисел Фибоначчи.

% Функция fib_g/1 вычисляет n-е число Фибоначчи с использованием охранного выражения.
fib_g(N) when N < 2 -> N;  % Если n меньше 2, результат равен n.
fib_g(N) -> fib_g(N-1)+fib_g(N-2).  % В остальных случаях результат - сумма двух предыдущих чисел Фибоначчи.

% Функция tail_fib/1 вычисляет n-е число Фибоначчи с использованием хвостовой рекурсии.
tail_fib(0, Result, _Next) -> Result;  % Если номер итерации равен 0, возвращается текущий результат.
tail_fib(Iter, Result, Next) when Iter > 0 -> tail_fib(Iter-1, Next, Result+Next).  % Иначе, рекурсивно вычисляем следующее число Фибоначчи.
tail_fib(N) -> tail_fib(N, 0, 1).  % Функция вызывается с параметрами 0 и 1 для начала вычислений.

% Функция result/0 вычисляет 10000-е число Фибоначчи, выводит его и затраченное на вычисление время.
result() ->
  Start = os:timestamp(),  % Фиксируем начальное время выполнения.
  io:format("~w~n",[fib_p(45)]),  % Выводим 10000-е число Фибоначчи.
  io:format("total time taken ~f seconds~n", [timer:now_diff(os:timestamp(), Start) / 1000000]).  % Выводим затраченное время в секундах.