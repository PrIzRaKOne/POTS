-module(proc_sieve). % Определение модуля proc_sieve
%% API
-export([result/0, generate/1, sieve/1]). % Экспорт функций result/0, generate/1, sieve/1, делая их доступными извне модуля

sieve(State) -> % Определение функции sieve, которая принимает текущее состояние State
  {N, NextPid} = State, % Разбор текущего состояния

  receive % Получение сообщений
    Value when is_integer(Value) -> % Если получено целочисленное значение
      if % Условный оператор
        N == undefined -> % Если N не определено
          io:format("~w was set for thread ~w~n",[Value, self()]), % Вывод сообщения о назначении значения
          NewN = Value, % Новое значение N
          NewState = {NewN, NextPid}, % Новое состояние
          sieve(NewState); % Рекурсивный вызов функции sieve с новым состоянием
        true -> % Иначе
          Remainder = Value rem N, % Остаток от деления Value на N
          if % Вложенный условный оператор
            Remainder == 0 -> % Если остаток равен 0
              io:format("~w was passed~n",[Value]), % Вывод сообщения о передаче числа
              sieve(State); % Рекурсивный вызов функции sieve с текущим состоянием
            true -> % Иначе
              if % Вложенный условный оператор
                NextPid == undefined -> % Если следующий процесс не определен
                  InitialState = {undefined, undefined}, % Начальное состояние
                  NewNextPid = spawn(proc_sieve, sieve, [InitialState]), % Создание нового процесса
                  NewNextPid ! Value, % Передача значения новому процессу
                  io:format("~w created next thread with pid ~w~n",[Value, NewNextPid]), % Вывод сообщения о создании нового процесса
                  NewState = {N, NewNextPid}, % Новое состояние
                  sieve(NewState); % Рекурсивный вызов функции sieve с новым состоянием
                true -> % Иначе
                  NextPid ! Value, % Передача значения следующему процессу
                  io:format("~w was sent to next thread with pid ~w~n",[Value, NextPid]), % Вывод сообщения о передаче значения
                  sieve(State) % Рекурсивный вызов функции sieve с текущим состоянием
              end
          end
      end;
    {done, RepId} -> % Если получено сообщение о завершении
      if % Условный оператор
        NextPid == undefined -> % Если следующий процесс не определен
          RepId ! [N], % Передача N
          io:format("Process ~w was killed for last process~n",[self()]), % Вывод сообщения о завершении работы
          exit(normal); % Выход из процесса
        true -> % Иначе
          NextPid ! {done, self()}, % Передача сигнала следующему процессу
          receive % Ожидание сообщения
            Value when is_list(Value) -> % Если получен список
              RepId ! [N|Value], % Передача списка
              io:format("Process ~w was killed for medium process~n",[self()]), % Вывод сообщения о завершении работы
              exit(normal) % Выход из процесса
          end
      end
  end.

generate(MaxN) -> % Определение функции generate, которая запускает процесс решета Эратосфена
  List = lists:seq(2, MaxN), % Генерация списка чисел от 2 до MaxN
  InitialState = {undefined, undefined}, % Начальное состояние
  BasePid = spawn(proc_sieve, sieve, [InitialState]), % Запуск процесса
  lists:foreach(fun(X) -> BasePid ! X end, List), % Передача списка чисел процессу
  BasePid ! {done, self()}, % Передача сигнала о завершении процессу
  receive % Ожидание результата
    Value -> Value % Возвращение результата
  end.

result() -> io:format("~w~n", [generate(10)]). % Определение функции result, выводящей результат выполнения функции generate с аргументом 10

