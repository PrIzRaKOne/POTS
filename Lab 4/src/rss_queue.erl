%%server/1: Процесс-сервер, управляющий очередью элементов RSS. Обрабатывает запросы на добавление элементов, получение всех элементов и другие сообщения.
%%get_record_state/2: Определяет состояние записи в очереди (существующая, обновленная или различная) на основе сравнения с другими записями.
%%start/0: Инициализирует процесс сервера, создавая пустой список элементов и запуская функцию сервера с этим списком.
%%add_item/2: Добавляет новый элемент в очередь, принимая PID процесса сервера и элемент, который нужно добавить.
%%add_feed/2: Добавляет список элементов в очередь, используя add_item/2 для каждого элемента.
%%get_all/1: Отправляет запрос на получение всех элементов из очереди и ожидает ответа, возвращая полученные элементы или сообщение об ошибке.
%%result/0: Запускает процесс сервера, сканирует файлы RSS, добавляет элементы в очередь, получает и отображает элементы из очереди, а затем завершает процесс сервера.

% Определение модуля с именем rss_queue.
-module(rss_queue).

% Включение заголовочного файла xmerl.hrl.
-include("C:/Program Files/Erlang OTP/lib/xmerl-1.3.34/include/xmerl.hrl").
% Включение заголовочного файла httpd.hrl.
-include("C:/Program Files/Erlang OTP/lib/inets-9.1/include/httpd.hrl").
% Определение макроса TIMEOUT с значением 10000.
-define(TIMEOUT,10000).
% Импорт функций из модуля rss_parse.
-import(rss_parse,[get_feed_items/2, get_item_time/1, compare_feed_items/2, display_items/1]).

%% API
% Экспорт функций для использования вне модуля.
-export([result/0, server/1, start/0, add_item/2, add_feed/2, get_all/1]).

% @doc Это процесс очереди, который хранит все RSSItems в списке для быстрого доступа.
% Он хранит все записи в порядке даты публикации, сортируя их после добавления нового элемента.
% Определение функции сервера с аргументом List.
server(List) ->
  % Прием сообщений.
  receive
    % Если получено сообщение о добавлении элемента.
    {add_item, RSSItem} ->
      if % Условный оператор.
        % Проверка, является ли RSSItem записью типа xmlElement.
        is_record(RSSItem, xmlElement) ->
          if % Внутренний условный оператор.
            % Если список пуст, добавить RSSItem в список и продолжить работу сервера.
            List == [] -> server([RSSItem]);
            % Иначе получить состояние записи и саму запись.
            true -> {State, Item} = get_record_state(List, RSSItem),
              % Определение функции для фильтрации элементов.
              FilterFunc = fun(Item1,Item2) -> get_item_time(Item1) < get_item_time(Item2) end,
              % Конструкция case для сопоставления с шаблонами.
              case State of
                % Если состояние - same, продолжить работу сервера с текущим списком.
                same -> server(List);
                % Если состояние - updated.
                updated ->
                  % Удаление предыдущего элемента из списка.
                  RemovedList = lists:delete(Item, List),
                  % Сортировка списка с учетом нового элемента.
                  NewList = lists:sort(FilterFunc, [RSSItem|RemovedList]),
                  % Запуск функции сервера с новым списком.
                  server(NewList);
                % Если состояние - different.
                different ->
                  % Сортировка списка с учетом нового элемента.
                  NewList = lists:sort(FilterFunc, [RSSItem|List]),
                  % Запуск функции сервера с новым списком.
                  server(NewList)
              end
          end;
        % Если RSSItem не является записью xmlElement, продолжить работу сервера с текущим списком.
        true -> server(List)
      end;
    % Если получено сообщение о получении всех элементов, отправить список и продолжить работу сервера.
    {get_all, ReqID} -> ReqID ! List, server(List);
    % Для любых других значений, вывести "Some value" и само значение.
    _Value -> io:format("Some value ~p~n",[_Value])
  end.
% Определение функции get_record_state с аргументами List и RSSItem.
get_record_state(List, RSSItem) ->
  % Фильтрация списка для поиска элементов с совпадающими записями.
  SameList = lists:filter(fun(Elem) -> compare_feed_items(Elem,RSSItem) == same end, List),
  % Фильтрация списка для поиска обновленных элементов.
  UpdatedList = lists:filter(fun(Elem) -> compare_feed_items(Elem,RSSItem) == updated end, List),
  if % Условный оператор.
    % Если найдены совпадающие элементы, вернуть первый из них.
    length(SameList) > 0 -> {same, lists:nth(1,SameList)};
    true -> if % В противном случае.
              % Если найдены обновленные элементы, вернуть первый из них.
              length(UpdatedList) > 0 -> {updated, lists:nth(1,UpdatedList)};
              % Иначе вернуть состояние "different".
              true -> {different, undefined}
            end
  end.
% Определение функции start.
start() ->
  % Создание пустого списка.
  Q=[],
  % Запуск сервера с передачей ему пустого списка.
  spawn(?MODULE,server,[Q]).
% Функция добавления элемента в очередь.
add_item(QPid, Item) -> QPid ! {add_item, Item}, ok.

% Определение функции add_feed с аргументами QPid и Feed.
add_feed(QPid, Feed) ->
  % Перебор элементов списка.
  lists:foreach(
    % Для каждого элемента списка выполнить функцию добавления в очередь.
    fun(Item) -> add_item(QPid, Item) end,
    % Перебираемый список.
    Feed
  ),
  ok. % Возврат "ok".

% Определение функции get_all с аргументом QPid.
get_all(QPid) ->
  % Отправка запроса на получение всех элементов очереди.
  QPid ! {get_all, self()},
  receive % Получение ответа.
    % Если получен ответ, вернуть его.
    Value -> {ok, Value}
  % Если время ожидания истекло, вернуть ошибку timeout.
  after ?TIMEOUT -> {error, timeout}
  end.
% Определение функции result.
result() ->
  % Запуск сервера и сохранение PID.
  PID = start(),
  % Сканирование файла RSS и получение данных.
  {Feed1,_} = xmerl_scan:file("digg-science-rss1.xml"),
  % Сканирование файла RSS и получение данных.
  {Feed2,_} = xmerl_scan:file("digg-science-rss2.xml"),
  % Получение элементов из данных RSS.
  Feed1Items = get_feed_items(Feed1, []),
  % Получение элементов из данных RSS.
  Feed2Items = get_feed_items(Feed2, []),
  % Добавление элементов первого RSS в очередь.
  add_feed(PID, Feed1Items),
  % Получение всех элементов из очереди.
  {State, List} = get_all(PID),
  if % Условный оператор.
    % Если состояние - ok, отобразить элементы.
    State == ok -> display_items(List);
    % Иначе вывести сообщение об ошибке.
    true -> io:format("Error obtaining items list")
  end,
  % Добавление элементов второго RSS в очередь.
  add_feed(PID, Feed2Items),
  % Получение всех элементов из очереди.
  {NewState, NewList} = get_all(PID),
  if % Условный оператор.
  % Если состояние - ok, отобразить элементы.
    NewState == ok -> display_items(NewList);
  % Иначе вывести сообщение об ошибке.
    true -> io:format("Error obtaining second items list")
  end,
  % Завершить процесс с указанным PID.
  exit(PID, normal).
