-module(rss_queue_new).
% Определение модуля с именем rss_queue_new.

-include("C:/Program Files/Erlang OTP/lib/xmerl-1.3.34/include/xmerl.hrl").
% Включение файла заголовка xmerl.hrl.

-include("C:/Program Files/Erlang OTP/lib/inets-9.1/include/httpd.hrl").
% Включение файла заголовка httpd.hrl из пакета inets.

-include("logging.hrl").
% Включение файла заголовка logging.hrl.

-define(TIMEOUT,10000).
% Определение макроса TIMEOUT с значением 10000.

-import(rss_parse,[get_feed_items/2, get_item_time/1, compare_feed_items/2, display_items/1]).
% Импорт функций из модуля rss_parse.

%% API
-behaviour(gen_server).
% Объявление поведения gen_server.

-record(rssQ,{queue,subscribers}).
% Определение записи rssQ с полями queue и subscribers.

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3, get_all/1, ult_result/0, add_item/2, add_feed/2, test_item/0]).
% Экспорт функций инициализации, обработчиков вызовов, обработчика cast'ов, обработчика информационных сообщений,
% завершения, изменения кода, а также других функций: get_all, ult_result, add_item, add_feed, test_item.

init([]) -> process_flag(trap_exit, true), {ok, #rssQ{queue = [], subscribers = sets:new()}}.
% Функция инициализации. Устанавливает флаг trap_exit в true для обработки сообщений EXIT от других процессов.
% Возвращает кортеж {ok, #rssQ{queue = [], subscribers = sets:new()}}, указывая, что инициализация прошла успешно.

handle_call(_Request={get_all}, _From, State=#rssQ{queue = List}) -> {reply, List, State}.
% Функция обработки вызова. Если приходит вызов с параметром {get_all}, процесс возвращает текущий список очереди.

handle_cast(_Msg={add_item, RSSItem}, State=#rssQ{queue = List, subscribers = SubList}) ->
  if
    is_record(RSSItem, xmlElement) ->
      if
        List == [] -> NewList = [RSSItem], {noreply, State#rssQ{queue = NewList}};
        true -> {RState, Item} = get_record_state(List, RSSItem),
          FilterFunc = fun(Item1,Item2) -> get_item_time(Item1) < get_item_time(Item2) end,
          case RState of
            same -> {noreply, State};
            updated ->
              RemovedList = lists:delete(Item, List),
              NewList = lists:sort(FilterFunc, [RSSItem|RemovedList]),
              SubSet = sets:to_list(SubList),
              lists:foreach(fun(QPid) -> add_item(QPid, RSSItem) end, SubSet),
              {noreply, State#rssQ{queue = NewList}};
            different ->
              NewList = lists:sort(FilterFunc, [RSSItem|List]),
              SubSet = sets:to_list(SubList),
              lists:foreach(fun(QPid) -> add_item(QPid, RSSItem) end, SubSet),
              {noreply, State#rssQ{queue = NewList}}
          end
      end;
    true -> {noreply, State}
  end;
% Функция обработки cast'а. Если приходит сообщение о добавлении элемента в очередь, процесс добавляет элемент в очередь,
% обновляет список подписчиков и рассылает элементы подписчикам.

handle_cast(_Msg={subscribe, QPid}, State=#rssQ{queue = List, subscribers = SubList}) ->
  Validate = sets:is_element(QPid, SubList),
  if
    Validate == true -> {noreply, State};
    true ->
      NewSubList = sets:add_element(QPid, SubList),
      ?INFO("Queue ~p subscribed to base queue~n",[QPid]),
      monitor(process,QPid),
      add_feed(QPid, List),
      {noreply, State#rssQ{subscribers = NewSubList}}
  end;
% Функция обработки cast'а. Если приходит сообщение о подписке на очередь, процесс добавляет подписчика и
% отправляет ему текущее содержимое очереди.

handle_cast(_Msg={unsubscribe, QPid}, State=#rssQ{subscribers = SubList}) ->
  Validate = sets:is_element(QPid, SubList),
  if
    Validate == false -> {noreply, State};
    true ->
      NewSubList = sets:del_element(QPid, SubList),
      ?INFO("Queue ~p unsubscribed from base queue~n",[QPid]),
      {noreply, State#rssQ{subscribers = NewSubList}}
  end.
% Функция обработки cast'а. Если приходит сообщение об отписке от очереди, процесс удаляет подписчика из списка.

handle_info(_Info={'DOWN', _Ref, process, QPid, _Reason}, State=#rssQ{subscribers = SubList}) ->
  ?WARN("Base queue terminated with reason",[]),
  NewSubList = sets:del_element(QPid,SubList),
  {noreply, State#rssQ{subscribers = NewSubList}};
% Функция обработки информационного сообщения. Если базовая очередь завершается, процесс удаляет ее из списка подписчиков.

handle_info(_Info={'EXIT', PID, Reason}, State=#rssQ{subscribers = SubList}) ->
  Validate = sets:is_element(PID, SubList),
  if Validate == true ->
    ?WARN("Base queue process terminated with reason.~nThe queue process ~p will now be terminating...~n",[self()]),
    NewSubList = sets:del_element(PID,SubList),
    {noreply, State#rssQ{subscribers = NewSubList}};
    true ->
      ?WARN("Process ~p terminated with reason.~nThe queue process ~p will now be terminating...~n",[PID, self()]),
      {stop, Reason}
  end.
% Функция обработки информационного сообщения. Если процесс очереди завершается, процесс удаляет его из списка подписчиков.

terminate(_Reason, _State) -> ?INFO("Queue process ~p is shutting down with reason.~n",[self()]), ok.
% Функция завершения. Выводит сообщение о завершении процесса и возвращает `ok`.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
% Функция изменения кода. Просто возвращает новое состояние процесса.

get_record_state(List, RSSItem) ->
  SameList = lists:filter(fun(Elem) -> compare_feed_items(Elem,RSSItem) == same end, List),
  UpdatedList = lists:filter(fun(Elem) -> compare_feed_items(Elem,RSSItem) == updated end, List),
  if
    length(SameList) > 0 -> {same, lists:nth(1,SameList)};
    true -> if
              length(UpdatedList) > 0 -> {updated, lists:nth(1,UpdatedList)};
              true -> {different, undefined}
            end
  end.
% Вспомогательная функция для определения состояния элемента в списке очереди.

add_item(QPid, Item) -> gen_server:cast(QPid,{add_item, Item}).
% Функция добавления элемента в очередь другого процесса.

add_feed(QPid, Feed) -> lists:foreach(fun(Item) -> add_item(QPid, Item) end, Feed), ok.
% Функция добавления всех элементов очереди другого процесса.

test_item() -> io:format("This is just a test").
% Тестовая функция.

get_all(QPid) -> gen_server:call(QPid ,{get_all}).
% Функция для получения текущего состояния очереди.

loop(BasePID) ->
  receive after 5000 ->
    Result = get_all(BasePID),
    display_items(Result),
    loop(BasePID)
  end.
% Функция для циклического получения текущего состояния очереди и вывода элементов.

ult_result() -> inets:start(), ssl:start(),
  {SysState, Result} = gen_server:start(rss_process_new, [], []),
  if
    SysState == ok ->
      io:format("System process ~p started ok right now~n",[Result]),
      {StatusCNN, PidCNN} = gen_server:call(Result,{start_server, ["http://rss.cnn.com/rss/cnn_topstories.rss"]}, ?TIMEOUT),
      {StatusNYT, PidNYT} = gen_server:call(Result,{start_server, ["https://rss.nytimes.com/services/xml/rss/nyt/World.xml"]}, ?TIMEOUT),
      {StatusYahoo, PidYahoo} = gen_server:call(Result,{start_server, ["https://www.yahoo.com/news/rss"]}, ?TIMEOUT),
      {StatusBase, BasePID} = gen_server:call(Result,{start_server, []}, ?TIMEOUT),
      if
        StatusCNN == error -> ?ERROR("Error creating CNN process~n", []);
        true ->
          if
            StatusNYT == error -> ?ERROR("Error creating New York Times process~n", []);
            true ->
              if
                StatusYahoo == error -> ?ERROR("Error creating Yahoo process~n", []);
                true ->
                  if
                    StatusBase == error -> ?ERROR("Error creating base process~n", []);
                    true ->
                      ok = gen_server:cast(PidCNN,{subscribe, BasePID}),
                      ok = gen_server:cast(PidNYT,{subscribe, BasePID}),
                      ok = gen_server:cast(PidYahoo,{subscribe, BasePID}),
                      loop(BasePID)
                  end
              end
          end
      end;
    true -> io:format("System process ~p failed to start~n",[Result])
  end.
% Функция для запуска процесса системы и его подписки на несколько RSS-лент.
