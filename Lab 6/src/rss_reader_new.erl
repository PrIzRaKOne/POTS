-module(rss_reader_new).
% Определение модуля с именем rss_reader_new.

-include("C:/Program Files/Erlang OTP/lib/xmerl-1.3.34/include/xmerl.hrl").
% Включение файла заголовка xmerl.hrl.

-include("C:/Program Files/Erlang OTP/lib/inets-9.1/include/httpd.hrl").
% Включение файла заголовка httpd.hrl из пакета inets.

-include("logging.hrl").
% Включение файла заголовка logging.hrl.

-define(RECEIVE_INTERVAL, 10000).
% Определение макроса RECEIVE_INTERVAL с значением 10000.

-define(TIMEOUT, 10000).
% Определение макроса TIMEOUT с значением 10000.

-import(rss_queue_new,[start/1, server/1, get_record_state/2, add_feed/2, add_item/2, get_all/1, test_item/0]).
% Импорт функций из модуля rss_queue_new.

-import(rss_parse,[is_rss2_feed/1, get_feed_items/2, get_item_time/1, compare_feed_items/2, display_items/1]).
% Импорт функций из модуля rss_parse.

-behaviour(gen_server).
% Объявление поведения gen_server.

-record(rssRead,{url,qPid}).
% Определение записи rssRead с полями url и qPid.

%% API
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
% Экспорт функций для взаимодействия с модулем.

init([URL, QPid]) -> link(QPid), erlang:send_after(?RECEIVE_INTERVAL, self(), trigger), {ok, #rssRead{url = URL, qPid = QPid}}.
% Инициализация процесса. Устанавливает связь с QPid и отправляет себе сообщение trigger.

handle_call(_Request, _From, State) -> {reply, todo_reply, State}.
% Обработчик синхронных вызовов. Отправляет ответ todo_reply.

handle_cast(_Msg, State) -> {noreply, State}.
% Обработчик асинхронных сообщений. Не возвращает ответа.

handle_info(_Info=trigger, State=#rssRead{url = URL, qPid = QPid}) ->
% Обработчик информационных сообщений. При получении сообщения trigger выполняет запрос RSS-ленты.
  try
    {Status, Result} = httpc:request(URL), io:format("We are doing good!"),
    if
      Status == ok ->
        {StatusLine, _Header, Response} = Result,
        {_Version, Code, Deciphered} = StatusLine,
        if
          Code == 200 ->
            {Feed, _} = xmerl_scan:string(Response),
            Valid = is_rss2_feed(Feed),
            if
              Valid == true ->
                FeedItems = get_feed_items(Feed, []),
                add_feed(QPid, FeedItems),
                erlang:send_after(?RECEIVE_INTERVAL, self(), trigger),
                {noreply, State}
            end;
          true -> throw({connect_error, Deciphered})
        end;
      true -> throw({httpc_error, Result})
    end
  catch
    throw:{httpc_error, _Msg} ->
      ?ERROR("Error connecting to resource ~p:.~nThe web server will now exit...~n",[URL]),
      {stop,httpc_error,State};
    throw:{connect_error, _Msg} ->
      ?ERROR("Error getting response from resource ~p:.~nThe web server will now exit...~n",[URL]),
      {stop,connect_error,State};
    throw:validation_error ->
      ?ERROR("The RSS feed received from ~p is not a valid RSS 2.0 feed.~nThe web server will now exit...",[URL]),
      {stop,validation_error,State}
  end.

terminate(_Reason, _State) -> ?INFO("Reader process ~p is shutting down with reason.~n",[self()]), ok.
% Функция завершения процесса. Выводит сообщение о завершении.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
% Функция изменения кода процесса. Просто возвращает новое состояние.
