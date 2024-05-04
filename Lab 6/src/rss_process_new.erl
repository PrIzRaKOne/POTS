%%Этот код реализует Erlang-процесс, который является сервером общего назначения (gen_server) для обработки RSS-лент.

%init/1: Функция инициализации, вызываемая при старте процесса. Устанавливает флаг trap_exit в true,
% чтобы процесс мог обрабатывать сообщения EXIT от других процессов, и возвращает кортеж {ok, #systemP{}}, указывая, что инициализация прошла успешно.

%%handle_call/3: Функция обработки вызова. Если приходит вызов с параметром {start_server, QState},
% процесс создает серверы очереди (rss_queue_new) и чтения (rss_reader_new).
% В ответ на вызов возвращается кортеж {reply, {ReqState, Result}, State}, где ReqState - состояние запроса, Result - результат выполнения, и State - текущее состояние процесса.

%%handle_cast/2: Функция обработки сообщения-каста. Просто выводит значение Value и возвращает {noreply, State}, не изменяя состояние процесса.

%%handle_info/2: Функция обработки информационных сообщений. Если приходит сообщение о завершении другого процесса ({'EXIT', PID, _Reason}),
% происходит запись сообщения об ошибке в журнал и возвращается {noreply, State}, не изменяя состояние процесса.

%%terminate/2: Функция завершения. Вызывается при завершении процесса. Выводит сообщение о завершении процесса в журнал и возвращает ok.

%%code_change/3: Функция изменения кода. Вызывается при изменении кода процесса во время его работы. Просто возвращает новое состояние процесса.


% Определение модуля с именем rss_process_new.
-module(rss_process_new).
% Включение файла заголовка xmerl.hrl.
-include("C:/Program Files/Erlang OTP/lib/xmerl-1.3.34/include/xmerl.hrl").
% Включение файла заголовка httpd.hrl из пакета inets.
-include("C:/Program Files/Erlang OTP/lib/inets-9.1/include/httpd.hrl").
% Включение файла заголовка logging.hrl.
-include("logging.hrl").
% Определение макроса RECEIVE_INTERVAL с значением 10000.
-define(RECEIVE_INTERVAL, 10000).
% Определение макроса TIMEOUT с значением 10000.
-define(TIMEOUT, 10000).
% Импорт функций из модуля rss_queue_new.
-import(rss_queue_new,[start/1, server/1, get_record_state/2, add_feed/2, add_item/2, get_all/1]).
% Импорт функций из модуля rss_parse.
-import(rss_parse,[is_rss2_feed/1, get_feed_items/2, get_item_time/1, compare_feed_items/2, display_items/1]).
% Объявление поведения gen_server.
-behaviour(gen_server).
% Определение записи systemP без полей.
-record(systemP,{}).

%% API
% Экспорт функций инициализации, обработчиков вызовов, обработчика cast'ов, обработчика информационных сообщений,завершения и изменения кода.
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
% Функция инициализации. Устанавливается флаг trap_exit в true, чтобы процесс ловил сообщения EXIT от других процессов.
init([]) -> process_flag(trap_exit,true), {ok, #systemP{}}.
% Функция обработки вызова. Если вызов с параметром {start_server, QState}, то создаются процессы rss_queue_new и rss_reader_new.
handle_call(_Request = {start_server, QState}, _From, State = #systemP{}) -> io:format("We are in process!~n"),
  {ReqState, Result} = gen_server:start_link(rss_queue_new, [], []),
  if
    ReqState == ok ->
      if
        QState == [] -> ?INFO("Starting base queue process ~p in system process.~n",[Result]), {reply, {ReqState, Result}, State};
        true -> [URL] = QState, ?INFO("Starting queue process ~p with URL ~p in system process.~n",[Result, URL]),
          {_WebState, _WebResult} = gen_server:start(rss_reader_new, [URL, Result], []), {reply, {ReqState, Result}, State}
      end;
    true -> {reply, {ReqState, Result}, State}
  end.
% Функция обработки cast'а. Просто выводит значение Value.
handle_cast(_Msg = Value, State) -> io:format("Some value in system process ~p~n",[Value]), {noreply, State}.
% Функция обработки информационных сообщений. Ловит сообщения о завершении других процессов и выводит информацию об этом.
handle_info(_Info = {'EXIT', PID, _Reason}, State) ->
  ?ERROR("Process ~p died from ~p with reason~n",[PID, self()]),
  {noreply, State}.
% Функция завершения. Выводит информацию о завершении процесса.
terminate(_Reason, _State) -> ?INFO("System process ~p is shutting down with reason.~n",[self()]), ok.
% Функция изменения кода. Просто возвращает новое состояние.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

