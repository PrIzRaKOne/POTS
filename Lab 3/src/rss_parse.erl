% Определение модуля с именем rss_parse.
-module(rss_parse).

% Включение заголовочного файла xmerl.hrl из библиотеки xmerl.
-include("C:/Program Files/Erlang OTP/lib/xmerl-1.3.34/include/xmerl.hrl").
% Включение заголовочного файла httpd.hrl из библиотеки inets.
-include("C:/Program Files/Erlang OTP/lib/inets-9.1/include/httpd.hrl").

%% API
% Экспорт функций result/0, is_rss2_feed/1, get_feed_items/2, get_item_time/1, compare_feed_items/2.
-export([result/0, is_rss2_feed/1, get_feed_items/2, get_item_time/1, compare_feed_items/2]).
% Определение функции is_rss2_feed с аргументом Data.
is_rss2_feed(Data) ->
  % Распаковка кортежа Data в переменные R и _.
  {R,_} = Data,
  % Условная конструкция if.
  if
    % Если R является записью xmlElement.
    is_record(R, xmlElement) ->
      % Определение анонимной функции Func, проверяющей версию RSS.
      Func = fun({_, Name, _, _, _, _, _, _, Value, _}) -> Name == version andalso Value == "2.0" end,
      % Фильтрация атрибутов записи R по заданной функции.
      Result = lists:filter(Func, R#xmlElement.attributes),
      % Вложенная условная конструкция if.
      if
        % Если длина списка Result равна 1, возвращаем true.
        length(Result) == 1 -> true;
        true -> false % Иначе возвращаем false.
      end;
    true -> false % Иначе возвращаем false.
  end.

% Определение функции get_feed_items с аргументами Record и List.
get_feed_items(Record, List) ->
  if % Условная конструкция if.
  % Если Record является записью xmlElement.
    is_record(Record, xmlElement) ->
      if % Вложенная условная конструкция if.
        % Если имя элемента записи Record равно "item", добавляем Record в список List.
        Record#xmlElement.name == item -> [Record|List];
        % Иначе рекурсивно вызываем функцию get_feed_items для содержимого элемента Record.
        true -> lists:foldl(fun get_feed_items/2, List, Record#xmlElement.content)
      end;
    true -> List % Иначе возвращаем список List.
  end.

% Определение функции get_item_string с аргументом Item.
get_item_string(Item) ->
  % Получение заголовка новости.
  Title = get_item_parameter(Item, title),
  % Получение ссылки новости.
  Link = get_item_parameter(Item, link),
  % Получение уникального идентификатора новости.
  GUID = get_item_parameter(Item, guid),
  % Получение времени публикации новости.
  Time = get_item_parameter(Item, pubDate),
  % Возвращаем кортеж с уникальным идентификатором, заголовком, ссылкой и временем публикации новости.
  {GUID, Title, Link, Time}.

% Определение функции get_items_string с аргументом Feed.
get_items_string(Feed) ->
  % Преобразование списка новостей в список кортежей с помощью функции get_item_string и списочной свёртки.
  lists:foldl(fun(Item,List) -> [get_item_string(Item)|List] end, [], Feed).

% Определение функции get_item_parameter с аргументами Item и Parameter.
get_item_parameter(Item, Parameter) ->
  % Фильтрация содержимого элемента Item по заданному условию.
  L = lists:filter(fun(Elem) -> element(1,Elem) == xmlElement andalso element(2,Elem) == Parameter end, Item#xmlElement.content),
  if  % Условная конструкция if.
    length(L) > 0 -> % Если длина списка L больше 0.
      Container = lists:nth(1,L), % Получаем первый элемент списка L.
      Text = lists:nth(1, Container#xmlElement.content), % Получаем текст элемента.
      Text#xmlText.value; % Возвращаем значение текста элемента.
    true -> false % Иначе возвращаем false.
  end.

% Определение функции get_item_time с аргументом Item.
get_item_time(Item) ->
  % Получение времени публикации новости.
  Value = get_item_parameter(Item, pubDate),
  if % Условная конструкция if.
    % Если значение равно false, возвращаем bad_date.
    Value == false -> bad_date;
    true -> % Иначе.
      % Преобразуем строку времени в формат даты.
      RequestDate = httpd_util:convert_request_date(Value),
      if % Вложенная условная конструкция if.
        % Если преобразование не удалось, возвращаем bad_date.
        RequestDate == bad_date -> RequestDate;
      % Иначе преобразуем дату в секунды.
        true -> calendar:datetime_to_gregorian_seconds(RequestDate)
      end
  end.

% Определение функции compare_feed_items с аргументами OldItem и NewItem.
compare_feed_items(OldItem, NewItem) ->
  % Получение строки из старой новости.
  OldString = get_item_string(OldItem),
  % Получение строки из новой новости.
  NewString = get_item_string(NewItem),
  if % Условная конструкция if.
    % Если строки совпадают, возвращаем same.
    OldString == NewString -> same;
    true -> % Иначе.
      % Получение уникального идентификатора старой новости.
      OldGUID = get_item_parameter(OldItem, guid),
      % Получение уникального идентификатора новой новости.
      NewGUID = get_item_parameter(NewItem, guid),
      % Проверка наличия уникальных идентификаторов.
      ValidGUID = OldGUID /= false andalso NewGUID /= false,
      if % Вложенная условная конструкция if.
      % Если оба идентификатора не равны false и совпадают, возвращаем updated.
        ValidGUID andalso OldGUID == NewGUID -> updated;
        true ->                     % Иначе.
          % Получение заголовка старой новости.
          OldTitle = get_item_parameter(OldItem, title),
          % Получение заголовка новой новости.
          NewTitle = get_item_parameter(NewItem, title),
          % Проверка наличия заголовков.
          ValidTitle = OldTitle /= false andalso NewTitle /= false,
          if % Вложенная условная конструкция if.
            % Если оба заголовка не равны false и совпадают, возвращаем updated.
            ValidTitle andalso OldTitle == NewTitle -> updated;
            true -> % Иначе.
              % Получение ссылки старой новости.
              OldLink = get_item_parameter(OldItem, link),
              % Получение ссылки новой новости.
              NewLink = get_item_parameter(NewItem, link),
              % Проверка наличия ссылок.
              ValidLink = OldLink /= false andalso NewLink /= false,
              if % Вложенная условная конструкция if.
              % Если обе ссылки не равны false и совпадают, возвращаем updated.
                ValidLink andalso OldLink == NewLink -> updated;
                true -> different % Иначе возвращаем different.
              end
          end
      end
  end.

% Определение функции build_unique_feed с аргументами Feed1Items и Feed2Items.
build_unique_feed(Feed1Items, Feed2Items) ->
  % Определение анонимной функции Func1 с аргументами Item и List.
  Func1 = fun(Item, List) ->
    % Фильтрация элементов Feed2Items по заданному условию.
    L = lists:filter(fun(Elem) -> compare_feed_items(Elem,Item) /= different end, Feed2Items),
    if % Условная конструкция if.
      length(L)>0 -> % Если длина списка L больше 0.
        [Item] ++ List; % Добавляем Item в начало списка List.
      true -> List % Иначе возвращаем List.
    end
          end,
  % Применяем функцию Func1 к элементам списка Feed1Items.
  L1 = lists:foldl(Func1,[],Feed1Items),
  % Определяем элементы, отсутствующие в списке L1.
  L1U = Feed1Items -- L1,
  % Сортируем объединенный список L1U и Feed2Items по времени публикации новости.
  lists:sort(fun(Item1,Item2) -> get_item_time(Item1) < get_item_time(Item2) end, L1U ++ Feed2Items).

result() ->                         % Определение функции result.
  % Сканирование файла "digg-science-rss1.xml" с помощью xmerl.
  RSS1 = xmerl_scan:file("digg-science-rss1.xml"),
  % Сканирование файла "digg-science-rss2.xml" с помощью xmerl.
  RSS2 = xmerl_scan:file("digg-science-rss2.xml"),
  % Проверка, является ли RSS1 RSS 2.0.
  Valid1 = is_rss2_feed(RSS1),
  % Проверка, является ли RSS2 RSS 2.0.
  Valid2 = is_rss2_feed(RSS2),
  % Вывод сообщения о том, являются ли оба потока данных допустимыми.
  io:format("Are both of the feeds valid? ~p and ~p~n", [Valid1, Valid2]),
  {Feed1,_} = RSS1,                 % Разбиение RSS1 на Feed1.
  {Feed2,_} = RSS2,                 % Разбиение RSS2 на Feed2.
  % Получение элементов новостной ленты из Feed1.
  Feed1Items = get_feed_items(Feed1, []),
  % Получение элементов новостной ленты из Feed2.
  Feed2Items = get_feed_items(Feed2, []),
  % Преобразование элементов новостной ленты из Feed1 в строки.
  DisplayFeed1 = get_items_string(Feed1Items),
  % Вывод первой новостной ленты.
  io:format("The first news feed (~p articles):~n~p~n~n~n~n~n~n~n~n~n~n",[length(DisplayFeed1), DisplayFeed1]),
  % Преобразование элементов новостной ленты из Feed2 в строки.
  DisplayFeed2 = get_items_string(Feed2Items),
  % Вывод второй новостной ленты.
  io:format("The second news feed (~p articles):~n~p~n~n~n~n~n~n~n~n~n~n",[length(DisplayFeed2), DisplayFeed2]),
  % Преобразование уникальных элементов новостной ленты в строки.
  DisplayUniqueFeed = get_items_string(build_unique_feed(Feed1Items, Feed2Items)),
  % Вывод уникальной новостной ленты.
  io:format("Unique news feed (~p articles):~n~p~n",[length(DisplayUniqueFeed), DisplayUniqueFeed]).
