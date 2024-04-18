-module(main).
-export([start/0]).

output_path() -> 
    "outputs.txt".

measurement_path() -> 
    "measurements.txt".

-record(status, {min, max, total, count}).

solution() ->
    {ok, File} = file:open(measurement_path(), [read]),
    Map = read_lines(File, #{}),
    MapToSort = maps:to_list(Map),
    SortedMap = lists:keysort(1, MapToSort),
    Bucket = lists:foldl(fun format_line/2, "", SortedMap),
    Bucket.

read_lines(File, Map) ->
    case io:get_line(File, '') of
        eof  -> 
            Map;
        Line -> 
            NewMap = process_line(Line, Map),
            read_lines(File, NewMap)
    end.

process_line(Line, Map) when Line =/= "" ->
    [CityName, MeasurementStr] = string:split(Line, ";", all),
    {Measurement,_} = string:to_integer(MeasurementStr),
    case maps:find(CityName, Map) of
        {ok, Status} ->
            Min = min(Status#status.min, Measurement),
            Max = max(Status#status.max, Measurement),
            Total = Status#status.total + Measurement,
            Count = Status#status.count + 1,
            Map#{ CityName => #status{min = Min, max = Max, total = Total, count = Count} };
        error ->
            Map#{ CityName => #status{min = Measurement, max = Measurement, total = Measurement, count = 1} }
    end;
process_line(_, Map) ->
    Map.

format_line({CityName, Status}, Acc) ->
    Avg = Status#status.total div Status#status.count,
    Line = io_lib:format("~s=~p;~p;~p(~p/~p)\n", [CityName, Status#status.min, Status#status.max, Avg, Status#status.total, Status#status.count]),
    Acc ++ Line.

start() -> 
    {ok, Binary} = file:read_file(output_path()),
    ExpectOutput = string:trim(binary_to_list(Binary)),
    Start = erlang:system_time(millisecond),
    Got = string:trim(solution()),
    End = erlang:system_time(millisecond),
    Elapsed = End - Start,
    io:format("Elapsed: ~p milliseconds~n", [Elapsed]),
    if 
        ExpectOutput =:= Got ->
            io:format("Test Passed~n");
        true ->
            io:format("Test Failed~nExpectOutput: ~s~nGot: ~s~n", [ExpectOutput, Got])
    end.