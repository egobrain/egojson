-module(egojson).

-export([decode/1]).

-record(state, {
          pos = 0 :: non_neg_integer()
         }).

-define(INC_POS(S, A), S#state{pos=S#state.pos + A}).

decode(Bin) ->
    parse_value(
      Bin, #state{},
      fun(Data, <<>>, _State) ->
              {ok, Data};
         (_Data, _Rest, #state{pos=Pos}) ->
              {error, {Pos, invalid_json}}
      end).


parse_string(Bin, State, Next) ->
    parse_string(Bin, <<>>, State, Next).

parse_string(<<>>, _Acc, #state{pos=Pos}, _Next) ->
    {error, {Pos+1, trancated_json}};
parse_string(<<"\\\\", Rest/binary>>, Acc, State, Next) ->
    parse_string(Rest, <<Acc/binary, $\\>>, ?INC_POS(State, 2), Next);
parse_string(<<"\\\"", Rest/binary>>, Acc, State, Next) ->
    parse_string(Rest, <<Acc/binary, $">>, ?INC_POS(State, 2), Next);
parse_string(<<$", Rest/binary>>, Acc, State, Next) ->
    Next(Acc, Rest, ?INC_POS(State, 1));
parse_string(<<Ch, Rest/binary>>, Acc, State, Next) ->
    parse_string(Rest, <<Acc/binary, Ch>>, ?INC_POS(State, 1), Next).


%% white_space(<<$ , Rest/binary>>, State, Next) ->
%%     white_space(Rest, ?INC_POS(State, 1), Next);
%% white_space(Bin, State, Next) ->
%%     Next(Bin, State).

parse_sign(<<$-, Rest/binary>>, State, Next) ->
    Next(-1, Rest, ?INC_POS(State, 1));
parse_sign(<<$+, Rest/binary>>, State, Next) ->
    Next(1, Rest, ?INC_POS(State, 1));
parse_sign(Bin, State, Next) ->
    Next(1, Bin, State).

parse_integer_digits(Bin, State, Next) ->
    parse_integer_digits(Bin, 0, false, State, Next).
parse_integer_digits(<<Ch, Rest/binary>>, Acc, _Inited, State, Next) when Ch >= $0 andalso Ch =< $9 ->
    parse_integer_digits(Rest, Acc*10+Ch-$0, true, ?INC_POS(State, 1), Next);
parse_integer_digits(_Bin, _Acc, false, #state{pos=Pos}, _Next) ->
    {error, {Pos+1, trancated_json}};
parse_integer_digits(Bin, Acc, true, State, Next) ->
    Next(Acc, Bin, State).

parse_fractional_digits(Bin, State, Next) ->
    parse_fractional_digits(Bin, 0, 0.1, false, State, Next).
parse_fractional_digits(<<Ch, Rest/binary>>, Acc, K, _Inited, State, Next) when Ch >= $0 andalso Ch =< $9 ->
    parse_fractional_digits(Rest, Acc+(Ch-$0)*K, K/10, true, ?INC_POS(State, 1), Next);
parse_fractional_digits(_Bin, _Acc, _K, false, #state{pos=Pos}, _Next) ->
    {error, {Pos+1, trancated_json}};
parse_fractional_digits(Bin, Acc, _K, true, State, Next) ->
    Next(Acc, Bin, State).

parse_unsigned_number_part(Bin, State, Next) ->
    parse_integer_digits(
      Bin, State,
      fun(Integer, <<$., Rest2/binary>>, State2) ->
              parse_fractional_digits(
                Rest2, State2,
                fun(Fraction, <<Ch, Rest3/binary>>, State3) when Ch =:= $e orelse Ch =:= $E ->
                        parse_sign(
                          Rest3, State3,
                          fun(Sign, Rest4, State4) ->
                                  parse_integer_digits(
                                    Rest4, State4,
                                    fun(E, Rest5, State5) ->
                                            Next(Sign*(Integer+Fraction)*math:pow(0.1, E), Rest5, State5)
                                    end)
                          end);
                   (Fraction, Rest3, State3) ->
                        Next(Integer+Fraction, Rest3, State3)
                end);
         (Integer, Rest2, State2) ->
              Next(Integer, Rest2, State2)
      end).

parse_value(<<${, Rest/binary>>, State, Next) ->
    parse_object(Rest, ?INC_POS(State, 1), Next);
parse_value(<<$[, Rest/binary>>, State, Next) ->
    parse_array(Rest, ?INC_POS(State, 1), Next);
parse_value(<<$", Rest/binary>>, State, Next) ->
    parse_string(Rest, ?INC_POS(State, 1), Next);
parse_value(<<$-, Rest/binary>>, State, Next) ->
    parse_unsigned_number_part(
      Rest, ?INC_POS(State, 1),
      fun(Number, Rest2, State2) ->
              Next(-1*Number, Rest2, State2)
      end);
parse_value(<<Ch, _Rest/binary>> = Data, State, Next) when Ch >= $0 andalso Ch =< $9 ->
    parse_unsigned_number_part(Data, ?INC_POS(State, 1), Next);
parse_value(<<"null", Rest/binary>>, State, Next) ->
    Next(null, Rest, ?INC_POS(State, 4));
parse_value(<<"true", Rest/binary>>, State, Next) ->
    Next(true, Rest, ?INC_POS(State, 4));
parse_value(<<"false", Rest/binary>>, State, Next) ->
    Next(false, Rest, ?INC_POS(State, 5));
parse_value(_Bin, #state{pos=Pos}, _Next) ->
    {error, {Pos, invalid_json}}.

parse_object(Bin, State, Next) ->
    parse_object_field(Bin, State, new_object(), Next).
parse_object_field(Bin, State0, Obj, Next) ->
    parse_key(
      Bin, State0,
      fun(Key, <<$:, Rest1/binary>>, State1) ->
              parse_value(
                Rest1, State1,
                fun(Value, Rest2, State2) ->
                        Obj2 = append_object(Key, Value, Obj),
                        case Rest2 of
                            <<$,, Rest3/binary>> ->
                                parse_object_field(Rest3, ?INC_POS(State2, 1), Obj2, Next);
                            <<$}, Rest3/binary>> ->
                                Next(Obj2, Rest3, State2);
                            _ ->
                                {error, {State2#state.pos, invalid_json}}
                        end
                end);
         (Key, <<$,, Rest1/binary>>, State1) ->
              Obj2 = append_object(Key, true, Obj),
              parse_object_field(Rest1, ?INC_POS(State1, 1), Obj2, Next);
         (Key, <<$}, Rest1/binary>>, State1) ->
              Obj2 = append_object(Key, true, Obj),
              Next(Obj2, Rest1, ?INC_POS(State1, 1));
         (_Key, _Rest, #state{pos=Pos}) ->
              {error, {Pos, invalid_json}}
      end).

parse_key(<<$", Rest/binary>>, State, Next) ->
    parse_string(Rest, ?INC_POS(State, 1), Next);
parse_key(_Bin, #state{pos=Pos}, _Next) ->
    {error, {Pos, invalid_json}}.

parse_array(Bin, State, Next) ->
    parse_array_item(Bin, State, [], Next).

parse_array_item(Bin, State, Arr, Next) ->
    parse_value(
      Bin, State,
      fun(Value, Rest, State2) ->
              Arr2 = [Value|Arr],
              case Rest of
                  <<$,, Rest2/binary>> ->
                      parse_array_item(Rest2, ?INC_POS(State2, 1), Arr2, Next);
                  <<$], Rest2/binary>> ->
                      Next(Arr2, Rest2, ?INC_POS(State, 1));
                  _ ->
                      {error, {State2#state.pos, invalid_json}}
              end
      end).

%% =============================================================================
%%% Internal functions
%% =============================================================================

new_object() ->
    {[]}.

append_object(Key, Value, {List}) ->
    {[{Key, Value}|List]}.
