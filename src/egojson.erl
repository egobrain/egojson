-module(egojson).

-export([decode/1]).

decode(Bin) ->
    parse_value(
      Bin, 0,
      fun(Data, <<>>, _Pos) ->
              {ok, Data};
         (_Data, _Rest, Pos) ->
              {error, {Pos, invalid_json}}
      end).

parse_string(Bin, Pos, Next) ->
    parse_string(Bin, <<>>, Pos, Next).

parse_string(<<>>, _Acc, Pos, _Next) ->
    {error, {Pos+1, invalid_json}};
parse_string(<<$\\, Ch, Rest/binary>>, Acc, Pos, Next) ->
    case Ch of
        $" -> parse_string(Rest, <<Acc/binary, Ch>>, Pos+2, Next);
        $\\ -> parse_string(Rest, <<Acc/binary, Ch>>, Pos+2, Next);
        $/ -> parse_string(Rest, <<Acc/binary, Ch>>, Pos+2, Next);
        $b -> parse_string(Rest, <<Acc/binary, $\b>>, Pos+2, Next);
        $f -> parse_string(Rest, <<Acc/binary, $\f>>, Pos+2, Next);
        $n -> parse_string(Rest, <<Acc/binary, $\n>>, Pos+2, Next);
        $r -> parse_string(Rest, <<Acc/binary, $\r>>, Pos+2, Next);
        $t -> parse_string(Rest, <<Acc/binary, $\t>>, Pos+2, Next);
        $u ->
            parse_hexadecimal(
              Rest, Pos+2,
              fun(Symbol, Rest2, Pos2) ->
                      parse_string(Rest2, <<Acc/binary, Symbol/binary>>, Pos2, Next)
              end);
        _ ->
            {error, {Pos+2, invalid_json}}
    end;
parse_string(<<$", Rest/binary>>, Acc, Pos, Next) ->
    Next(Acc, Rest, Pos+1);
parse_string(<<Ch, Rest/binary>>, Acc, Pos, Next) ->
    parse_string(Rest, <<Acc/binary, Ch>>, Pos+1, Next).

parse_hexadecimal(Bin, Pos0, Next) ->
    parse_hex(
      Bin, 0, Pos0,
      fun(H1, Rest, Pos) ->
              parse_hex(
                Rest, H1, Pos,
                fun(H2, Rest2, Pos2) ->
                        parse_hex(
                          Rest2, H2, Pos2,
                          fun(H3, Rest3, Pos3) ->
                                  parse_hex(
                                    Rest3, H3, Pos3,
                                    fun(H4, Rest4, Pos4) ->
                                            io:format("~p", [H4]),
                                            Next(unicode:characters_to_binary([H4]) , Rest4, Pos4)
                                    end)
                          end)
                end)
      end).

parse_hex(<<Ch, Rest/binary>>, Acc, Pos, Next) when Ch >= $0 andalso Ch =< $9 ->
    Next(Acc * 16 + Ch - $0, Rest, Pos+1);
parse_hex(<<Ch, Rest/binary>>, Acc, Pos, Next) when Ch >= $a andalso Ch =< $f ->
    Next(Acc * 16 + Ch - $a + 10, Rest, Pos+1);
parse_hex(<<Ch, Rest/binary>>, Acc, Pos, Next) when Ch >= $A andalso Ch =< $F ->
    Next(Acc * 16 + Ch - $A + 10, Rest, Pos+1);
parse_hex(_Bin, Pos, _Acc, _Next) ->
    {error, {Pos+1, invalid_json}}.

white_space(<<Ch, Rest/binary>>, Pos, Next) when Ch =:= $  orelse Ch =:= $\n ->
    white_space(Rest, Pos+1, Next);
white_space(Bin, Pos, Next) ->
    Next(Bin, Pos).

parse_sign(<<$-, Rest/binary>>, Pos, Next) ->
    Next(-1, Rest, Pos+1);
parse_sign(<<$+, Rest/binary>>, Pos, Next) ->
    Next(1, Rest, Pos+1);
parse_sign(Bin, Pos, Next) ->
    Next(1, Bin, Pos).

parse_integer_digits(Bin, Pos, Next) ->
    parse_integer_digits(Bin, 0, false, Pos, Next).
parse_integer_digits(<<Ch, Rest/binary>>, Acc, _Inited, Pos, Next) when Ch >= $0 andalso Ch =< $9 ->
    parse_integer_digits(Rest, Acc*10+Ch-$0, true, Pos+1, Next);
parse_integer_digits(_Bin, _Acc, false, Pos, _Next) ->
    {error, {Pos+1, invalid_json}};
parse_integer_digits(Bin, Acc, true, Pos, Next) ->
    Next(Acc, Bin, Pos).

parse_fractional_digits(<<$., Bin/binary>>, Pos, Next) ->
    parse_fractional_digits(Bin, 0, 0.1, false, Pos+1, Next);
parse_fractional_digits(Bin, Pos, Next) ->
    Next(0, Bin, Pos).
parse_fractional_digits(<<Ch, Rest/binary>>, Acc, K, _Inited, Pos, Next) when Ch >= $0 andalso Ch =< $9 ->
    parse_fractional_digits(Rest, Acc+(Ch-$0)*K, K/10, true, Pos+1, Next);
parse_fractional_digits(_Bin, _Acc, _K, false, Pos, _Next) ->
    {error, {Pos+1, invalid_json}};
parse_fractional_digits(Bin, Acc, _K, true, Pos, Next) ->
    Next(Acc, Bin, Pos).

parse_unsigned_number_part(Bin, Pos, Next) ->
    parse_integer_digits(
      Bin, Pos,
      fun(Integer, Rest2, Pos2) ->
              parse_fractional_digits(
                Rest2, Pos2,
                fun(Fraction, Rest3, Pos3) ->
                        parse_e_number_part(
                          Rest3, Pos3,
                          fun(E, Rest4, Pos4) ->
                                  Next((Integer+Fraction)*E, Rest4, Pos4)
                          end)
                end)
      end).

parse_e_number_part(<<Ch, Rest/binary>>, Pos, Next)  when Ch =:= $e orelse Ch =:= $E ->
    parse_sign(
      Rest, Pos+1,
      fun(Sign, Rest2, Pos2) ->
              parse_integer_digits(
                Rest2, Pos2,
                fun(E, Rest3, Pos3) ->
                        Next(math:pow(10, Sign*E), Rest3, Pos3)
                end)
      end);
parse_e_number_part(Bin, Pos, Next) ->
    Next(1, Bin, Pos).

parse_value(Bin, Pos, Next) ->
    white_space(
      Bin, Pos,
      fun(Rest, Pos2) ->
              parse_value_(
                Rest, Pos2,
                fun(Value, Rest2, Pos3) ->
                        white_space(
                          Rest2, Pos3,
                          fun(Rest3, Pos4) ->
                                  Next(Value, Rest3, Pos4)
                          end)
                end)
      end).
parse_value_(<<${, Rest/binary>>, Pos, Next) ->
    parse_object(Rest, Pos+1, Next);
parse_value_(<<$[, Rest/binary>>, Pos, Next) ->
    parse_array(Rest, Pos+1, Next);
parse_value_(<<$", Rest/binary>>, Pos, Next) ->
    parse_string(Rest, Pos+1, Next);
parse_value_(<<$-, Rest/binary>>, Pos, Next) ->
    parse_unsigned_number_part(
      Rest, Pos+1,
      fun(Number, Rest2, Pos2) ->
              Next(-1*Number, Rest2, Pos2)
      end);
parse_value_(<<Ch, _Rest/binary>> = Data, Pos, Next) when Ch >= $0 andalso Ch =< $9 ->
    parse_unsigned_number_part(Data, Pos, Next);
parse_value_(<<"null", Rest/binary>>, Pos, Next) ->
    Next(null, Rest, Pos+4);
parse_value_(<<"true", Rest/binary>>, Pos, Next) ->
    Next(true, Rest, Pos+4);
parse_value_(<<"false", Rest/binary>>, Pos, Next) ->
    Next(false, Rest, Pos+5);
parse_value_(_Bin, Pos, _Next) ->
    {error, {Pos+1, invalid_json}}.

parse_object(Bin, Pos, Next) ->
    parse_object_field(Bin, Pos, {[]}, Next).

parse_object_field(Bin, Pos, Obj, Next) ->
    white_space(
      Bin, Pos,
      fun(Rest, Pos2) ->
              parse_object_field_(Rest, Pos2, Obj, Next)
      end).
parse_object_field_(<<$}, Rest/binary>>, Pos, {ObjList}, Next) ->
    Next({lists:reverse(ObjList)}, Rest, Pos+1);
parse_object_field_(Bin, Pos0, {ObjList}, Next) ->
    parse_object_field__(Bin, Pos0, {ObjList}, Next).
parse_object_field__(Bin, Pos0, {ObjList}, Next) ->
    parse_key(
      Bin, Pos0,
      fun(Key, <<$:, Rest1/binary>>, Pos1) ->
              parse_value(
                Rest1, Pos1+1,
                fun(Value, Rest2, Pos2) ->
                        Obj2 = {[{Key, Value}|ObjList]},
                        case Rest2 of
                            <<$,, Rest3/binary>> ->
                                parse_object_field__(Rest3, Pos2+1, Obj2, Next);
                            _ ->
                                parse_object_field(Rest2, Pos2, Obj2, Next)
                        end
                end);
         (Key, <<$,, Rest1/binary>>, Pos1) ->
              Obj2 = {[{Key, true}|ObjList]},
              parse_object_field__(Rest1, Pos1+1, Obj2, Next);
         (Key, Rest1, Pos1) ->
              Obj2 = {[{Key, true}|ObjList]},
              parse_object_field_(Rest1, Pos1, Obj2, Next)
      end).

parse_key(Bin, Pos, Next) ->
    white_space(
      Bin, Pos,
      fun(Rest2, Pos2) ->
              parse_key_(
                Rest2, Pos2,
                fun(Key, Rest3, Pos3) ->
                        white_space(
                          Rest3, Pos3,
                          fun(Rest4, Pos4) ->
                                  Next(Key, Rest4, Pos4)
                          end)
                end)
      end).
parse_key_(<<$", Rest/binary>>, Pos, Next) ->
    parse_string(Rest, Pos+1, Next);
parse_key_(_Bin, Pos, _Next) ->
    {error, {Pos+1, invalid_json}}.

parse_array(Bin, Pos, Next) ->
    parse_array_item(Bin, Pos, [], Next).

parse_array_item(Bin, Pos, Arr, Next) ->
    white_space(
      Bin, Pos,
      fun(Rest, Pos2) ->
              parse_array_item_(
                Rest, Pos2, Arr, Next)
      end).
parse_array_item_(<<$], Rest/binary>>, Pos, Arr, Next) ->
    Next(lists:reverse(Arr), Rest, Pos+1);
parse_array_item_(Bin, Pos, Arr, Next) ->
    parse_array_item__(Bin, Pos, Arr, Next).
parse_array_item__(Bin, Pos, Arr, Next) ->
    parse_value(
      Bin, Pos,
      fun(Value, Rest, Pos2) ->
              Arr2 = [Value|Arr],
              case Rest of
                  <<$,, Rest2/binary>> ->
                      parse_array_item__(Rest2, Pos2+1, Arr2, Next);
                  _ ->
                      parse_array_item(Rest, Pos2, Arr2, Next)
              end
      end).
