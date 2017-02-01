%%%-------------------------------------------------------------------
%%% @author Simone Stefani
%%% @copyright (C) 2017, simonestefani.me
%%% Created : 18. Jan 2017 17:51
%%%-------------------------------------------------------------------
-module(huffman).

-compile(export_all).

sample() ->
  "the quick brown fox jumps over the lazy dog
  this is a sample text that we will use when we build
  up a table we will only handle lower case letters and
  no punctuation symbols the frequency will of course not
  represent english but it is probably not that far off ".

text() ->
  "this is something that we should encode".


%%%-------------------------------------------------------------------
%%% Public API
%%%-------------------------------------------------------------------

test() ->
  Sample = sample(),
  Text = text(),
  test(Sample, Text).

test(Param1, Param2) ->
  case is_integer(Param2) of
    true ->
      Sample = read(Param1, Param2),
      test(Sample, Sample);
    false ->
      Tree = tree(Param1),
      Encode = encode_table(Tree),
%%      Decode = decode_table(Tree),
      Seq = encode(Param2, Encode),
      Param2 = decode(Seq, Tree)
  end.


% Generate a frequency list and build Huffman table
tree(_Sample) ->
  Freq = freq(_Sample),
  huffman(Freq).


% The tree is encoded as a list of nested tuple. The smallest elements occupy the
% innermost positions (lowest in the corresponding tree).
encode_table(Tree) ->
  {L, R} = Tree,
  encode_table(L, [0]) ++ encode_table(R, [1]).
encode_table({L, R}, Bits) ->
  encode_table(L, [0 | Bits]) ++ encode_table(R, [1 | Bits]);
encode_table(Char, Bits) ->
  [{Char, lists:reverse(Bits)}].


% IMP: copy of encode_table method!
decode_table(Tree) ->
  {L, R} = Tree,
  decode_table(L, [0]) ++ decode_table(R, [1]).
decode_table({L, R}, Bits) ->
  encode_table(L, [0 | Bits]) ++ encode_table(R, [1 | Bits]);
decode_table(Char, Bits) ->
  [{Char, lists:reverse(Bits)}].


% The complexity of the encode operation depends on the implementation of the standard
% Erlang methods "keyfind" and "append". It can be estimated in ~ N^2
encode(Text, Table) ->
  encode(Text, Table, []).
encode([], _, Result) ->
  Result;
encode([Char | Rest], Table, Result) ->
  {_, Val} = lists:keyfind(Char, 1, Table),
  encode(Rest, Table, lists:append(Result, Val)).


% Use tree structure to decode
decode(Seq, Tree) ->
  decode(Seq, Tree, Tree).
decode([0 | Seq], {Left, _}, Tree) ->
  decode(Seq, Left, Tree);
decode([1 | Seq], {_, Right}, Tree) ->
  decode(Seq, Right, Tree);
decode(Seq, Char, Tree) ->
  case Seq of
    [] -> [Char];
    _ -> [Char | decode(Seq, Tree, Tree)]
  end.

%%% Modified implementation of the suggested method for decoding. Returns a list of characters
%%% which represent the decoded text in form of string
%%decode(Seq, Table) ->
%%  decode(Seq, Table, []).
%%decode([], _Table, Text) ->
%%  lists:reverse(Text);
%%decode(Seq, Table, Text) ->
%%  {Char, Rest} = decode_char(Seq, 1, Table),
%%  decode(Rest, Table, [Char|Text]).
%%decode_char(Seq, N, Table) ->
%%  {Code, Rest} = lists:split(N, Seq),
%%  case lists:keyfind(Code, 2, Table) of
%%    {Char, _} -> {Char, Rest};
%%    false -> decode_char(Seq, N + 1, Table)
%%  end .


%%%-------------------------------------------------------------------
%%% Helper methods
%%%-------------------------------------------------------------------

% Generate frequency distribution list
freq(Sample) ->
  freq(Sample, []).
freq([], Freq) ->
  lists:keysort(2, Freq);
freq([Char | Rest], Freq) ->
  case lists:keyfind(Char, 1, Freq) of
    false ->
      freq(Rest, lists:append(Freq, [{Char, 1}]));
    {Char, Val} ->
      freq(Rest, lists:keyreplace(Char, 1, Freq, {Char, Val + 1}))
  end.

% Build Huffman tree
huffman([{Tree, _} | []]) ->
  Tree;
huffman(List) ->
  [{C1, F1}, {C2, F2} | Rest] = lists:keysort(2, List),
  huffman([{{C1, C2}, F1 + F2} | Rest]).


read(File, N) ->
  {ok, Fd} = file:open(File, [read, binary]),
  {ok, Binary} = file:read(Fd, N),
  file:close(Fd),
  case unicode:characters_to_list(Binary, utf8) of
    {incomplete, List, _} ->
      List;
    List ->
      List
  end.