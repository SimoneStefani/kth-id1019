*Programming II (ID1019) - Royal Institute of Technology KTH*
# Huffman Encoding


### 1. Introduction
Huffman encoding is a way to assign binary codes to symbols that reduces the overall number of bits used to encode a typical string of those symbols. The Huffman coding scheme takes each symbol and its weight (or frequency of occurrence), and generates proper encodings so that higher weighted symbols have fewer bits in their encoding.

### 2. The Huffman tree

The first step to create an Huffman encoding tree is to collect the frequency distribution of each letter in a sample text.
The following method **`freq\1`** accepts a text like `“In a hole in the ground there lived a hobbit“` and returns a list of tuples of the form `{Char, Freq}` like `[{i, 4}, {n, 3}, ...]`.

```erlang
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
```

After computing the frequency of each term, the tree representation must be generated. The method **`huffman\1`** keeps the frequency list sorted by frequency ascending and recursively extract the two smallest elements, creates an internal node with these elements as children and frequency equal to the sum of their frequencies and add the node to the list. 
  
```erlang
huffman([{Tree, _} | []]) ->
  Tree;
huffman(List) ->
  [{C1, F1}, {C2, F2} | Rest] = lists:keysort(2, List),
  huffman([{{C1, C2}, F1 + F2} | Rest]).
```

When the process is completed a Huffman tree is returned in the form of nested tuples. The sentence *"In a hole in the ground there lived a hobbit"* is thus converted into the following structure where the numbers represents the ASCII values of the letters. The characters with the lowest frequency are the ones most deeply nested.

```erlang
{{{104,{98,110}},32},{{101,{111,105}},{{116,{114,100}},{{97,108},
{{117,118},{73,103}}}}}}
```

### 3. The encoding table

The Huffman encoding table is a list of tuples which connects a character with its corresponding binary code. To build such table, the Huffman tree is traversed collecting all the leaves and registering the path from the root as a sequence of ones and zeros (0 left branch, 1 right branch).

```erlang
encode_table(Tree) ->
  {L, R} = Tree,
  encode_table(L, [0]) ++ encode_table(R, [1]).
encode_table({L, R}, Bits) ->
  encode_table(L, [0 | Bits]) ++ encode_table(R, [1 | Bits]);
encode_table(Char, Bits) ->
  [{Char, lists:reverse(Bits)}].
```

The execution of the code above, on the previously encoded sentence, yelds the following result:

```erlang
[{104,[0,0,0]},{98,[0,0,1,0]},{110,[0,0,1,1]},{32,[0,1]},{101,[1,0,0]},
 {111,[1,0,1,0]},{105,[1,0,1,1]},{116,[1,1,0,0]},{114,[1,1,0,1,0]},
 {100,[1,1,0,1,1]},{97,[1,1,1,0,0]},{108,[1,1,1,0,1]},{117,[1,1,1,1,0,0]},
 {118,[1,1,1,1,0,1]},{73,[1,1,1,1,1,0]},{103,[1,1,1,1,1,1]}]
```

### 4. Encoding a message
In order to encode a sentence, one character at time is considered and translated to the Huffman binary code equivalent through the encoding table.

```erlang
encode(Text, Table) ->
  encode(Text, Table, []).
encode([], _, Result) ->
  Result;
encode([Char | Rest], Table, Result) ->
  {_, Val} = lists:keyfind(Char, 1, Table),
  encode(Rest, Table, lists:append(Result, Val)).
```

Using this function to encode the phrase "lighthearted union" gives the following (partial) binary sequence:
```erlang
[1,1,1,0,1,1,0,1,1,1,1,1,1,1,1,0,0,0,1,1,0,0,0,0,0,1,0,0,1|...]
```

The time complexity of this implementation of the function **`encode\2`** depends largely on the implementation of the Erlang methods **`keyfind\3`** and **`append\2`**. It has in any case a linear factor because every character needs to be translated.

### 5. Decoding a message

While decoding a message may seem more complicated because of the unknown length of each character, it is actually quite simple. In fact Huffman encoding builds a unique correspondence between the binary values and the letter they represent. Consequently it is sufficient to analyze a binary encoded string bit by bit until a matching character is found. Then continue searching for a new character.

```erlang
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
```