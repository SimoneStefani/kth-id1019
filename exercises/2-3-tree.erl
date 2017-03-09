%%%-------------------------------------------------------------------
%%% @author Simone Stefani
%%% @copyright (C) 2017, simonestefani.me
%%% Created : 9. Mar 2017 18:36
%%%-------------------------------------------------------------------
-module(introduction).

-compile(export_all).


%%%-------------------------------------------------------------------
%%% 2-3 Tree
%%%-------------------------------------------------------------------

% the empty tree: nil
% a leaf: {leaf, Key, Value}
% a two-node: {two, Key, Left, Right}
% a three-node: {three, K1, K2, Left, Middle, Right}

% Temporary:
% a four-node: {four, K1, K2, K3, Left, M1, M2, Right}


%%%-------------------------------------------------------------------
%%% Insertion
%%%-------------------------------------------------------------------

insert(Key, Value, nil) -> {leaf, Key, Value};

insert(Key, Value, {leaf, K, _}=L) ->
  if
    Key <= K -> {two, Key, {leaf, Key, Value}, L};
    true -> {two, K, L, {leaf, Key, Value}};
  end.

