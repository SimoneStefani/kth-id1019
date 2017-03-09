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

% a four-node: {four, K1, K2, K3, Left, M1, M2, Right}