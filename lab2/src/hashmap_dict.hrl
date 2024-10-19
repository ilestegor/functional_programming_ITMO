%%%-------------------------------------------------------------------
%%% @author ilestegor
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. Oct 2024 10:40 PM
%%%-------------------------------------------------------------------
-author("ilestegor").

-record(oahashdict, {
  table = [],
  size = 0,
  capacity = 10
}).