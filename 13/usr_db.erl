%% Code from 
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

%%% File    : usr_db.erl
%%% Description : Database API for subscriber DB

-module(usr_db).
-include("usr.hrl").
-include_lib("stdlib/include/ms_transform.hrl").
-export([create_tables/1, close_tables/0, add_usr/1, update_usr/1, delete_usr/1,
        lookup_id/1, lookup_msisdn/1, restore_backup/0, delete_disabled/0]).

create_tables(_FileName) ->
    application:start(mnesia),
    mnesia:create_table(usr, [
            {attributes, record_info(fields, usr)},
            {index, [#usr.id]}]).

close_tables() ->
    mnesia:close_table(usr).

add_usr(Usr) ->
    update_usr(Usr).

update_usr(Usr) ->
    mnesia:dirty_write(Usr),
    ok.

lookup_id(CustId) ->
    case mnesia:dirty_index_read(usr, CustId, #usr.id) of
        [Usr] -> {ok, Usr};
        []    -> {error, instance}
    end.

%% NB this code omitted from the description in Chapter 10.

%% Delete a user, specified by their customer id. Returns
%% either `ok' or an `error' tuple with a reason, if either the 
%% lookup of the id fails, or the delete of the tuple.


delete_usr(CustId) ->
    case get_index(CustId) of
        {ok,PhoneNo} ->
            delete_usr(PhoneNo, CustId);
        {error, instance} ->
            {error, instance}
    end.

%% Delete a user, specified by their phone number and customer id. Returns
%% either `ok' or an `error' tuple with a reason.

delete_usr(PhoneNo, _CustId) ->
    mnesia:dirty_delete({usr, PhoneNo}),
    ok.


lookup_msisdn(PhoneNo) ->
    case mnesia:dirty_read({usr, PhoneNo}) of
        [Usr] -> {ok, Usr};
        []    -> {error, instance}
    end.

get_index(CustId) ->
    case mnesia:dirty_index_read(usr, CustId, #usr.id) of
        [#usr{msisdn=PhoneNo}] -> {ok, PhoneNo};
        []                 -> {error, instance}
    end.

restore_backup() ->
    mnesia:wait_for_tables([usr], 60000).

delete_disabled() ->
    MS = ets:fun2ms(fun(#usr{status=S,msisdn=P}) when S==disabled -> P end),
    PhoneNos = mnesia:dirty_select(usr, MS),
    lists:foreach(fun(PhoneNo) -> mnesia:dirty_delete({usr, PhoneNo}) end, PhoneNos),
    ok.

