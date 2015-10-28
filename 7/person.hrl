%% Code from 
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-record(person, {name,age=0,phone="",address}).

birthday(#person{age=Age} = P) ->
    P#person{age=Age+1}.

joe() ->
    #person{name="Joe",
        age=21,
        phone="999-999"}.

% None of the functions are required to be modified after extending 
% persone with address attribute. We added address here only for 
% the sake of completeness.
showPerson(#person{age=Age,phone=Phone,name=Name,address=Address}) ->
    io:format("name: ~p  age: ~p  phone: ~p address: ~p~n", [Name,Age,Phone,Address]).
