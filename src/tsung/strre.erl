-module(strre).
-purpose("string replace functionality").
-export([gsub/3,to_xml/1]).

gsub(S,Old,New) ->
    StrLen = string:len(S),
    OldLen = string:len(Old),
    Pos = string:str(S,Old),
    if
        Pos =:= 0 ->
            S;
        true ->
            Left = string:left(S,Pos-1),
            Right = string:right(S,StrLen-OldLen-Pos+1),
            string:concat(string:concat(Left,New),gsub(Right,Old,New))
    end.

to_xml(Str) ->
    gsub(gsub(gsub(Str,"\"","&quot;"), "<", "&lt;"), ">", "&gt;").
