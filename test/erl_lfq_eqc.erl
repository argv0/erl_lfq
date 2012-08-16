-module(erl_lfq_eqc).
-compile([export_all]).

-ifdef(TEST).
-ifdef(EQC).
-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_statem.hrl").

-include_lib("eunit/include/eunit.hrl").

lfq_eqc_test_() ->
    {timeout, 20,
        fun() ->
                ?assert(eqc:quickcheck(eqc:testing_time(4,
                            erl_lfq_eqc:prop_sequential())))
        end
    }.

-record(state, {
        model = [],
        pid,
        queue
        }).

initial_state() ->
    #state{}.

producer_loop(Q) ->
    receive
        {in, Ref, Pid, Bin} ->
            erl_lfq:in(Q, Bin),
            Pid ! {Ref, ok}
    end,
    producer_loop(Q).

command(S) ->
    oneof(
            [{call, ?MODULE, new, []} || S#state.pid == undefined] ++
            [{call, ?MODULE, in, [S#state.pid, binary()]} || S#state.pid /= undefined] ++
            [{call, ?MODULE, out, [S#state.queue]} || S#state.pid /= undefined]
    ).

new() ->
    {ok, Q} = erl_lfq:new(),
    Pid = spawn_link(?MODULE, producer_loop, [Q]),
    {Q, Pid}.

in(Pid, Bin) ->
    Ref = make_ref(),
    Pid ! {in, Ref, self(), Bin},
    receive
        {Ref, ok} ->
            ok
    end.

out(Queue) ->
    erl_lfq:out(Queue).

invariant(#state{queue=Q, model=M}) when Q /= undefined ->
    QLen = length(M),
    QSize = lists:foldl(fun(E, Acc) ->
                    byte_size(E) + Acc
            end, 0, M),
    case erl_lfq:byte_size(Q) == QSize of
        true ->
            case erl_lfq:len(Q) == QLen of
                true ->
                    true;
                false ->
                    {bad_length, erl_lfq:len(Q), QLen}
            end;
        false ->
            {bad_byte_size, erl_lfq:byte_size(Q), QSize}
    end;
invariant(_) ->
    true.

precondition(_S,{call,_,_,_}) ->
    true.

postcondition(_S,{call,_,new,[]},_R) ->
    true;
postcondition(_S,{call,_,in,[_,_Bin]},_R) ->
    true;
postcondition(S,{call,_,out,[_]},R) ->
    case R of
        {empty, _} ->
            case S#state.model == [] of
                true ->
                    true;
                false ->
                    {unexpected_empty, S#state.model}
            end;
        {{value, Val}, _} ->
            case S#state.model == [] of
                true ->
                    {unexpected_non_empty, Val};
                false ->
                    case Val == hd(S#state.model) of
                        true ->
                            true;
                        false ->
                            {unexpected_value, Val, hd(S#state.model)}
                    end
            end
    end.


next_state(S,V,{call, _, new, []}) ->
    S#state{queue={call, erlang, element, [1, V]}, pid={call, erlang, element, [2, V]}};
next_state(S,_V,{call, _, in, [_, Bin]}) ->
    S#state{model=S#state.model ++ [Bin]};
next_state(S,_V,{call, _, out, [_]}) ->
    case S#state.model == [] of
        true ->
            %% still empty
            S#state{model=[]};
        false ->
            S#state{model=tl(S#state.model)}
    end.

prop_sequential() ->
    ?FORALL(Cmds,commands(?MODULE),
            ?TRAPEXIT(
            aggregate(command_names(Cmds),
                      begin
                    {H,S,Res} = run_commands(?MODULE,Cmds),
                    catch(unlink(S#state.pid)),
                    catch(exit(S#state.pid, kill)),
                    ?WHENFAIL(io:format("History: ~p\nState: ~p\nRes: ~p\n~p\n",
                                        [H,S,Res, zip(Cmds, [Y || {_, Y} <- H])]),
                              Res == ok)
                end))).
-endif.
-endif.

