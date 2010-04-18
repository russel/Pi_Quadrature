-module(pi_erlang_zvi).
-author("Zvi").
-compile([export_all]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% functional and parallel PI calculation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
test()->
	N = 1000000,
	Impls = [math,list_creation,lc,lc_no_pow,map,mapfoldl,serial_decr,serial,parallel],
	[ {Impl,timer:tc(pi,calc_pi,[Impl,N])} || Impl<-Impls ].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 1. built-in constant
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
calc_pi(math,_N) -> 
	math:pi();
	
calc_pi(list_creation,N) ->
	lists:seq(1,N),
	3.14159;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 2. serial - list comprehension - direct Matlab translation
%%	Step = 1/N;
%%	PI = Step*sum(4./(1+(((1:N)-0.5)*Step).^2))
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
calc_pi(lc,N) ->
	Step = 1/N,
	Step*lists:sum( [4.0/(1.0 + math:pow(Step*(I-0.5),2)) || I<-lists:seq(1,N)] );

calc_pi(lc_no_pow,N) ->
	Step = 1/N,
	Xs = [Step*(I-0.5) || I<-lists:seq(1,N)],
	Step*lists:sum( [4.0/(1.0 + X*X) || X<-Xs] );

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 3. serial - using HOF: map
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
calc_pi(map,N) ->
	Step = 1/N,
	F = fun(I) -> 
		X=Step*(I-0.5),
		4.0/(1.0+X*X)
	    end,
	Step*lists:sum( lists:map( F, lists:seq(1,N) ) );

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 4. serial - using HOF: mapfoldl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
calc_pi(mapfoldl,N) ->
	Step = 1/N,
	F = fun(I,Sum) -> 
		X=Step*(I-0.5), 
		V=4.0/(1.0+X*X), 
                 {0,Sum+V}
	    end,
	{_,Sum} = lists:mapfoldl(F, 0, lists:seq(1,N)),
	Step*Sum;

%% replace list to plist module  in tests using HOFs
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 5. serial - tail recursion - decrement index
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
calc_pi(serial_decr,N) ->
	calc_pi1(N,1/N,0);


%%%calc_pi(serial_decr,N) ->
%%%calc_pi1(N,1/N,0);


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 6. serial - tail recursion - increment index
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
calc_pi(serial,N) ->
	calc_pi2(1,N,1/N);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 7. parallel SMP version - process per scheduler
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
calc_pi(parallel,N) ->
	NWorkers = erlang:system_info(schedulers),
	NPerWorker = trunc(N/NWorkers+0.5),
	Step = 1/N,
	Self = self(),
	Pids = [spawn(fun() -> Self ! {sum,calc_pi2(I,lists:min([N,I+NPerWorker-1]),Step)} end) 
	        || I<-lists:seq(1,N,NPerWorker) ],
	lists:sum([receive {sum,S} -> S end || _<-Pids]).

	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
calc_pi1(0,Step,Sum) -> Step*Sum;
calc_pi1(I,Step,Sum) ->
	X=Step*(I-0.5),
	V=4.0/(1.0+X*X),
	calc_pi1(I-1,Step,Sum+V).

%%%calc_pi1(0, Step, Sum, _Numerator) -> 4*Sum div round(1/Step);
%%%calc_pi1(I, Step, Sum, Numerator) ->
%%%%% io:format("I ~p Step ~p Sum ~p Numerator ~p~n",
%%%%% [I, Step, Sum, Numerator]),
%%%X = Step*(I-0.5),
%%%V = round(Numerator / (1.0+X*X)), %% integer by rounding
%%%calc_pi1(I-1, Step, Sum+V, Numerator).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
calc_pi2(From,To,Step) ->
	calc_pi2(From,From,To,Step,0).

calc_pi2(To,_From,To,Step,Sum) -> Step*Sum;
calc_pi2( I,From,To,Step,Sum) ->
	X=Step*(I-0.5),
	V=4.0/(1.0+X*X),
	calc_pi2(I+1,From,To,Step,Sum+V).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
