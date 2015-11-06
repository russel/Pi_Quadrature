#!/usr/bin/env swipl

pi_by_4(N, N, _Delta, Result, Result) :- !.
pi_by_4(I, N, Delta, Current, Result) :-
    X is (I - 0.5) * Delta,
    Next is Current + 1.0 / (1.0 + X * X),
    K is I + 1,
    pi_by_4(K, N, Delta, Next, Result).

:-
    use_module(output),
    N is 10000000, % 100 times fewer that usual due to performance.
    Delta is 1 / N,
    T_start is cputime,
    pi_by_4(0, N, Delta, 0, Pi_by_4),
    Pi is 4 * Delta * Pi_by_4,
    T_elapse is cputime - T_start,
    out('Sequential', Pi, N, T_elapse),
    halt.
