% revfm.pl -- Reverse engineering on configuration files
% Authors: Arnaud Gotlieb
% SIMULA Research Laboratory, Lysaker , Norway
% Date = Oct. 2013 - version 1
%        May  2014 - version 2
%        June 2014 - version 3: generate constraints of the form X=u => Y in {v1,..,vn} and Y not in {w1,.., wp}
%        July 2014 - version 4: external usage to read in configuration matrix

:- use_module(library(clpfd)).
:- use_module(library(random)).

%:- clpfd:full_answer.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EXTERNAL USAGE:
%
% Ex:  sicstus -f -l revfm.pl --goal main. -a configuration_matrix.csv results.txt
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

main:-
        prolog_flag(argv, [F_IN, F_OUT]),
        analyse_file(csv, F_IN, MATRIX),
        MATRIX = [M1|_],
        create_lv(M1, LV),
        revfm(LV, MATRIX, LTC),
        write_results(F_OUT, LTC),
        halt.


% Ex: analyse_file(csv, 'configuration_matrix.csv', M).

analyse_file(csv, F_IN, MATRIX):-
        open(F_IN, read, STREAM),
        skip_line(STREAM),
        read_all(STREAM, MATRIX),
        close(STREAM).

read_all(S, []) :-
        peek_char(S, end_of_file),
        !.
read_all(S, [M|Ms]) :-
        read_line(S, CODES),
        analyse_line(CODES, [], M),
        !,
        read_all(S, Ms).
read_all(S, _) :-
        write('Revfm exception: configuration file corrupted'). 

analyse_line([], C, [M]):-
        lists:reverse(C,CR),
        name(M, CR).
analyse_line([44|S], C, [M|Ms]) :-
        lists:reverse(C,CR),
        name(M, CR),
        !,
        analyse_line(S, [], Ms).
analyse_line([N|S], C, M):-
        analyse_line(S, [N|C], M).


create_lv([], []).
create_lv([_X|Xs], [_Y|Ys]) :-
        create_lv(Xs, Ys).
        


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% USAGE:
%
%  Ex: [revfm], revfm([X,Y,Z], [[1,4, 78], [5,6, 7], [1, 6, 78], [5,1, 7]], _LTC), write_results(_LTC).
%   
%  Ex: [revfm], trace, revfm([X,Y,Z], [[1,4, 78], [5,6, 7], [1, 6, 78], [5,1, 7]], _LTC), arg(2,X,Vx), arg(2,Z,Vz), Vx#=Vz-2.
%
% Ex: [revfm], revfm([X,Y,Z], [[1,4, 78], [5,6, 7], [1, 6, 78], [5,1, 7]], _LTC), write_results('res.txt',_LTC).
% interpret, compute the list of cross-tree constraints, write it in a human readable format
%
% EX: [revfm], bench(12, 10, 5, CTR).
% interpret, generate a random variability matrix (12 configs, 10 features, 5 values), compute the list of cross-tree constraints,
%
% 1sec for creating 10000 CTRS (ex: bench(100, 100, 20, CTR), bench(1000, 100, 20, CTR)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% revfm(+LV, +TABLE, -LTC)
% LV must be a list of unbounded vars ;
% LTC is the list of cross-tree constraints. 3 possibilities:
% - ctc(Feat1, U, Feat2, [V1,..,Vp], [W1,...,Wq]) means (Feat1 = U) implies (Feat2 in {V1,...,Vp} and Feat2 not in {W1,..,Wq}) ;
% - nil: to be ignored

% Data structure: A feature is a triple feat(N, X, DOMX) where N is the feature number, X is an FD variable, DOMX is a list representing the initial domain of X

% Ex: revfm([X,Y,Z], [[1,4, 78], [5,6, 7], [1, 6, 78], [5,1, 7]], LTC).
% X = feat(1, _A, [1,5])
% Y = feat(2, _B, [1,4,6])
% Z = feat(3, _C, [7,78])

% _A in{1}\/{5},
% _B in{1}\/{4}\/{6},
% _C in{7}\/{78} ? 

revfm(LV, TABLE, LTC) :-
        create_all_atts(LV, 1, LVV),       % LV is a list of attributed variables
        clpfd:table([LVV], TABLE),
        W = 0,                             % W=1 : activate trace, W=0 : don't
        update_all_atts(LV),
        find_all_ltc(LV, W, LV, [], LTC).

create_all_atts([], _N, []).
create_all_atts([X|Xs], N, [Y|Ys]) :-
        X = feat(N, Y, _DOMY),
        N1 is N+1,
        create_all_atts(Xs, N1, Ys).

update_all_atts([]).
update_all_atts([X|Xs]) :-
        arg(2, X, FD_VAR),
        arg(3, X, DOMY),
        clpfd:fd_set(FD_VAR, XSET), clpfd:fdset_to_list(XSET, DOMY),
        update_all_atts(Xs).

find_all_ltc([], W, _LV, LTC, LTC).
find_all_ltc([X|Xs], W, LV, LTC1, LTC3) :-
        find_ltc(W, X, LV, LTC1, LTC2),
        find_all_ltc(Xs, W, LV, LTC2, LTC3).

find_ltc(W, X, LV, LTC_IN, LTC_OUT):-
        \+ (find_ctc(X, LV, LVal), asserta(lsg(LVal)) ),
        !.   % nothing found for X
find_ltc(W, X, LV, LTC_IN, LTC_OUT):-
        retract(lsg(LVal)),
        write_ctc(W, LVal),
        append(LVal, LTC_IN, LTC_OUT).
        
find_ctc(X, LV, LVal):-
        arg(1, X, N),
        arg(2, X, Y),
        findall( L,
                 ( test_hypo(Y), find_constraints(LV, X, N, Y, L) ), 
                 LRES
               ),
        flatten(LRES, [], LVal).

% Non-deterministic predicate
%test_hypo(X) :-
%        indomain(X).

test_hypo(X) :-
        clpfd:fd_dom(X, FD),   % FD is a ConstantRange (CR) 	::= ConstantSet | Constant .. Constant | CR /\ CR | CR \/ CR | \ CR 
        label(FD, X).

label({N}, X) :-
        !,
        X = N.
label(N..N, X) :-
        !,
        X = N.
label(N..M, X) :-
        N \= M,
        !,
        ( X = N ; (N1 is N+1, label(N1..M, X)) ).
label('\\/'(T1,T2), X) :-
        !,
        ( label(T1, X) ; label(T2, X) ). 
label('/\\'(T1,T2), X) :-
        !,
        write('Warning'),nl.
label('\\'(T), X) :-
        !,
        write('Warning'),nl.

find_constraints([], _X, _N, _Y, []).
find_constraints([F|Fs], X, N, Y, [ctc(X, F, CTR1, CTR2)|RES]) :-
        arg(1, F, M),
        M \== N,
        arg(2, F, S),
        
%        ( number(S) ->
%            generate_ctr(requires,X,Y,F,S, CTR1)
%        ;
%            CTR1 = nil
%        ),
        clpfd:fd_set(S, DSET),
        clpfd:fdset_to_list(DSET, CTR1),

        arg(3, F, DF),
        clpfd:list_to_fdset(DF,FSET),
        clpfd:fdset_subtract(FSET, DSET, DIFF),
%        ( clpfd:fdset_singleton(DIFF, ELT) ->
%            generate_ctr(excludes,X,Y,F,ELT, CTR2)
%        ;
%            CTR2 = nil
%        ),
        clpfd:fdset_to_list(DIFF,CTR2),
        
        !,
        find_constraints(Fs, X, N, Y, RES).
find_constraints([F|Fs], X, N, Y, RES) :-
        find_constraints(Fs, X, N, Y, RES).

%generate_ctr(FONCT,X,Y,F,S,CTR) :-
%        CTR =.. [FONCT, X,Y,F,S].

flatten([],OUT, OUT).
flatten([X|Xs], IN, OUT) :-
        append(X, IN, IN1), 
        flatten(Xs, IN1, OUT).


write_ctc(1, LVal) :-
        write_ctc_rec(LVal).
write_ctc(0, LVal).


write_results(LTC):-
        write_ctc_rec(user_error, LTC).

write_results(FILE, LTC) :-
  open(FILE, write, STREAM),
  write_ctc_rec(STREAM, LTC),
  close(STREAM),
  write(user_error, 'Results file produced'), nl(user_error).

write_ctc_rec(_S, []).
write_ctc_rec(S, [nil|LVal]):-
        !,
        write_ctc_rec(S, LVal).
write_ctc_rec(S, [ctc(F1,F2,IN, NOTIN)|LVal]) :-
        arg(1, F1, V1),
        arg(2, F1, N),
        arg(1, F2, V2),
        write(S, 'Feat'),write(S, V1),write(S, ' = '),write(S, N), write(S, ' => '),
        write(S, 'Feat'),write(S, V2),write(S, ' in '),write(S, IN),write(S, ' and not in '), write(S, NOTIN), nl(S),
        !,
        write_ctc_rec(S, LVal).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Naive version for comparison
%  
%  revfm_naive([X,Y,Z], [[1,4, 78], [5,6, 7], [1, 6, 78], [5,1, 7]], LTC).

revfm_naive(LV, TABLE, LTC) :-
        create_all_atts(LV, 1, LVV), % LV is a list of attributed variables
        rec_search(TABLE, LVV, TABLE, 1, LTC).

rec_search([], LVV, TABLE, _, LTC).
rec_search([T|Ts], LVV, TABLE, N, LTC) :-
        rec_search_ctr(T, LVV, T, TABLE, N, 1, LTC1),   % AG: TO BE FINISHED
        N1 is N+1,
        rec_search(Ts, LVV, TABLE, N1, LTC2),
        lists:append(LTC1, LTC2, LTC).

rec_search_ctr([], [], _T, _TABLE, _N, _M, []).
rec_search_ctr([V|Vs], [X|Xs], T, TABLE, N, M, [CTR|LCTR]) :-
        extract_col(TABLE, V, N, M, LCOL),
        find_implications(LCOL, LCOL, V, X, N, M, CTR),  % X=V in cell(N, M) of TABLE
        M1 is M+1,
        rec_search_ctr(Vs, Xs, T, TABLE, N, M1, LCTR).

extract_col([], _V, _N, _M, []):-!.
extract_col([L|Ls], V, N, M, [L|S]) :-
        lists:nth1(M, L, V),
        !,
        extract_col(Ls, V, N, M, S).
extract_col([L|Ls], V, N, M, S):-
        extract_col(Ls, V, N, M, S).


find_implications([], _LCOL, _V, _X, _N, _M, []) :-!.
find_implications([W|Ws], LCOL, V, X, N, M, [CTR|LCTR]) :-
        lists:length(W, P),
        lookup_impl(1, P, W, M,  V, LCOL, CTR),
        find_implications(Ws, LCOL, V, X, N, M, LCTR).

% AG: lookup_impl TO BE CONTNUED
% Typical call: Call: lookup_impl([1,4,78],1,[[1,4,78],[1,6,78]],_5243) ?
%
        
% Algo: 1. Iterate on Table, select a column, Iterate on values, select a value - (X=v) - extract columns st X=v is present
% find patterns in the remains, go back to 1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Random generator of one variability maxtrix
%
% gen_random(+N, +SIZE, +D, -TABLES)
% N is the number of configurations to be generated
% SIZE is the number of features for each individual configuration
% D is the upper bound value, for generating random value for configration parameters from 0 to D-1
%
% For example, gen_random(3, 5, 2, T) produces a table T, of 3 configurations with 5 features with values from 0 to 1 (i.e., Boolean matrix)

gen_random(N, SIZE, D, TABLES) :-
        gen_random(0, N, SIZE, D, TABLES).

gen_random(N, N, _SIZE, _D, []).
gen_random(N1, N, SIZE, D, [T|Ts]) :-
        N1 < N,
        gen_table(0, SIZE, D, T),
        N2 is N1+1,
        gen_random(N2, N, SIZE, D, Ts).

gen_table(S, S, _D, []).
gen_table(S1, S, D, [X|Xs]) :-
        S1 < S,

        random(0, D, X),
        S2 is S1+1,
        gen_table(S2, S, D, Xs).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% BENCHMARKS
%
% [revfm], bench(12, 10, 5, _CTR), sort_ctc(_CTR, LREQ, NR, LEXC, LX, N).
% bench(50, 25, 15, _CTR), sort_ctc(_CTR, LREQ, NR, LEXC, LX, N). 
% bench(20, 10, 10, _CTR), sort_ctc(_CTR, LREQ, NR, LEXC, LX, N). 

bench(N, SIZE, D, CTR) :-
        gen_random(N, SIZE, D, TABLE),
        lists:length(LV, SIZE),
        statistics(runtime,[_,_T0]),
        revfm(LV, TABLE, CTR),
        statistics(runtime,[_,_T1]),
        T is _T1-_T0,
        write('%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'),nl,
        write('STATISTICS'),nl,
        write('Runtime: '), write(T),write(' miliseconds'),nl,
        fd_statistics,
        write('%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'),nl.


% benchs will repeat 10 times the experiment to avoid the factor of bad luck
benchs(N, SIZE, D) :-
        P = 10,
        benchs(0, P, 0, T, 0, M, N, SIZE, D),
        T1 is T / P,
        M1 is M // P,
        write('%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'),nl,
        write('STATISTICS over '),write(P), write(' trials'),nl,
        write('Average runtime: '), write(T1),write(' miliseconds'),nl,
        write('Average nbre of generated ctrs: '), write(M1),nl,nl,
        write('P times'),nl,
        fd_statistics,
        write('%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'),nl.

benchs(P, P, T, T, M, M, N, SIZE, D) :-!.
benchs(P1, P, T1, T, M1, M, N, SIZE, D):-
       gen_random(N, SIZE, D, TABLE),
       lists:length(LV, SIZE),
       statistics(runtime,[_,_TT0]),
       revfm(LV, TABLE, CTR),
       statistics(runtime,[_,_TT1]),
       lists:length(CTR, M0),
       M2 is M1 + M0,
       T2 is _TT1 - _TT0 + T1,
       P2 is P1 + 1,
       benchs(P2, P, T2, T, M2, M, N, SIZE, D).
