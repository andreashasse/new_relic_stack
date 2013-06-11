-define(NR(X), new_relic_stack:apply({class, ?MODULE}, fun() -> X end)).
-define(NRCLASS(X, CLASS),
        new_relic_stack:apply({class, CLASS}, fun() -> X end)).
