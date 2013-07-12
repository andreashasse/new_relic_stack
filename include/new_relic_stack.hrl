-define(NR(X), new_relic_stack:apply({class, ?MODULE}, fun() -> X end)).
-define(NRCLASS(X, CLASS),
        new_relic_stack:apply({class, CLASS}, fun() -> X end)).
-define(NRBG(X, BG),
        new_relic_stack:apply({background, BG}, fun() -> X end)).
-define(NRIGNORE(X),
        new_relic_stack:apply(ignore, fun() -> X end)).
