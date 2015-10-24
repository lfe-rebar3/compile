-define(PROVIDER, compile).
-define(RE_PREFIX, "^[^._]").
-define(PRV_ERROR(Reason),
        {error, {?MODULE, Reason}}).

