-define(PROVIDER, compile).
-define(NAMESPACE, lfe).
-define(RE_PREFIX, "^[^._]").
-define(PRV_ERROR(Reason),
        {error, {?MODULE, Reason}}).

