-module(direct_type_deps).

-type t() :: result:t().
-type u() :: option:t().
-type v() :: panic:t().

