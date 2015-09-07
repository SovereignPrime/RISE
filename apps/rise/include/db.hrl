-record(db_version, 
        {
         version :: {non_neg_integer(), non_neg_integer(), non_neg_integer()},
         schema :: [proplists:property()],
         update_fun :: fun()
        }).
-record(db_group,
        {
         id :: non_neg_integer(),
         name :: string(),
         subgroups :: non_neg_integer() | undefined
        }).
-record(db_group_members,
        {
         group :: non_neg_integer(),
         contact :: non_neg_integer()
        }).

-record(db_contact,
        {id :: non_neg_integer(),
         name="User " :: string() | undefined,
         email="" :: string() | undefined,
         phone="" :: string() | undefined,
         photo="undefined.png" :: string() | undefined,
         bitmessage :: binary() | undefined,
         address :: binary() | undefined,
         my=false :: boolean(),
         status :: atom()}).
-record(db_contact_note,
        {
         id :: non_neg_integer(),
         contact :: non_neg_integer(),
         datetime :: calendar:date_time(),
         text="" :: string()
        }).
-record(db_contact_roles,
        {id :: non_neg_integer(),
         type,
         tid,
         role,
         contact :: non_neg_integer()
        }).

-record(db_task_change,
        {address :: binary(),
         datetime :: calendar:date_time(),
         field :: atom(),
         new_value :: term()
        }).
-record(db_task,
        {id :: binary(),
         due :: calendar:date_time(),
         recurring :: tuple(),
         name :: iodata(),
         text = <<"">> :: binary(),
         parent :: binary(),
         effort={1.0, days} :: {float(), atom()} | atom(),
         sort :: non_neg_integer() | atom(),
         status=new :: atom(),
         changes=[] :: [#db_task_change{}]
        }).
-record(db_task_tree,
        {task :: binary(),
         parent :: binary(),
         time :: non_neg_integer(),
         visible=false :: boolean()
        }).

%% DEPRECATED: Will be removed in next release
-record(db_file,
        {id,
         path,
         type,
         user,
         date,
         status,
         size}).
-record(db_attachment,
        {id,
         file :: binary(),
         type,
         tid :: binary()}).

-record(db_expense,
        {id,
         name,
         date,
         type=expense,
         text,
         amount,
         status,
         to,
         from}).
-record(db_expense_tasks,
        {expense,
         task :: binary()}).

%% DEPRECATED: Will be removed in next release
-record(db_update,
        {id,
         subject,
         from,
         to=[],
         text,
         date,
         status}).
-record(db_search,
        {name="" :: string(),
         text :: [proplists:property()]
        }).

-define(ROLES, 
        [{"Responsible", "responsible"},
         {"Accountable", "accountable"},
         {"Consulted", "consulted"},
         {"Informed", "informed"},
         {"Concerning", "concerning"}]).
