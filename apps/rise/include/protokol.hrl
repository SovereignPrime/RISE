%% Moved to maps. 
%% Will be removed in future
-record(task_packet,
        {id,
         name,
         due,
         text,
         parent,
         status,
         involved,
         attachments,
         time,
         changes=[]}).

-record(role_packet,
        {address,
         role}).

-record(task_tree_packet,
        {task,
         parent,
         time}).

-record(task_comment,
        {task,
         text,
         time}).

-record(message_packet,
        {subject,
         text,
         involved,
         attachments,
         time}).

-record(update_packet,
        {subject,
         text,
         version,
         attachments,
         time}).
