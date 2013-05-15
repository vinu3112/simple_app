%module names
-define(SUPER_VISOR, store_supervisor).
-define(APP_LOG, log_gen_server).
-define(ASSOC_SERVER,store_gen_server).
-define(ASSOC_EVENT,store_gen_event).


%Record used for storing details of user
-record(user_detail,{name,id}).