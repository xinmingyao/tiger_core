
-define(NODE_DOWN,down).
-define(NODE_UP,up).


-record(master_node,{name,state::?NODE_DOWN|?NODE_UP}).

