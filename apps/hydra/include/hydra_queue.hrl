-record(hydra_queue_req, {
    created_at :: erlang:timestamp(),
    from :: {Pid :: pid(), Ref :: reference()},
    uri :: hydra_queue:queue_req_uri()
}).

-define(PUSH_CMD(Priority, Payload), {push_cmd, Priority, Payload}).

-define(PULL_CMD(Count), {pull_cmd, Count}).

-define(QUEUE_TRANSFER, {queue_transfer}).

-define(TABLE_NAME_BY_PRIORITY(P), (
    case P of
        1 -> hydra_queue_1;
        2 -> hydra_queue_2;
        3 -> hydra_queue_3;
        4 -> hydra_queue_4;
        5 -> hydra_queue_5
    end
)).