-record(eva_queue_req, {
    created_at :: erlang:timestamp(),
    from :: tuple(),
    payload :: term()
}).

-define(PUSH_CMD(Priority, Payload), {push_cmd, Priority, Payload}).

-define(PULL_CMD(Count), {pull_cmd, Count}).

-define(QUEUE_TRANSFER, {queue_transfer}).

-define(TABLE_NAME_BY_PRIORITY(P),
    case P of
        1 -> eva_queue_1;
        2 -> eva_queue_2;
        3 -> eva_queue_3;
        4 -> eva_queue_4;
        5 -> eva_queue_5
    end
).