-record(hydra_queue_ets, {
    %% tables
    tables :: [atom()],
    %% bag
    bag :: hydra_queue_bag:bag()
}).