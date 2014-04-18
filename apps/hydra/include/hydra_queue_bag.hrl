-record(hydra_queue_bag, {
    %% priorities
    p :: [non_neg_integer()],
    %% priorities bags
    pb :: [non_neg_integer()],
    %% priorities limits
    pl :: [non_neg_integer()],
    %% price
    prc :: non_neg_integer()
}).