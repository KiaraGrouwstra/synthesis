module Blocks (tasks, blocks) where

-- tasks
tasks = [concat drop]
    where
        -- tamandu
        -- concat :: [a] -> [a] -> [a]
        concat = (++)
        -- drop :: Int -> [a] -> [a]
        drop = drop

-- building blocks
blocks = [const flip true false not blocks]
    where
        const = const
        flip = reverse
        true = True
        false = False
        not = not
