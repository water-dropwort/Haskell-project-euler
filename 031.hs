module Problem031 where

answer :: Int
answer = solve 200

solve :: Int -> Int
solve total = sum [1 | e2  <- [0..total`div`200]
                     , e1  <- [0..(total-coinSum[e2])`div`100]
                     , p50 <- [0..(total-coinSum[e2,e1])`div`50]
                     , p20 <- [0..(total-coinSum[e2,e1,p50])`div`20]
                     , p10 <- [0..(total-coinSum[e2,e1,p50,p20])`div`10]
                     , p5  <- [0..(total-coinSum[e2,e1,p50,p20,p10])`div`5]
                     , p2  <- [0..(total-coinSum[e2,e1,p50,p20,p10,p5])`div`2]]

coinSum :: [Int] -> Int
coinSum coins = sum [price*num | (price, num) <- zip [200,100,50,20,10,5,2,1] coins]
