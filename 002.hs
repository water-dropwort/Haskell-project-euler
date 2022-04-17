module Problem002 where

solve :: Int -> Int
solve termOfMax = solve' (1,1) 0
  where
    -- タプルはフィボナッチ数列の2項を格納するキューをイメージ。
    solve' (x_im2,x_im1) total =
      let x_i = x_im1 + x_im2
      in  if x_i <= termOfMax then
            if even x_i then
              solve' (x_im1,x_i) (total + x_i)
            else
              solve' (x_im1,x_i) total
          else
            total
