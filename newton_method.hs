f :: Float -> Float
f x = (x^2) - 5
f' x = (2*x)

nextX :: Float -> Float
nextX xN = xN - ( (f xN) / (f' xN) )

newtonRaphson :: Float -> Float -> Float -> IO()
newtonRaphson a tolerance last
  | tolI < tolerance = print next
  | otherwise = newtonRaphson next tolerance a
  where next = nextX a
        tolI = (abs (next - last)) / last
