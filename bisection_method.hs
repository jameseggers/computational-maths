f x = 8 - 4.5 * (x - sin x)

bisect :: Int -> Float -> Float -> Float -> Int -> IO()
bisect maxIterations a b tol iterations
  | fxNS == 0 = print xNS
  | tolI < tol = print fxNS
  | maxIterations == iterations = print "max itrations reached"
  | lol < 0 = bisect maxIterations a xNS tol (iterations + 1)
  | lol > 0 = bisect maxIterations xNS b tol (iterations + 1)
  where xNS = (a + b) / 2
        tolI = (b - a) / 2
        fxNS = f xNS
        lol = (f a) * fxNS
