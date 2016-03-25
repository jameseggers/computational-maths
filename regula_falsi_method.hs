f :: Float -> Float
f x = (x^3) - (2*x) - 5

numericalSolution :: Float -> Float -> Float -> Float -> Float
numericalSolution a fB b fA = ((a * fB) - (b * fA)) / (fB - fA)

regulaFalsi :: Float -> Float -> Float -> Int -> Int -> IO()
regulaFalsi a b tolerance maxIterations iterations
  | tolI < tolerance = print xNS
  | iterations > maxIterations = print "Max Iterations Reached"
  | faNS < 0 = regulaFalsi a xNS tolerance maxIterations (iterations + 1)
  | faNS > 0 = regulaFalsi xNS b tolerance maxIterations (iterations + 1)
  where fa  = f a
        fb  = f b
        xNS = numericalSolution a (f a) b (f b)
        faNS = fa * (f xNS)
        tolI = (b - a) / 2
