module Support.Probability (gentogen, gentogens, uniform, exponential, poisson_process, bernoulli, gaussian) where
	
import System.Random

gentogen :: StdGen -> StdGen
gentogen gen = snd (random gen :: (Int,StdGen))

gentogens :: StdGen -> [StdGen]
gentogens gen = iterate gentogen gen

uniform :: StdGen -> [Double]
uniform gen = randomRs (0.0,1.0) gen

exponential :: Double -> StdGen -> [Double]
exponential lambda gen = map f $ uniform gen
	where f u = -(log u)/lambda

poisson_process :: Double -> StdGen -> [Double]
poisson_process rate gen = scanl1 (+) $ exponential rate gen

bernoulli :: Double -> StdGen -> [Bool]
bernoulli rate gen = map (\x -> x < rate) $ uniform gen

gaussian :: Double -> Double -> Double -> Double
gaussian mu sig x = (1/(sqrt (2*pi*sig^2)))*(exp ((-(x-mu)^2)/(2*sig^2)))

	
