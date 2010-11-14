module Neuron.PoissonNeuron where

import Neuron.Support
import List
import Support.Probability
import Support.Correlation
import System.Random

gen_spikes :: Probability -> StdGen -> [Time]
gen_spikes lambda gen = poisson_process lambda gen

merge_trains :: [Time] -> [Time] -> Probability -> (StdGen,StdGen) -> [Time]
merge_trains t1 t2 a (g1,g2) = merge ft1 ft2
	where ft1 = map fst $ filter (\(x,y) -> y) $ zip t1 b1;
		  ft2 = map fst $ filter (\(x,y) -> y) $ zip t2 b2;
		  b1 = bernoulli a g1;
		  b2 = bernoulli (1-a) g2

merge :: [Time] -> [Time] -> [Time]
merge [] x = x
merge x [] = x
merge (x1:xs1) (x2:xs2)
	| (x1 < x2) 		= x1:(merge xs1 (x2:xs2))
	| otherwise 		= x2:(merge (x1:xs1) xs2)

spike_corr :: (Time,Time,Time) -> [Time] -> [Time] -> Double
spike_corr (t1,t2,dt) s1 s2 = corr (f s1) (f s2)
	where f x = get_spike_section_from_times x (t1,t2,dt)
	
spike_rate :: (Time,Time,Time) -> [Time] -> Double
spike_rate (t1,t2,dt) s = (mean (f s))/dt
	where f x = get_spike_section_from_times x (t1,t2,dt)

get_spike_section_from_times :: [Time] -> (Time,Time,Time) -> [Double]
get_spike_section_from_times ts (t1,t2,dt) = f ts
	where f x = map (g . fst) $ takeWhile (\(s,t) -> t <= t2) $ zip (spikes $ h x) (times $ h x);
		  g x = if x then 1.0 else 0.0;
		  h x = times_to_train (t1,dt) x

inputs :: Double -> Double -> Int -> [StdGen] -> [[Time]]
inputs r c n gens = map f [1..n]
	where f x = merge_trains common (gen_spikes r $ gens !! (3*x)) (sqrt c) $ (gens !! (3*x-1),gens !! (3*x-2));
		  common = gen_spikes r $ gens !! 0

