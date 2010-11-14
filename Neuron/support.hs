module Neuron.Support (CurrentPulse(..), CurrentProfile, get_current, null_pulse, 
					   SpikeTrain(..), get_spike, offset_train, times_to_train,  
					   train_to_times, times_to_bools, raster, steps, euler, 
					   Voltage, Probability, Current, Resistance, Time) where
						
import Support.Misc
import Data.List
	
data CurrentPulse = CurrentPulse { istart :: Current
								 , iend :: Current
								 , iamp :: Current } deriving (Show)
								
type CurrentProfile = [CurrentPulse]

get_current_from_pulse :: Time -> CurrentPulse -> Current
get_current_from_pulse t pulse
 	| (t < (istart pulse))      = 0.0
	| (t < (iend pulse))        = iamp pulse
	| otherwise     			= 0.0
	
get_current :: Time -> CurrentProfile -> Current
get_current t profile = sum $ map (get_current_from_pulse t) profile

null_pulse :: CurrentProfile
null_pulse = [CurrentPulse { istart = 0, iend = 0, iamp = 0 }]
	
data SpikeTrain = SpikeTrain { spikes :: [Bool]
							 , times :: [Time] } deriving (Show)
	
get_spike :: Time -> SpikeTrain -> Bool
get_spike t spiketrain = (not $ null prespikes) && (fst $ last $ prespikes)
	where prespikes = takeWhile f st;
		  f (spike,time) = (time <= t);
		  st = zip (spikes spiketrain) (times spiketrain)
	
offset_train :: SpikeTrain -> Time -> SpikeTrain
offset_train spiketrain t = SpikeTrain { spikes = spikes spiketrain, times = new_times} 
	where new_times = map ((+) t) (times spiketrain)
	
times_to_train :: (Time,Time) -> [Time] -> SpikeTrain
times_to_train (t0,dt) spike_times = SpikeTrain { spikes = ss, times = ts }
	where ss = times_to_bools (t0,dt) spike_times;
		  ts = [dt*i + t0 | i <- [0..]]
		
times_to_bools :: (Time,Time) -> [Time] -> [Bool]
times_to_bools (t0,dt) [] = [False | i <- [0..]]
times_to_bools (t0,dt) (st:sts) = (replicate n False) ++ [True] ++ (times_to_bools (t1,dt) sts)
	where t1 = t0+(fromIntegral (n+1))*dt;
		  n = floor ((st-t0)/dt)
		
train_to_times :: Time -> SpikeTrain -> [Time]
train_to_times t train = [time | (spike,time) <- st, spike]
	where st = takeWhile f $ zip (spikes train) (times train);
		  f (spike,time) = time <= t
		
raster :: Time -> Time -> Time -> SpikeTrain -> String
raster dt bin t train = [if s then '|' else '.' | s <- ss]
	where ss = map (any ((==) True)) $ groupEvery j $ take k $ spikes train;
		  k = (steps t dt);
		  j = (steps bin dt)

type Voltage = Double       -- Volts
type Probability = Double   -- No units
type Current = Double       -- Amps
type Resistance = Double    -- Ohms
type Time = Double          -- Seconds

steps :: Time -> Time -> Int
steps t dt = fromIntegral (ceiling (t/dt))

euler :: Double -> Double -> Double -> Double
euler f_ f dt = f + f_*dt