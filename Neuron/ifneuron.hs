module Neuron.IFNeuron (Config(..), State(..), time, Neuron, simulate, 
						get_train, displayState, displayNeuron, find_rate) where
	
import Neuron.Support
import Data.List

data Config = Config { vrest :: Voltage
					 , vreset :: Voltage
					 , vspike :: Voltage
					 , vthresh :: Voltage
					 , ess :: [Voltage]
					 , lags :: [Time]
					 , rm :: Double
					 , res :: Resistance
					 , tau :: Time
					 , taus :: Time
					 , psmax :: Probability
					 , vk :: Voltage
					 , tau_sra :: Time
					 , delg :: Double } deriving (Show)

c1 = Config { vrest = -70e-3, 
			  vreset = -80e-3, 
			  vspike = 50e-3, 
			  vthresh = -54e-3,
			  ess = [0.0,0.0],
			  lags = [80e-3,0e-3],
		 	  rm = 1, 
			  res = 10e6, 
			  tau = 10e-3, 
			  taus = 20e-3,  
			  psmax = 1.0,
			  vk = -70e-3, 
			  tau_sra = 100e-3, 
			  delg = 0.3e0 }

data State = State { vm :: Voltage
				   , g :: Double
				   , time_step :: Integer
				   , gss :: [Double]
				   , pss :: [Probability]
				   , config :: Config } deriving (Show)
				
time :: Time -> State -> Time
time dt state = dt*(fromIntegral $ time_step state)
					
type Neuron = [State]

dg :: State -> Double
dg state = -(g state)/(tau_sra $ config state)

dpss :: State -> [Probability]
dpss state = [ -ps/(taus $ config state) | ps <- pss state]	

dvm :: State -> Current -> Voltage
dvm state i = (-(vm state) + (vrest c) - (rm c)*(sra + synsum) + (res c)*i)/(tau c)
	where sra = (g state)*((vm state) - (vk c));
	      synsum = sum $ map syn $ zip3 (ess c) (gss state) (pss state);
		  syn (es,gs,ps) = gs*ps*((vm state) - es);
		  c = config state

g1 :: State -> Time -> Double
g1 state dt
	| vm0 == vs        = g0 + (delg $ config state)
	| otherwise  	   = euler (dg state) g0 dt
	where vm0 = vm state;
		  g0 = g state;
		  vs = vspike $ config state;

pss1 :: State -> [Bool] -> Time -> [Probability]
pss1 state spikes dt = map f (zip3 spikes (pss state) (dpss state))
	where f (s,ps,dps)
		| s == True      = ps + (psmax $ config state)*(1-ps)
		| otherwise      = euler dps ps dt

vm1 :: State -> Current -> Time -> Voltage
vm1 state i dt
	| vm0 == vs        = vr
	| vm0 <= vt        = euler (dvm state i) vm0 dt
	| otherwise        = vs
	where vm0 = vm state;
	      vs = vspike $ config state;
	      vt = vthresh $ config state;
	      vr = vreset $ config state;

		
update :: CurrentProfile -> Time -> (State,[SpikeTrain]) -> (State,[SpikeTrain])
update i dt (state,trains) = (new_state,new_trains)
	where new_state = State { vm = vm1 state (get_current t i) dt
							, g = g1 state dt
							, time_step = (time_step state) + 1
							, gss = gss state
							, pss = pss1 state (map (get_spike t) trains) dt
							, config = (config state) };
		  new_trains = [new_train train | train <- trains];
		  new_train old_train = SpikeTrain { spikes = drop (n old_train) $ spikes old_train
										   , times = drop (n old_train) $ times old_train };
		  n old_train = length $ takeWhile (< t) $ times old_train;
		  t = time dt state
						
simulate :: State -> CurrentProfile -> [SpikeTrain] -> Time -> Neuron
simulate state iinj trains dt = map fst $ iterate (update iinj dt) (state,new_trains)
	where new_trains = [offset_train train lag | (train,lag) <- zip trains (lags $ config state)]

get_train :: Time -> Neuron -> SpikeTrain
get_train dt neuron = SpikeTrain { spikes = map fst train, times = map snd train } 
	where train = [(vm state == vspike (config state),time dt state) | state <- neuron]

displayState :: (Show a) => (State -> [a]) -> State -> String
displayState fields state = intercalate "," $ map show $ fields state

displayNeuron :: (Show a) => (State -> [a]) -> Int -> Neuron -> String
displayNeuron fields n neuron = unlines [displayState fields x | x <- take n neuron]

find_rate :: State -> Time -> Current -> Int
find_rate state dt i = length $ findIndices ((==) True) $ spikes train
	where train = get_train dt $ take (steps 1 dt) neuron;
	      neuron = simulate state irate [] dt;
	      irate = [CurrentPulse { istart = 0, iend = 1, iamp = i }]
