module IFPopulations where

import Neuron.PoissonNeuron
import Neuron.Support
import Neuron.IFNeuron
import Neuron.IFNetwork
import Support.Probability
import System.Random
import Data.List

type Input = [Time]
type InputGroup = [Input]

create_input_groups :: [(Int,Double,Double,[StdGen])] -> [InputGroup]
create_input_groups group_data = map (\(n,r,c,gens) -> inputs r c n gens) group_data

-- create_populations :: [(Int,State,[Double],[Double])] -> [InputGroup] -> [[StdGen]] -> Network
-- create_populations population_data inputs gens = 

create_population :: State -> [(Int,Double,[StdGen])] -> [(InputGroup,Double,[StdGen])] -> Network	
create_population state pconn iconn = map (\(x,y) -> create_neuron state x y) $ zip ins conns
	where ins = map (\(x,y,z) -> sample_inputs ) iconn;
		  conns = concat $ map (\(x,y) -> map (\a -> a + y) x) $ zip conns0 m;
		  conns0 = map (findIndices (\x -> x)) $ connectivity_matrix pconn;
		  m = scanl (+) 0 $ [n | (n,a,g) <- pconn]
	
create_neuron :: State -> [Input] -> [Int] -> ConnectedNeuron
create_neuron state inputs conns = cn
	where cn = ConnectedNeuron { name = "Neuron",
 								 initial_state = state,
								 synapses_int = conns,
								 synapses_ext = inputs,
								 current = null_pulse }

sample_inputs :: [(InputGroup,Double,StdGen)] -> [Input]
sample_inputs group_data = concat [[i | (i,b) <- (zip ig bs), b] | (ig,bs) <- zip groups mat]
	where mat = connectivity_matrix $ map (\(x,y,z) -> (length x,y,z)) group_data;
		  groups = [g | (g,_,_) <- group_data]

connectivity_matrix :: [(Int,Double,StdGen)] -> [[Bool]]
connectivity_matrix groups = bools
	where bools = map (\(x,y,z) -> take x $ bernoulli y z) groups

type Synapse = Bool

make_config :: [(Synapse,Time)] -> Config
make_config syns = Config { vrest = -70e-3, 
						    vreset = -80e-3, 
						    vspike = 50e-3, 
						    vthresh = -54e-3,
						    ess = map (\(x,y) -> if x then 0 else -80e-3) syns,
						    lags = map snd syns,
					 	    rm = 1, 
						    res = 10e6, 
						    tau = 10e-3, 
						    taus = 20e-3,  
						    psmax = 1.0,
						    vk = -70e-3, 
						    tau_sra = 100e-3, 
						    delg = 0.3e0 }

make_state :: Config -> State
make_state c = State { vm = -70e-3, 
     		 		   g = 0.0, 
					   time_step = 0, 
				 	   gss = replicate n 1.0,
					   pss = replicate n 0.0,
					   config = c }
	where n = length $ lags c
						

dt = 0.1e-3

-- net_trains = get_network_trains net dt
-- 
-- run t = do all_rasters dt 5e-3 t net_trains
