module Neuron.IFNetwork (ConnectedNeuron(..), Network, simulate_network, 
						 NetworkTrains, get_network_trains, all_rasters) where
	
import Neuron.Support
import Neuron.IFNeuron
import Support.Misc
import Data.List

data ConnectedNeuron = ConnectedNeuron { name :: String
									   , initial_state :: State
					   			       , synapses_int :: [Int]
					   			       , synapses_ext :: [[Time]]
					 				   , current :: CurrentProfile } deriving (Show)

type Network = [ConnectedNeuron]
type NetworkTrains = ([SpikeTrain],[SpikeTrain])
					
simulate_network :: Network -> Time -> [Neuron]
simulate_network network dt = neurons
	where neurons = map (simulate_network_neuron dt) $ zip network grouped_trains;
		  grouped_trains = [(map (trains !!) $ synapses_int cn) ++ (map (times_to_train (0.0,dt)) $ synapses_ext cn) | cn <- network];
		  trains = map (get_train dt) neurons

simulate_network_neuron :: Time -> (ConnectedNeuron, [SpikeTrain]) -> Neuron
simulate_network_neuron dt (cneuron,trains) = neuron
	where neuron = simulate (initial_state cneuron) (current cneuron) trains dt
	
get_network_trains :: Network -> Time -> NetworkTrains
get_network_trains network dt = (input_trains,output_trains)
	where input_trains = concat $ map (\x -> map (times_to_train (0.0,dt)) $ synapses_ext x) network;
		  output_trains = map (get_train dt) neurons;
		  neurons = simulate_network network dt

all_rasters :: Time -> Time -> Time -> NetworkTrains -> IO ()
all_rasters dt bin t trains = do
	putStrLn $ concat new_blocks
	where new_blocks = [(timeString i) ++ blk ++ "\n" | (blk,i) <- zip blocks [0..((length blocks)-1)]];
	      blocks = get_grouped_labelled_rasters (raster dt bin t) names $ concat all_trains;
		  names = concat $ map train_names $ zip ["in","n"] $ map length all_trains;
		  all_trains = [f trains | f <- [fst,snd]];
		  timeString ti = "\nt = " ++ (show (bin*(fromIntegral (ti*perline)))) ++ "\n";

train_names :: (String,Int) -> [String]
train_names (kind,k) = [kind ++ (show i) ++ ": " | i <- [1..k]]

get_grouped_labelled_rasters :: (SpikeTrain -> String) -> [String] -> [SpikeTrain] -> [String]
get_grouped_labelled_rasters to_raster names trains = map unlines line_blocks
	where line_blocks = [[n++r | (n,r) <- zip names rs] | rs <- (transpose rasters)];
		  rasters = map ((groupEvery perline) . to_raster) trains
		
perline = 150