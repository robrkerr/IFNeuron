module Test where

import Neuron.IFNetwork
import Neuron.IFNeuron
import Neuron.Support
import Support.Misc
import Data.List

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

s1 = State { vm = -70e-3, 
     		 g = 0.0, 
			 time_step = 0, 
			 gss = [1.0,1.0],
			 pss = [0.0,0.0],
			 config = c1 }

dt = 0.1e-3

train_ext = [0.1,0.11,0.13,0.18,0.21]

cn1 = ConnectedNeuron { name = "Neuron1",
 						initial_state = s1,
						synapses_int = [1],
						synapses_ext = [train_ext],
						current = null_pulse }
						
cn2 = ConnectedNeuron { name = "Neuron2",
 						initial_state = s1,
						synapses_int = [0,2],
						synapses_ext = [],
						current = null_pulse }
						
cn3 = ConnectedNeuron { name = "Neuron3",
 						initial_state = s1,
						synapses_int = [1],
						synapses_ext = [],
						current = null_pulse }
						
net = [cn1,cn2,cn3]

net_trains = get_network_trains net dt


run t = do all_rasters dt 5e-3 t net_trains
