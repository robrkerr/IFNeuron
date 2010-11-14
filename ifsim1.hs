module Test where

import Neuron.IFNeuron
import Neuron.Support
import Support.Misc
import Data.List

c1 = Config { vrest = -70e-3, 
			  vreset = -80e-3, 
			  vspike = 50e-3, 
			  vthresh = -54e-3,
			  ess = [0.0,0.0],
			  gss = [1.0,1.0],
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
			 pss = [0.0,0.0],
			 config = c1 }

dt = 0.1e-3

iinj = [CurrentPulse { istart = 1, 
				 	   iend = 4, 
					   iamp = 1.8e-9 }]

train_ext = times_to_train (0.0,dt) [0.10,0.11,0.13,0.18,0.21]

n1 = simulate s1 null_pulse [train_out2,train_ext] dt
train_out1 = get_train dt n1
n2 = simulate s1 null_pulse [train_out1] dt
train_out2 = get_train dt n2

fields x = concat [[time dt x], [vm x], [s], [g x], pss x]
	where s = if head $ spikes $ get_train dt [x] then 1.0 else 0.0

folder = "/Users/robertkerr/Dropbox/IFNeuron/"

output t = do
	writeFile (folder ++ "neuron1.csv") $ ("t,vm,s,g,ps1,ps2\n" ++ (displayNeuron fields k n1))
	writeFile (folder ++ "neuron2.csv") $ ("t,vm,s,g,ps1,ps2\n" ++ (displayNeuron fields k n2))
		where k = (steps t dt)
		
all_rasters bin t = do
	putStrLn string
	where string = concat (zipWith (\x y -> x ++ y) timing [(join names row) ++ "\n" | row <- list]);
		  list = zipWith3 (\x y z -> [x,y,z]) rin r1 r2;
	      names = ["in: ","n1: ","n2: "];
		  join names strs = concat $ zipWith3 (\x y z -> x ++ y ++ z) names strs $ spacers (length names);
		  spacers n = replicate n "\n";
		  r1 = groupEvery perline $ raster dt bin t train_out1;
		  r2 = groupEvery perline $ raster dt bin t train_out2;
		  rin = groupEvery perline $ raster dt bin t train_ext;
		  timing = [timeString ti | ti <- [0..((length rin) - 1)]];
		  timeString ti = "\nt = " ++ (show (bin*(fromIntegral (ti*perline)))) ++ "\n";
		  perline = 150
