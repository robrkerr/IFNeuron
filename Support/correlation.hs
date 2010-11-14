module Support.Correlation (mean, var, corr) where
	
corr :: [Double] -> [Double] -> Double
corr x1 x2 = (mean $ map (\(x,y) -> x*y) $ zip y1 y2)/(sqrt ((var x1)*(var x2)))
	where y1 = map (\x -> x - (mean x1)) x1;
		  y2 = map (\x -> x - (mean x2)) x2	

mean :: [Double] -> Double
mean x = (sum x)/(sum $ map (\y -> 1) x)

var :: [Double] -> Double
var x = (mean $ map (\y -> y^2) x) - ((mean x)^2)