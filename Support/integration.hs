module Support.Integration (trapezoidal, simpson, adaptive_simpson, monte_carlo) where
	
import Support.Probability
import Support.Misc
import System.Random
import Data.List

fromInt = fromInteger . toInteger

trapezoidal :: (Double -> Double) -> (Double,Double) -> Double -> Double
trapezoidal f (a,b) dx
	| (b-a) > dx 		= (int a (a+dx)) + (trapezoidal f (a+dx, b) dx)
	| (b-a) < 0.0 		= trapezoidal f (b,a) dx
	| otherwise 		= int a b
	where int x1 x2 = (x2 - x1)*(f (0.5*(x1 + x2)))

simpson :: (Double -> Double) -> (Double,Double) -> Double -> Double
simpson f (a,b) dx
	| (b-a) > dx 		= (simpson f (a, a+dx) dx) + (simpson f (a+dx, b) dx)
	| (b-a) < 0.0 		= simpson f (b,a) dx
	| otherwise 		= (1/6)*(b-a)*((f a) + 4*(f (0.5*(a+b))) + (f b))

adaptive_simpson :: (Double -> Double) -> (Double,Double) -> Double -> Double
adaptive_simpson f (a,b) tol
	| (abs diff) <= 15*tol 		= left + right + diff/15
	| otherwise 				= s1 + s2
	where s1 = (adaptive_simpson f (a,c) (0.5*tol));
		  s2 = (adaptive_simpson f (c,b) (0.5*tol));
		  diff = (left + right - prev);
		  prev = simpson f (a,b) (b-a);
		  left = simpson f (a,c) (c-a);
		  right = simpson f (c,b) (b-c);
		  c = 0.5*(a+b)

monte_carlo_helper :: ([Double] -> Double) -> ([Double],[Double]) -> Int -> [Double] -> Double
monte_carlo_helper f (a,b) n rand
	| max_fb < 0 		= (max_fb - min_fb)*area*fraction - max_fb*area
	| min_fb > 0		= (max_fb - min_fb)*area*fraction + min_fb*area
	| otherwise 		= (max_fb - min_fb)*area*fraction
	where fraction = (fromInt $ p - q)/(fromInt n);
	      p = count (map (\(x,y) -> and [(x > y), (y > 0)]) $ zip fs zs);
		  q = count (map (\(x,y) -> and [(x < y), (y < 0)]) $ zip fs zs);
		  zs = map (g min_fb max_fb) $ head grouped_r;
		  fs = map f xs;
		  xs = [map (\(x,a0,b0) -> g a0 b0 x) $ zip3 rx a b | rx <- (transpose $ tail grouped_r)];
		  grouped_r = groupEvery n rand;
		  g a0 b0 x = (b0-a0)*x + a0;
		  fb = map f $ get_corners (a,b);
		  area = prod (map (\(x,y) -> y - x) $ zip a b);
		  max_fb = maximum fb;
		  min_fb = minimum fb;

monte_carlo :: ([Double] -> Double) -> ([Double],[Double]) -> Int -> Int -> StdGen -> Double
monte_carlo f (a,b) n m gen = sum (map (\(x,r) -> monte_carlo_helper f x n r) xr)
	where xr = zip (get_grid (a,b) m) $ groupEvery (n*(1+w)) $ take ((n*(1+w))*m^w) $ uniform gen;
	      w = length a

get_corners :: ([Double],[Double]) -> [[Double]]
get_corners (a,b) = [f x | x <- (get_std_corners (length a))]
	where f x = map (\(x,y,z) -> z + x*y) $ zip3 x ls a;
		  ls = map (\(x,y) -> y - x) (zip a b)
		
get_std_corners :: Int -> [[Double]]
get_std_corners dim = transpose [flatten $ flatten $ mat m | m <- [0..(dim-1)]]
	where mat m = replicate (2^(dim-m-1)) (transpose (replicate (2^m) $ [0,1]));

get_grid :: ([Double],[Double]) -> Int -> [([Double],[Double])]
get_grid (a,b) n = [(f x, f (step x)) | x <- (get_std_grid (length a) n)]
	where f x = map (\(x,y,z) -> z + x*y) $ zip3 x ls a;
		  ls = map (\(x,y) -> y - x) (zip a b);
		  step = map (\x -> x + (1/(fromInt n)))

get_std_grid :: Int -> Int -> [[Double]]
get_std_grid dim n = transpose [flatten $ flatten $ mat m | m <- [0..(dim-1)]]
	where mat m = replicate (n^(dim-m-1)) (transpose (replicate (n^m) $ vec n));
		  vec n = map (\x -> (fromInt x)*(1.0/(fromInt n))) [0..(n-1)]

prod :: [Double] -> Double
prod x = foldl (*) 1 x

flatten :: [[a]] -> [a]
flatten x = foldl (++) [] x

