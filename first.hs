canSurvive pos n | n <= 2 = True
		 | pos == 3 = False
		 | pos > 3 = canSurvive (pos-3) (n-1)
		 | pos < 3 = canSurvive (n + pos - 3) (n-1)


longestChain n | n == 1 = 1
	       | otherwise = max (longestChain (n-1)) (chainLength n)

chainLength n | n == 1 = 1
	      | even n = 1 + chainLength (div n 2)
	      | odd n = 1 + chainLength ( 3 * n + 1)


