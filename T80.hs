
import FurnitureResources

statsTest = training
sort x [] = [x]
sort  (o1,x,freq1) ((o2,y,freq2):rest) =
		if freq1 >= freq2 then 
				(o1,x,freq1):(o2,y,freq2):rest
				else
					(o2,y,freq2):(sort (o1,x,freq1) rest)
sort1 [] = []
sort1 (x:xs) = sort x (sort1 xs)

sortMe [] = []

sortMe((tocheck,[right,below]):tail)	= (tocheck, [sort1 right, sort1 below]):(sortMe tail)				


statsList :: [([Char],[[([Char],[Char],Int)]])]
statsList = sortMe (statsHelper training [])


statsHelper [] [] = []

statsHelper (head:tail) stats= generate head (statsHelper tail stats)

findFurnitureUpdate :: [Char] -> [Char] -> [Char] -> [([Char],[[([Char],[Char],Int)]])]-> [([Char],[[([Char],[Char],Int)]])]
findFurnitureUpdate a b c [] = if (c == "right") then [(a, [[(b,c,1)], []])] else [(a, [[], [(b,c,1)]])]


findFurnitureUpdate checkMe anaelgowa pos ( (tocheck,[right,below]) :xs)   | (tocheck==checkMe) = (tocheck,helperOne anaelgowa pos [right,below]):xs
																			    | otherwise = (tocheck,[right,below]):findFurnitureUpdate checkMe anaelgowa pos xs 

helperOne anaelgowa x [right,below] | x == "right" = [(helperRight anaelgowa right)]++[below]
									| otherwise = [right]++[(helperBelow anaelgowa below)]



helperRight anaelgowa [] = [(anaelgowa, "right", 1)]
helperRight anaelgowa ((elem, right, counter):tail) | elem == anaelgowa = ((elem, right, (counter+1)):tail)
												  | otherwise = (elem, right, counter):helperRight anaelgowa tail


helperBelow anaelgowa [] = [(anaelgowa, "below", 1)]
helperBelow anaelgowa ((elem, pos, counter):tail) | elem == anaelgowa = ((elem, pos, (counter+1)):tail)
												  | otherwise = (elem,pos, counter):helperBelow anaelgowa tail 



generate :: [[[Char]]] -> [([Char],[[([Char],[Char],Int)]])] ->[([Char],[[([Char],[Char],Int)]])]

generate [x] statsList = generateHelper x [] statsList 
generate (x:y:xs) statsList = generateHelper x y (generate (y:xs) statsList)  

generateHelper [h] [] statsList = statsList
generateHelper [h] [s] statsList = findFurnitureUpdate h s "below" statsList
generateHelper (h:x:xs) [] statsList = findFurnitureUpdate h x "right" (generateHelper (x:xs) [] statsList)


generateHelper (h:x:xs) (s:y:ys)  statsList= findFurnitureUpdate h x "right"  (findFurnitureUpdate h s "below" (generateHelper (x:xs) (y:ys) statsList))



getFurnStat :: [Char] -> [[([Char],[Char],Int)]]

getFurnStat elem = getFurnStatHelper elem statsList

getFurnStatHelper elem [] = []
getFurnStatHelper elem ((tocheck,[right,below]) :xs)  | elem == tocheck = [right, below] 
							   						  | otherwise = getFurnStatHelper elem xs

loop x z list | z==0 = list
			  | otherwise = loop x (z-1) list++[x]


getPossibleNeighbour :: [([Char],[Char],Int)] -> [([Char],[Char],Int)] -> [Char]

getPossibleNeighbour ((x,y,z):xs) [] = gimmeElem (getLenOfList ((x,y,z):xs) []) (getList ((x,y,z):xs) [])
getPossibleNeighbour ((x,y,z):xs) ((a,b,c):xy) = gimmeElem (getLenOfList ((x,y,z):xs) ((a,b,c):xy)) (getList ((x,y,z):xs) ((a,b,c):xy))

getLenOfList ((x,y,z):xs) [] =  length(getPosHelper xs [] (getPosHelperBase x z []))
getLenOfList ((x,y,z):xs) ((a,b,c):xy) =  length(getPosHelper xs ((a,b,c):xy) (getPosHelperBase x z []))

getList ((x,y,z):xs) [] =  getPosHelper xs [] (getPosHelperBase x z [])
getList ((x,y,z):xs) ((a,b,c):xy) =  getPosHelper xs ((a,b,c):xy) (getPosHelperBase x z [])

getPosHelperBase x z [] = loop x z []   

--should return an element w keda heya gayelha list aslan, okay?
getPosHelper [] [] list = list
getPosHelper ((x,y,z):xs) [] list = putInListKbeerXTimes ((x,y,z):xs) list
getPosHelper ((x,y,z):xs) ((a,b,c):xy) list = putInListKbeerXTimes ((a,b,c):xy) (putInListKbeerXTimes ((x,y,z):xs) list)
--list feeh kol 7aga 


gimmeElem x list = list !! (randomZeroToX x-1) 
--gimmeLen xs = length (xs)

--xs !! n      xs is list and n is pos yeraga3 el gowa n? 
putInListKbeerXTimes [] list = list
putInListKbeerXTimes ((a,b,c):xy) list = putInListKbeerXTimes xy (loop a c list) 


getStatsOfLeft left  = head (getFurnStat left) 
getStatsOfTop top = head (tail (getFurnStat top))
--
--putInGeneralCell left top statsList = getPossibleNeighbour getStatsOfLeft left statsList getStatsOfTop top statsList 
--hayraga3ly esm element el ha7oto fel row bta3 el room 

putInTopRow left  = getPossibleNeighbour (getStatsOfLeft left)  [] 
putInLeftCol top  = getPossibleNeighbour (getStatsOfTop top) [] 

----generalCaseFurnishing  left above statsList = getFurnStat left 

furnishTopRow rowNums elem list | (rowNums) == 0 = list
							    | otherwise = furnishTopRow (rowNums-1) newElem list++[newElem]
							    where newElem = (putInTopRow elem)
							    


furnishLeftCol elemTop list = list++[newElem] where newElem = (putInLeftCol elemTop) 

furnishGeneral left top list = list++[newElem]	where newElem = getPossibleNeighbour (getStatsOfLeft left) (getStatsOfTop top)					

--furnish yarab dy el bt3mely awel wa7da fel left xD xD
furnishYarab (x:xs) = (x:xs):[furnishLeftCol x []] 

merge :: Ord a => [a] -> [a] -> [a]
merge x [] = x
merge [] x = x
merge (x:xs) (y:ys) = x:merge xs (y:ys)

--getLeftAndTop [[x,y,z],[a]] =[x,y,z]:b 
--								where b = [merge [a] (furnishGeneral (a) (y) [])]
getLeftAndTop [[x,y,z],xs] n =[x,y,z]:b 
							  where b = [merge xs (furnishGeneral (xs !! n) (y) [])]


--furnishRoom :: Int -> [Char] -> [[[Char]]]

furnishRoom num firstElem = callHaga num (furnishYarab (furnishTopRow num firstElem []))



--furnishRoom num firstElem = callHaga num (furnishYarab (furnishTopRow num firstElem []))

--furnishRoomOne num firstElem =furnishYarab (furnishTopRow num firstElem []))

--furnishRoom num firstElem numChange| numChange==0 = callHaga num (furnishYarab (furnishTopRow num firstElem [])) 
--								   |otherwise= furnishRoom num 

--call haga dy 3andy two lists keda, law num > 2 yeb2a ha3mel eeeeeh 
numGrTwo num list = callHaga num (furnishYarab list)

--hayerga3ly menha list, ha-call bta3et eh..el left awel wa7da  

haga n list c   | c==n = list
             	| otherwise = haga n list1 (c+1)
				    where list1 = (getLeftAndTop list c)


callHaga rows list = haga (rows-1) list 0 

--furnishLeftCol elemTop list = list++[newElem] where newElem = (putInLeftCol elemTop) 

--furnishLeftCol elemTop list = furnishGeneralBase( newElem list++[newElem]) where newElem = (putInLeftCol elemTop) 

--furnishGeneralBase left list= furnishGeneral left top list where top = 

--furnishGeneral left top list = list++[newElem]	where newElem = getPossibleNeighbour (getStatsOfLeft left) (getStatsOfTop top)					

--furnishRoom :: Int -> [Char] -> [[[Char]]]

--furnishRoom num elem = furnishGeneralBase (furnishTopRow num elem [])



--furnishLeftCol elemTop list = furnishGeneralBase( newElem list++[newElem]) where newElem = (putInLeftCol elemTop) 


--furnishGeneral left top = [newElem]	where newElem = getPossibleNeighbour (getStatsOfLeft left) (getStatsOfTop top)					

--getLeftAndTop ((x:y:z):(a:b:c):xs) = ((x:y:z):([a]:(furnishGeneral [a] [y] [])))
--tester (a) = concat (merge (a) (furnishGeneral "e" "table" [] ))
--getLeftAndTop ((x:y:z):(a)) = merge [a] (furnishGeneral (a) (y) [])

--getLeftAndTop ((x:y:z):(a)) = concat (merge a a) 