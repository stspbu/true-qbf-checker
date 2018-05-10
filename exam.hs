module Exam where

import Prelude
import Data.List (intersect)
import Control.Monad.Cont

data Exp = Val Bool | Var String | Not Exp | And Exp Exp | Or Exp Exp | Impl Exp Exp | ForAll String Exp | Exists String Exp deriving (Eq)
data Tree = Branch Tree Tree | LonelyBranch Tree | Leaf Bool deriving (Eq, Show)

-- just for comfort
(&) :: Exp -> Exp -> Exp
(&) e1 e2 = And e1 e2 
infixl 7 &

(?) :: Exp -> Exp -> Exp
(?) e1 e2 = Or e1 e2 
infixl 6 ?

(%>) :: Exp -> Exp -> Exp
(%>) e1 e2 = Impl e1 e2
infixr 5 %>

-- output
instance Show Exp where
	show (Val a) = show a
	show (Var x) = x
	show (Not e) = "(!" ++ show e ++ ")"
	show (And e1 e2) = "(" ++ show e1 ++ " & " ++ show e2 ++ ")"
	show (Or (Not e1) e2) = "(" ++ show e1 ++ " -> " ++ show e2 ++ ")"
	show (Impl e1 e2) = "(" ++ show e1 ++ " -> " ++ show e2 ++ ")"
	show (Or e1 e2) = "(" ++ show e1 ++ " v " ++ show e2 ++ ")"
	show (ForAll x e) = "V" ++ x ++ show e
	show (Exists x e) = "E" ++ x ++ show e

-- start point
eval :: Exp -> IO()
eval exp = (runCont (eval' exp []) contOut) exp

contOut :: Maybe [(String, Bool)] -> Exp -> IO()
contOut (Just x) exp = do 
		putStrLn "Your QBF is TQBF"
		if x /= []
		then do
			putStrLn $ "E.g. use " ++ show (reverse x)
			putStrLn $ "Then you get: " ++ show (calc exp x)
		else 
			putStrLn "Check it with any replacement"

contOut _ exp = putStrLn "Your QBF is not tautology"

eval' :: Exp -> [(String, Bool)] -> Cont a (Maybe [(String, Bool)])
eval' (ForAll x e) vMap = do
	lBranch <- eval' e ((x, False):vMap)
	rBranch <- eval' e ((x, True):vMap)

	case (lBranch, rBranch) of
		((Just nvMap), (Just _)) -> return $ Just nvMap
		_ -> return Nothing


eval' (Exists x e) vMap = do
	lBranch <- eval' e ((x, False):vMap)
	rBranch <- eval' e ((x, True):vMap)
	case (lBranch, rBranch) of
		(a@(Just _), _) -> return a 
		(_, a@(Just _)) -> return a
		_ -> return Nothing

eval' e vMap = case calc e vMap of 
	ne@(Val True) -> return $ Just (vMap)
	(Val False) -> return Nothing
	e -> if getResult $ check [] [e] then return (Just (vMap)) else return Nothing

-- remove numbers
calc :: Exp -> [(String, Bool)] -> Exp
calc (Not e) m 		= combine $ Not (calc e m)
calc (And e1 e2) m 	= combine $ (calc e1 m) & (calc e2 m)
calc (Or e1 e2) m 	= combine $ (calc e1 m) ? (calc e2 m)
calc (Impl e1 e2) m = combine $ (calc (Not e1) m) ? (calc e2 m)
calc a@(Var v) m 	= case lookup v m of
		Just x -> Val x
		Nothing -> a
calc v@(Val _) m	= v
calc (ForAll x e) m = calc e m -- ignore quantors
calc (Exists x e) m = calc e m -- result will be printed

-- basis
combine :: Exp -> Exp
combine (Not (Val True)) = Val False
combine (Not (Val False)) = Val True

combine (Or (Val True) _) = Val True
combine (Or _ (Val True)) = Val True
combine (Or e (Val False)) = e
combine (Or (Val False) e) = e

combine (And (Val True) e) = e
combine (And e (Val True)) = e
combine (And _ (Val False)) = Val False
combine (And (Val False) _) = Val False
combine e = e

-- исчисление высказываний
isVar :: Exp -> Bool
isVar (Var x) = True
isVar _ = False

check :: [Exp] -> [Exp] -> Tree
check [] [] = Leaf True
check l r 
	| intersect l r /= [] = Leaf True
	| esLeft /= [] = ltSwapper (head esLeft) l r
	| esRight /= [] = rfSwapper (head esRight) l r
	| otherwise = Leaf False
	where 
		esLeft = filter (\x -> not $ isVar x) l
		esRight = filter (\x -> not $ isVar x) r

ltSwapper :: Exp -> [Exp] -> [Exp] -> Tree
ltSwapper (Not x) l r   	= LonelyBranch (check (l `exclude` (Not x)) (x:r)) 
ltSwapper (And x y) l r 	= LonelyBranch (check (x : y : (l `exclude` (x & y))) r) 
ltSwapper (Or x y) l r 		= Branch (check (x : (l `exclude` (x ? y))) r) (check (y : (l `exclude` (x ? y))) r) 
ltSwapper (Impl x y) l r 	= Branch (check (l `exclude` (x %> y)) (x:r)) (check (y : (l `exclude` (x %> y))) r) 

rfSwapper :: Exp -> [Exp] -> [Exp] -> Tree
rfSwapper (Not x) l r   	= LonelyBranch (check (x:l) (r `exclude` (Not x))) 
rfSwapper (Or x y) l r 		= LonelyBranch (check l (x : y : (r `exclude` (x ? y)))) 
rfSwapper (Impl x y) l r 	= LonelyBranch (check (x:l) (y : (r `exclude` (x %> y))))
rfSwapper (And x y) l r 	= Branch (check l (x : (r `exclude` (x & y)))) (check l (y : (r `exclude` (x & y)))) 

exclude :: [Exp] -> Exp -> [Exp]
exclude l e = filter (\x -> x /= e) l

getResult :: Tree -> Bool
getResult (Leaf s) = s
getResult (LonelyBranch t) = getResult t
getResult (Branch l r) = getResult l && getResult r


-- tests
wikiTest = ForAll "x" $ Exists "y" $ Exists "z" $ ((Var "x") ? (Var "z")) & (Var "y")											-- ok y=1, z=1

-- my tests
t1  = Exists "x" $ ForAll "y" $ ((Var "x") ? (Not $ Var "y")) & ((Not $ Var "x") ? (Var "y")) 									-- false
t2  = ForAll "y" $ Exists "x" $ ((Var "x") ? (Not $ Var "y")) & ((Not $ Var "x") ? (Var "y"))									-- ok x=0
t3  = (Var "x") ? (Not (Var "x"))																								-- ok any
t4  = ForAll "x" $ Exists "u" $ (((Var "x") %> (Var "y")) ? (Var "z")) & (Var "x") ? (Var "u") ? (Var "g") 						-- ok u=1
t5  = Val True																														-- ok any
t6  = ForAll "x" $ Var "x"																										-- f 
t7  = Exists "x" $ Var "x"																										-- ok x=1
t8  = ForAll "x" $ ForAll "u" $ Exists "g" $ (((Var "x") %> (Var "y")) ? (Var "z")) & (Var "x") ? (Var "u") ? (Var "g")	 		-- ok g=1
t9  = Exists "u" $ (Var "x") ? (Not (Var "x")) ? (Var "u")																		-- ok any
t10 = Exists "u" $ (Var "x") ? (Var "y") ? (Var "z")																			-- f
t11 = ForAll "x" $ ForAll "u" $ (((Var "x") %> (Var "y")) ? (Var "z")) & (Var "x") ? (Var "u") ? (Val True)						-- ok any
t12 = Exists "x" $ Exists "y" $ (Var "x") & (Var "y") & (Not $ Var "x")															-- f
t13 = ForAll "y" $ ForAll "z" $ Exists "z" $ ForAll "x" $ (Var "x") & (Val False)													-- f
t14 = ForAll "y" $ ForAll "z" $ Exists "z" $ ForAll "x" $ (Val True) ? (Val False)														-- ok any
t15 = ForAll "x" $ ForAll "y" $ Exists "z" $ ForAll "c" $ ForAll "d" $ ForAll "p" $ ((Var "x") %> (Var "y")) %> ((Var "y") %> (Var "z")) %> (((Var "c") %> (Var "d")) ? ((Var "y") %> (Var "p")))		-- ok z = 0
t16 = ForAll "x" $ ForAll "y" $ ForAll "z" $ ForAll "c" $ ForAll "d" $ ForAll "p" $ ((Var "x") %> (Var "y")) %> ((Var "y") %> (Not $ Var "y")) %> (((Var "c") %> (Var "d")) ? ((Var "y") %> (Var "p")))	-- ok any
t17 = ForAll "x" $ ForAll "y" $ ((Var "x") %> (Var "y")) %> (((Var "c") %> (Var "d")) ? ((Var "y") %> (Var "p")))				-- f
t18 = Val False 					
t19 = ForAll "x" $ ForAll "y" $ (Var "x") %> (Var "y") %> (Var "x")
t20 = ForAll "a" $ ForAll "b" $ ForAll "c" $ ((Var "a") %> (Var "b") %> (Var "c")) %> ((Var "a") %> (Var "b")) %> (Var "a") %> (Var "c") 	
t21 = ForAll "x" $ Exists "y" $ ((Var "x") %> (Var "y")) & ((Var "y") %> (Var "x"))
t22 = Exists "y" $ ForAll "x" $ ((Var "x") %> (Var "y")) & ((Var "y") %> (Var "x"))																						-- f
