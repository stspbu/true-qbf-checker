import Exam
import Test.HUnit
import System.Exit

goTest name exp expected = 
	case eval' exp [] of
		(Just (e, x)) -> TestLabel name $ TestCase (assertEqual ("For: " ++ show exp ++ "\nFound: " ++ show x ++ "\nWhich gives tautology: " ++ show (calc e x)) expected True)
		_ -> TestLabel name $ TestCase (assertEqual ("For: " ++ show exp) expected False)

tests = TestList 
	[	goTest "wikiTest" ( ForAll "x" $ Exists "y" $ Exists "z" $ ((Var "x") ? (Var "z")) & (Var "y") ) True 											-- ok y=1, z=1

	,	goTest "t1"  ( Exists "x" $ ForAll "y" $ ((Var "x") ? (Not $ Var "y")) & ((Not $ Var "x") ? (Var "y")) ) False 									-- false
	,	goTest "t2"  ( ForAll "y" $ Exists "x" $ ((Var "x") ? (Not $ Var "y")) & ((Not $ Var "x") ? (Var "y")) ) True									-- ok x=0
	,	goTest "t3"  ( (Var "x") ? (Not (Var "x")) ) True																								-- ok any
	,	goTest "t4"  ( ForAll "x" $ Exists "u" $ (((Var "x") %> (Var "y")) ? (Var "z")) & (Var "x") ? (Var "u") ? (Var "g") ) True 						-- ok u=1
	,	goTest "t5"  ( Val 1 ) True																														-- ok any
	,	goTest "t6"  ( ForAll "x" $ Var "x" ) False																										-- f 
	,	goTest "t7"  ( Exists "x" $ Var "x" ) True																										-- ok x=1
	,	goTest "t8"  ( ForAll "x" $ ForAll "u" $ Exists "g" $ (((Var "x") %> (Var "y")) ? (Var "z")) & (Var "x") ? (Var "u") ? (Var "g") ) True	 		-- ok g=1
	,	goTest "t9"  ( Exists "u" $ (Var "x") ? (Not (Var "x")) ? (Var "u")	) True																		-- ok any
	,	goTest "t10" ( Exists "u" $ (Var "x") ? (Var "y") ? (Var "z") )	False																			-- f
	,	goTest "t11" ( ForAll "x" $ ForAll "u" $ (((Var "x") %> (Var "y")) ? (Var "z")) & (Var "x") ? (Var "u") ? (Val 1) )	True						-- ok any
	,	goTest "t12" ( Exists "x" $ Exists "y" $ (Var "x") & (Var "y") & (Not $ Var "x") ) False														-- f
	,	goTest "t13" ( ForAll "y" $ ForAll "z" $ Exists "z" $ ForAll "x" $ (Var "x") & (Val 0) ) False													-- f
	,	goTest "t14" ( ForAll "y" $ ForAll "z" $ Exists "z" $ ForAll "x" $ (Val 1) ? (Val 0) ) True														-- ok any
	,	goTest "t15" ( ForAll "x" $ ForAll "y" $ Exists "z" $ ForAll "c" $ ForAll "d" $ ForAll "p" $ ((Var "x") %> (Var "y")) %> ((Var "y") %> (Var "z")) %> (((Var "c") %> (Var "d")) ? ((Var "y") %> (Var "p"))) ) True		-- ok z = 0
	,	goTest "t16" ( ForAll "x" $ ForAll "y" $ ForAll "z" $ ForAll "c" $ ForAll "d" $ ForAll "p" $ ((Var "x") %> (Var "y")) %> ((Var "y") %> (Not $ Var "y")) %> (((Var "c") %> (Var "d")) ? ((Var "y") %> (Var "p"))) ) True	-- ok any
	,	goTest "t17" ( ForAll "x" $ ForAll "y" $ ((Var "x") %> (Var "y")) %> (((Var "c") %> (Var "d")) ? ((Var "y") %> (Var "p"))) ) False				-- f
	,	goTest "t18" ( Val 0 ) False 																													-- f
	]

main = do
	results <- runTestTT tests
	if (errors results + failures results == 0)
		then
			exitWith ExitSuccess
		else 
			exitWith (ExitFailure 1)