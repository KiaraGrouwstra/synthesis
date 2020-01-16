{-# LANGUAGE TemplateHaskell, QuasiQuotes, LambdaCase, ImpredicativeTypes, RankNTypes, ScopedTypeVariables #-}

-- | ast manipulation
module Ast (skeleton, fnOutputs, filterTypeSigIoFns, fillHole, holeExpr, numAstNodes) where

import Language.Haskell.Exts.Pretty ( Pretty, prettyPrint )
import Language.Haskell.Exts.Syntax ( Type(..), Exp(..), QName(..), SpecialCon(..) )
-- , Exp
import Language.Haskell.Exts.Parser ( ParseResult, parse, fromParseResult )
import Language.Haskell.Interpreter (Interpreter, lift)
import Data.List (nub, delete, minimumBy)
import Control.Monad (forM, forM_)
import Data.HashMap.Lazy (HashMap, fromList, (!), elems, mapWithKey)
import Data.Maybe (catMaybes)
import Language.Haskell.Interpreter (Interpreter, typeChecks, typeChecksWithDetails)
-- import TcHoleErrors (findValidHoleFits)
-- import VarEnv (TidyEnv(..))
-- import Var (TyCoVar(..), TyCoVarSet(..))
-- import TcRnTypes (Implication(..), Ct(..))
-- import UniqSet (nonDetEltsUniqSet)
-- import Debug.Dump (d)
import Types
import FindHoles
import Hint
import Config (Strategy(..), strategy)

-- fillHoleGhc :: HashMap String Tp -> Expr -> IO [Expr]
-- fillHole block_types expr = do
--     -- tyCoVarsOfType :: Type -> TyCoVarSet
--     -- tyCoVarsOfTypes :: [Type] -> TyCoVarSet
--     -- tyCoVarsOfCt :: Ct -> TcTyCoVarSet
--     -- tyCoVarsOfWC :: WantedConstraints -> TyCoVarSet
--     -- let tycovarset :: TyCoVarSet = 

--     -- Type or Coercion Variables
--     -- let tycovars :: [TyCoVar] = nonDetEltsUniqSet tycovarset
--     let tycovars :: [TyCoVar] = []

--     -- The tidy_env for zonking (i.e. instantiate type variables): used for getLocalBindings, zonking subs / hole type
--     tidyEnv :: TidyEnv <- lift $ tcInitOpenTidyEnv tycovars

--     -- implication: Implication <- newImplication
--         -- ic_tclvl :: TcLevel	     -- no sensible default
--         -- ic_skols :: [TcTyVar]
--         -- ic_info :: SkolemInfo	 -- no sensible default
--         -- ic_telescope :: Maybe SDoc
--         -- ic_given :: [EvVar]
--         -- ic_no_eqs :: Bool
--         -- ic_env :: Env TcGblEnv TcLclEnv
--         -- ic_wanted :: WantedConstraints
--         -- ic_binds :: EvBindsVar	 -- no sensible default
--         -- ic_need_inner :: VarSet
--         -- ic_need_outer :: VarSet
--         -- ic_status :: ImplicStatus
--     let implications :: [Implication] = []

--     -- data CtEvidence
--         -- CtGiven	 
--         --     ctev_pred :: TcPredType	 
--         --     ctev_evar :: EvVar	 
--         --     ctev_loc :: CtLoc	 
--         -- CtWanted	 
--         --     ctev_pred :: TcPredType	 
--         --     ctev_dest :: TcEvDest	 
--         --     ctev_nosh :: ShadowInfo	 
--         --     ctev_loc :: CtLoc	 
--         -- CtDerived	 
--         --     ctev_pred :: TcPredType	 
--         --     ctev_loc :: CtLoc	 
--     -- mkNonCanonical :: CtEvidence -> Ct
--     -- mkIrredCt :: CtEvidence -> Ct
--     -- mkInsolubleCt :: CtEvidence -> Ct
--     -- data Ct
--     --     CDictCan	 
--     --         cc_ev :: CtEvidence	 
--     --         cc_class :: Class	 
--     --         cc_tyargs :: [Xi]	 
--     --         cc_pend_sc :: Bool	 
--     --     CIrredCan	 
--     --         cc_ev :: CtEvidence	 
--     --         cc_insol :: Bool	 
--     --     CTyEqCan	 
--     --         cc_ev :: CtEvidence	 
--     --         cc_tyvar :: TcTyVar	 
--     --         cc_rhs :: TcType	 
--     --         cc_eq_rel :: EqRel	 
--     --     CFunEqCan	 
--     --         cc_ev :: CtEvidence	 
--     --         cc_fun :: TyCon	 
--     --         cc_tyargs :: [Xi]	 
--     --         cc_fsk :: TcTyVar	 
--     --     CNonCanonical	 
--     --         cc_ev :: CtEvidence	 
--     --     CHoleCan	 
--     --         cc_ev :: CtEvidence	 
--     --         cc_hole :: Hole	 
--     --     CQuantCan QCInst
--     let constraints :: [Ct] = []
--     -- let constraint :: Ct = 

--     (tidyenv_, sdoc) <- lift $ findValidHoleFitsSource tidyEnv implications constraints constraint
--     print sdoc
--     return []

-- | filter building blocks to those matching a hole in the expression, and get the results Exprs
fillHole :: Int -> HashMap String Tp -> Expr -> Interpreter [Expr]
fillHole paramCount block_types expr = do
    -- say $ show [d| block_types |]
    -- say "block_types"
    -- say $ show block_types
    -- find a hole
    let hole_lenses = findHolesExpr expr
    -- TODO: let a learner pick a hole
    let hole_lens = head hole_lenses
    let hole_getter :: Expr -> Expr = fst hole_lens
    let hole_setter :: Expr -> Expr -> Expr = snd hole_lens
    let hole :: Expr = hole_getter expr
    say ("hole: " ++ prettyPrint hole)
    let tp :: Tp = holeType hole
    say ("tp: " ++ prettyPrint tp)
    -- let in_scope_vars :: ? = []?  -- TODO
    let together :: HashMap String Tp = block_types  -- + in_scope_vars
    let generated :: HashMap String [Expr] = mapWithKey genHoledVariants together
    say ("generated: " ++ show (fmap prettyPrint <$> generated))
    let expr_blocks :: [Expr] = case strategy of
            -- when using currying we will allow any level of application
            UseCurrying -> concat $ elems generated
            -- when using lambdas instead of currying we will only allow complete application of any (nested) function
            UseLambdas -> elems $ last <$> generated
    say ("expr_blocks" ++ show (fmap prettyPrint expr_blocks))
    -- fits <- 
    case tp of
                TyFun _l tpIn tpOut -> case strategy of
                    -- fill function-typed holes with a lambda
                    UseLambdas -> let
                                varName = "p" ++ show paramCount
                                src = "\\" ++ varName ++ " -> let _unused = (" ++ varName ++ " :: " ++ prettyPrint tpIn ++ ") in (_ :: " ++ prettyPrint tpOut ++ ")"
                                expr_ = fromParseResult (parse src :: ParseResult Expr)
                            -- TODO: get the type of the hole
                            in fillHole (paramCount + 1) block_types $ hole_setter expr expr_
                --     UseCurrying -> return $ filterCandidatesByType tp expr_blocks
                -- _ -> return $ filterCandidatesByType tp expr_blocks
                    UseCurrying -> filterCandidatesByCompile hole_setter expr expr_blocks
                _ -> filterCandidatesByCompile hole_setter expr expr_blocks
    -- enumerate hole fits
    -- standardizing reductions (hlint?)
    -- - eta reduction: pointfree -- only relevant with lambdas
    -- https://github.com/ndmitchell/hlint/blob/56b9b45545665113d277493431b1430e41a3e288/src/Hint/Lambda.hs#L101
    -- - beta reduction: pre-eval any subtree without unbound variables... won't apply?
    -- return fits

-- | as any block/parameter may be a (nested) function, generate variants with holes curried in to get all potential return types
genHoledVariants :: String -> Tp -> [Expr]
genHoledVariants k tp = genHoledVariants_ tp $ varNode k

-- | internal helper of `genHoledVariants` used for recursion
genHoledVariants_ :: Tp -> Expr -> [Expr]
genHoledVariants_ tp expr = expr : case tp of
    TyFun _l _a b -> genHoledVariants_ b $ App l expr holeExpr
    _ -> []

-- | filter candidates by trying them in the interpreter to see if they blow up. using the GHC compiler instead would be better.
filterCandidatesByCompile :: (Expr -> Expr -> Expr) -> Expr -> [Expr] -> Interpreter [Expr]
filterCandidatesByCompile setter expr expr_blocks = fmap catMaybes $ sequence $ fitExpr setter expr <$> expr_blocks

-- TODO: ensure blocks are in some let construction!
-- | check if a candidate fits into a hole by just type-checking the result through the interpreter.
-- | this approach might not be very sophisticated, but... it's for task generation, I don't care if it's elegant.
fitExpr :: (Expr -> Expr -> Expr) -> Expr -> Expr -> Interpreter (Maybe Expr)
fitExpr setter expr candidate = do
    -- say $ "expr: " ++ prettyPrint expr
    say $ "candidate: " ++ prettyPrint candidate
    let xpr = setter expr candidate
    let cand_str = prettyPrint xpr
    say $ "substituted: " ++ cand_str
    -- checks <- typeChecks $ prettyPrint xpr
    -- return $ if checks then Just xpr else Nothing
    checks <- typeChecksWithDetails cand_str
    case checks of
        Left ghcErrors -> do
            -- say $ show ghcErrors
            return Nothing
        Right str -> do
            say str
            return $ Just xpr

-- -- https://github.com/ghc/ghc/blob/master/compiler/typecheck/TcHoleErrors.hs
-- -- findValidHoleFits: getLocalBindings/tcFilterHoleFits: tcCheckHoleFit
-- -- | find potential fits among variables/blocks either directly or any level of return type of function blocks
-- filterCandidatesByType :: Tp -> HashMap String Tp -> [Expr]
-- filterCandidates tp block_types = catMaybes $ elems $ fitType tp <$> block_types

-- | check if two types fit, and if so, return an expression for this candidate
-- fitType :: Tp -> Tp -> Maybe Expr
-- fitType tp block_type = maybe_fit
--     where
--         maybe_fit = Nothing

-- -- honestly I guess there are a few ways to generate potential benchmark/training functions...
-- -- tl;dr tho they all need some version of fillHole up...

-- -- | generate a function type, to then generate functions matching this type
-- genFnType :: IO Tp -- TyFun
-- genFnType = randomFnType True True nestLimit [] tyVarCount
--     where tyVarCount :: Int = 0 -- TODO: is this okay?

-- -- | just directly generate any function, and see what types end up coming out
-- genFnFromType :: IO Expr
-- genFnFromType = do
--     fnType <- genFnType
--     return ()

-- -- | generate a parameter type, to then generate functions taking this input
-- genFnInType :: IO Tp -- TyFun
-- genFnInType = randomType True True nestLimit [] tyVarCount
--     where tyVarCount :: Int = 0 -- TODO: is this okay?

-- -- | just directly generate any function, and see what types end up coming out
-- genFnFromInType :: IO Expr
-- genFnFromInType = do
--     inType <- genFnInType
--     return ()

-- -- | just directly generate any function, and see what types end up coming out
-- genFn :: IO Expr
-- genFn = do
--     return ()

-- | given sample inputs by type and type instantiations for a function, get its in/out pairs (by type)
fnOutputs :: HashMap String String -> HashMap String String -> String -> [String] -> Interpreter (HashMap String String)
fnOutputs fn_bodies instantiation_inputs k instantiations = let
            fn_str = fn_bodies ! k
            inputs = (!) instantiation_inputs <$> instantiations
        in
            fromList . zip instantiations <$> mapM (fnIoPairs fn_str) inputs

-- | number of AST nodes in an Expr
numAstNodes :: Expr -> Int
numAstNodes = foldr (\ _node acc -> acc + 1) 0

-- | find equivalent functions (by type and then input/output) and keep the shortest ones
filterTypeSigIoFns :: HashMap String Expr -> HashMap String (HashMap String [String]) -> Interpreter (HashMap String (HashMap String String))
filterTypeSigIoFns fn_asts type_sig_io_fns = forM type_sig_io_fns $ mapM $ \fns -> do
    case length fns of
        1 -> return ()
        _ -> say $ "comparing equivalent fns " ++ show fns
    let minByMap fn = minimumBy $ \ a b -> compare (fn a) (fn b)
    let shortest = minByMap (numAstNodes . (!) fn_asts) fns
    let rest = delete shortest fns
    forM_ rest $ \fn ->
        say $ "dropping " ++ fn ++ " for terser equivalent " ++ shortest
    return shortest

-- | hole `_` as an AST Expr
holeExpr :: Expr
holeExpr = Var l $ Special l $ ExprHole l

-- | make a typed hole for a type
skeleton :: Tp -> Expr
skeleton = ExpTypeSig l holeExpr
