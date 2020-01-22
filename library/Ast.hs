{-# LANGUAGE TemplateHaskell, QuasiQuotes, LambdaCase, ImpredicativeTypes, RankNTypes, ScopedTypeVariables #-}

-- | ast manipulation
module Ast (skeleton, hasHoles, fnOutputs, filterTypeSigIoFns, fillHoles, fillHole, holeExpr, numAstNodes, genFn, genFns, letRes) where

import Language.Haskell.Exts.Syntax ( Type(..), Exp(..), QName(..), SpecialCon(..) )
import Language.Haskell.Exts.Parser ( ParseResult, parse, fromParseResult )
import Language.Haskell.Interpreter (Interpreter, lift, typeChecks, typeOf)
import Data.List (nub, delete, minimumBy, isInfixOf, partition )
import Control.Monad (forM, forM_)
import Data.HashMap.Lazy (HashMap, fromList, (!), elems, mapWithKey)
import Data.Maybe (catMaybes)
-- import TcHoleErrors (findValidHoleFits)
-- import VarEnv (TidyEnv(..))
-- import Var (TyCoVar(..), TyCoVarSet(..))
-- import TcRnTypes (Implication(..), Ct(..))
-- import UniqSet (nonDetEltsUniqSet)
-- import Debug.Dump (d)
import Types
import FindHoles
import Hint
import Utility (pick, pp)
import Config (Strategy(..), strategy, nestLimit, maxWildcardDepth)

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

-- | generate potential programs filling any holes in a given expression using some building blocks 
fillHoles :: Int -> HashMap String Tp -> Expr -> Interpreter [Expr]
fillHoles maxHoles = fillHoles_ maxHoles 0

-- | recursive helper for fillHoles
fillHoles_ :: Int -> Int -> HashMap String Tp -> Expr -> Interpreter [Expr]
fillHoles_ maxHoles paramCount block_types expr = do
    (partial, candidates) <- fillHole paramCount block_types expr
    -- say $ "candidates: " ++ show (pp <$> candidates)
    -- say $ "partial: " ++ show (pp <$> partial)
    -- TODO: get paramCount out so we can update it in a useful manner here
    rest <- case maxHoles of
                0 -> return []
                _ -> mapM (fillHoles_ (maxHoles - 1) paramCount block_types) partial
    return $ candidates ++ concat rest

-- | filter building blocks to those matching a hole in the (let-in) expression, and get the results Exprs
fillHole :: Int -> HashMap String Tp -> Expr -> Interpreter ([Expr], [Expr])
fillHole paramCount block_types expr = do
    -- find a hole
    let hole_lenses = findHolesExpr expr
    -- TODO: let a learner pick a hole
    let hole_lens = head hole_lenses
    -- let hole_getter :: Expr -> Expr = fst hole_lens
    let hole_setter :: Expr -> Expr -> Expr = snd hole_lens
    -- let hole :: Expr = hole_getter expr
    -- let tp :: Tp = holeType hole
    -- let in_scope_vars :: ? = []?  -- TODO
    let together :: HashMap String Tp = block_types  -- + in_scope_vars
    let generated :: HashMap String [Expr] = mapWithKey (genHoledVariants maxWildcardDepth) together
    let expr_blocks :: [Expr] = case strategy of
            -- when using currying we will allow any level of application
            UseCurrying -> concat $ elems generated
            -- when using lambdas instead of currying we will only allow complete application of any (nested) function
            UseLambdas -> elems $ last <$> generated
    let inserted = hole_setter expr <$> expr_blocks
    let (partial, complete) = partition hasHoles inserted
    -- TODO: given a type, filter partial programs by type-check
    candidates <- filterCandidatesByCompile complete
    -- candidates <- case tp of
    --             -- TODO: when implementing UseLambdas also capture wildcard type here
    --             TyFun _l tpIn tpOut -> case strategy of
    --                 -- -- fill function-typed holes with a lambda
    --                 -- UseLambdas -> let
    --                 --             varName = "p" ++ show paramCount
    --                 --             src = "\\" ++ varName ++ " -> let _unused = (" ++ varName ++ " :: " ++ pp tpIn ++ ") in (_ :: " ++ pp tpOut ++ ")"
    --                 --             expr_ = fromParseResult (parse src :: ParseResult Expr)
    --                 --         -- TODO: get the type of the hole
    --                 --         in fillHole (paramCount + 1) block_types $ hole_setter expr expr_
    --             --     UseCurrying -> return $ filterCandidatesByType tp complete
    --             -- _ -> return $ filterCandidatesByType tp complete
    --                 UseCurrying -> filterCandidatesByCompile complete
    --             _ -> filterCandidatesByCompile complete
    -- standardizing reductions (hlint?)
    -- - eta reduction: pointfree -- only relevant with lambdas
    -- https://github.com/ndmitchell/hlint/blob/56b9b45545665113d277493431b1430e41a3e288/src/Hint/Lambda.hs#L101
    return (partial, candidates)

-- | as any block/parameter may be a (nested) function, generate variants with holes curried in to get all potential return types
genHoledVariants :: Int -> String -> Tp -> [Expr]
genHoledVariants maxDepth k tp = genHoledVariants_ maxDepth tp $ var k

-- | internal helper of `genHoledVariants` used for recursion
genHoledVariants_ :: Int -> Tp -> Expr -> [Expr]
genHoledVariants_ maxDepth tp expr =
    let holed = app expr . expTypeSig holeExpr
    in expr : case tp of
    TyFun _l a b -> genHoledVariants_ maxDepth b $ holed a
    TyWildCard _l _maybeName -> case maxDepth of
                0 -> []
                _ -> genHoledVariants_ (maxDepth - 1) tp $ holed wildcard
    _ -> []

-- | filter candidates by trying them in the interpreter to see if they blow up. using the GHC compiler instead would be better.
filterCandidatesByCompile :: [Expr] -> Interpreter [Expr]
filterCandidatesByCompile exprs = fmap catMaybes $ sequence $ fitExpr <$> exprs

-- TODO: ensure blocks are in some let construction!
-- | check if a candidate fits into a hole by just type-checking the result through the interpreter.
-- | this approach might not be very sophisticated, but... it's for task generation, I don't care if it's elegant.
fitExpr :: Expr -> Interpreter (Maybe Expr)
fitExpr expr = do
    checks <- typeChecks $ pp expr
    ok <- if not checks then return False else do
        -- use typeOf to filter out non-function programs
        tp_str <- typeOf $ pp expr
        let tp = fromParseResult (parse tp_str :: ParseResult Tp)
        return $ case tp of
            TyFun _l _a _b -> True
            _ -> False
    return $ if ok then Just expr else Nothing

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

-- Interpreter.typeOf top expression
-- for each App node:
--      -- get the type signature by Interpreter.typeOf on the function
--      -- using the expected type for the expression, compare with the function signature to potentially fill in (part of) its type variables
--      for the function's parameter slot, assign the substituted input type for the ExprTypeSig

-- -- | generate a function type, to then generate functions matching this type
-- genFnType :: IO Tp -- TyFun
-- genFnType = randomFnType True True nestLimit [] tyVarCount
--     where tyVarCount :: Int = 0 -- TODO: is this okay?

-- -- | generate a parameter type, to then generate functions taking this input
-- genFnInType :: IO Tp -- TyFun
-- genFnInType = randomType True True nestLimit [] tyVarCount
--     where tyVarCount :: Int = 0 -- TODO: is this okay?

-- | _ :: (_ -> _)
anyFn :: Expr
anyFn = skeleton $ tyFun wildcard wildcard

-- | get the body of a let-in expression
letRes :: Expr -> Expr
letRes = \case
    (Let _l _binds xp) -> xp

-- | just directly generate any functions in batch, and see what types end up coming out.
-- | in this approach, further nodes can impose new constraints on the type variables introduced in earlier nodes.
genFns :: Int -> HashMap String Tp -> HashMap String Expr -> Interpreter [Expr]
genFns maxHoles block_types block_asts =
    fillHoles maxHoles block_types $ letIn block_asts anyFn

-- | just directly sample a generated function, and see what types end up coming out.
genFn :: Int -> HashMap String Tp -> HashMap String Expr -> Interpreter Expr
genFn maxHoles block_types block_asts = do
    -- -- TODO: save duplicate effort of finding holes
    -- while hasHoles (genFn_ fn_types) expr
    candidates <- genFns maxHoles block_types block_asts
    lift $ pick candidates
-- genFn = lift . fmap pick . genFns

-- | check if an expression contains holes
hasHoles :: Expr -> Bool
hasHoles = not . null . findHolesExpr

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
    -- case length fns of
    --     1 -> return ()
    --     _ -> say $ "deduping equivalent fns: " ++ show fns
    let minByMap fn = minimumBy $ \ a b -> compare (fn a) (fn b)
    -- TODO: keep not just the function with the fewest number of AST nodes, but the more generic one (most type variable names/occurrences) if possible
    let shortest = minByMap (numAstNodes . (!) fn_asts) fns
    let rest = delete shortest fns
    forM_ rest $ \fn ->
        -- say $ "dropping " ++ fn ++ " for terser equivalent " ++ shortest
        say $ fn ++ " -> " ++ shortest
    return shortest

-- | hole `_` as an AST Expr
holeExpr :: Expr
holeExpr = Var l $ Special l $ ExprHole l

-- | make a typed hole for a type
skeleton :: Tp -> Expr
skeleton = ExpTypeSig l holeExpr
