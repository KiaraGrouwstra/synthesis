{-# LANGUAGE LambdaCase, ImpredicativeTypes, RankNTypes #-}

-- | ast manipulation
module Ast (typeNode, skeleton, fnOutputs, instantiateFnTypes, filterTypeSigIoFns) where

import Language.Haskell.Exts.Pretty ( prettyPrint )
import Language.Haskell.Exts.Syntax ( Type(..), Exp )
import Language.Haskell.Exts.Parser ( ParseResult, parse, fromParseResult )
import Language.Haskell.Interpreter (Interpreter, lift)
import Data.List (nub, delete, minimumBy)
import Control.Monad (forM, forM_)
import Data.HashMap.Lazy (HashMap, fromList, (!))
import Types
import FindHoles
import Utility
import Hint
import Control.Lens

maxInstances :: Int = 5  -- may get less after nub filters out duplicate type instances
nestLimit :: Int = 0 -- high values make for big logs while debugging...

data Strategy = UseLambdas | UseCurrying

strategy :: Strategy
strategy = UseLambdas

-- https://github.com/ghc/ghc/blob/master/compiler/typecheck/TcHoleErrors.hs
-- findValidHoleFits: getLocalBindings/tcFilterHoleFits: tcCheckHoleFit
fillHole :: Int -> Expr -> IO [Expr]
fillHole paramCount expr = do
    -- find a hole
    let holeLenses = findHolesExpr expr
    -- TODO: let a learner pick a hole
    let holeLens = head holeLenses
    let holeGetter :: (Expr -> Const Expr Expr) -> Expr -> Const Expr Expr = fst holeLens
    let holeSetter :: (Expr -> Identity Expr) -> Expr -> Identity Expr = snd holeLens
    let hole :: Expr = view holeGetter expr
    -- find a way to replace this hole in the ast
    let tp :: Tp = holeType hole
    let fits = case tp of
                TyFun _l tpIn tpOut -> case strategy of
                    -- fill function-typed holes with a lambda
                    UseLambdas -> let
                                varName = "p" ++ show paramCount
                                src = "\\" ++ varName ++ " -> let _unused = (" ++ varName ++ " :: " ++ prettyPrint tpIn ++ ") in (_ :: " ++ prettyPrint tpOut ++ ")"
                                expr_ = fromParseResult (parse src :: ParseResult Expr)
                            -- TODO: get the type of the hole
                            in set holeSetter expr expr_
                    -- TODO: for functions find potential fits either using return types or any level of return type of the function
                    -- UseCurrying -> []
                -- TODO: find potential fits among variables/blocks
                -- _ -> []
    -- -- enumerate hole fits
    -- -- standardizing reductions (hlint?)
    -- -- - eta reduction: pointfree -- only relevant with lambdas
    -- -- https://github.com/ndmitchell/hlint/blob/56b9b45545665113d277493431b1430e41a3e288/src/Hint/Lambda.hs#L101
    -- -- - beta reduction: pre-eval any subtree without unbound variables... won't apply?
    -- return fits
    return []

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

fnOutputs :: HashMap String String -> HashMap String String -> String -> [String] -> Interpreter (HashMap String String)
fnOutputs fn_bodies instantiation_inputs k instantiations = let
            fn_str = fn_bodies ! k
            inputs = (!) instantiation_inputs <$> instantiations
        in
            fromList . zip instantiations <$> mapM (handleInTp fn_str) inputs

instantiateFnTypes :: HashMap String String -> Interpreter (HashMap String [(Tp, Tp)])
instantiateFnTypes fn_type_strs = do
    let fn_types :: HashMap String Tp = (\type_str -> fromParseResult (parse type_str :: ParseResult Tp)) <$> fn_type_strs
    -- fn_in_types :: HashMap String Tp <- mapM fnInTp fn_type_strs
    -- let in_types :: [Tp] = nub $ elems fn_in_types
    -- let str_in_types :: [String] = prettyPrint <$> in_types
    -- let fn_str_in_types :: HashMap String String = prettyPrint <$> fn_in_types
    fill_types :: [Tp] <- nub . flatten <$> lift (genTypes nestLimit maxInstances)
    -- in_type_instantiations_ :: [Item Tp] <- lift $ mapM instantiateType in_types
    let fn_type_instantiations :: HashMap String [Tp] = instantiateTypes fill_types <$> fn_types
    -- let fn_str_type_instantiations :: HashMap String [String] = fmap prettyPrint <$> fn_type_instantiations
    let fn_io_type_instantiations :: HashMap String [(Tp, Tp)] = fmap fnTypeIO <$> fn_type_instantiations
    return fn_io_type_instantiations

filterTypeSigIoFns :: HashMap String String -> HashMap String (HashMap String [String]) -> Interpreter (HashMap String (HashMap String String))
filterTypeSigIoFns fn_bodies type_sig_io_fns = forM type_sig_io_fns $ mapM $ \fns -> do
    case length fns of
        1 -> return ()
        _ -> say $ "comparing equivalent fns " ++ show fns
    let minByMap fn = minimumBy $ \ a b -> compare (fn a) (fn b)
    -- TODO: compare by AST nodes, not fn body string length!
    let shortest = minByMap (length . (!) fn_bodies) fns
    let rest = delete shortest fns
    forM_ rest $ \fn ->
        say $ "dropping " ++ fn ++ " for terser equivalent " ++ shortest
    return shortest

-- ast stuff

-- -- can't get TypeRep for polymorphic types
-- skeleton :: TypeRep -> Expr
-- skeleton rep = expr
--     where
--         io = typeRepArgs rep
--         hole = Var l $ Special l $ ExprHole l
--         i = typeNode . show $ head io
--         o = typeNode . show $ last io
--         tp_fn = TyFun l i o
--         expr = ExpTypeSig l hole tp_fn

-- | given a string representation of a function type, create a skeletal
-- | AST node representing the function consisting of a hole
skeleton :: String -> Expr
skeleton fn_tp_str = expr
    where
        src = "_ :: " ++ fn_tp_str
        expr = fromParseResult (parse src :: ParseResult Expr)
