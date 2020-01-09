-- {-# LANGUAGE ScopedTypeVariables #-}
-- TemplateHaskell, QuasiQuotes

-- | An example module.
module Example (main) where

import Language.Haskell.Exts.Pretty ( prettyPrint )
import Language.Haskell.Exts.Syntax ( Type(TyCon, TyVar) ) -- , SpecialCon(ExprHole), Name(Ident), QName(UnQual), TyApp, TyFun, Exp(ExpTypeSig)
-- import Language.Haskell.Exts.Parser ( ParseResult, parse, fromParseResult )
-- import Language.Haskell.Exts.SrcLoc ( SrcSpanInfo ) -- , SrcSpan(..), SrcSpanInfo(srcInfoSpan, srcInfoPoints)
-- import Test.QuickCheck ( Gen, arbitrary, sample, sample', variant, generate, resize )
-- TODO: pre-compile for performance, see https://github.com/haskell-hint/hint/issues/37
import Language.Haskell.Interpreter (Interpreter, interpret, as, typeOf, runInterpreter, lift, setImports) -- , MonadInterpreter, infer, eval, kindOf, typeChecks, InterpreterError(..), GhcError(..), liftIO
import Data.List (nub, replicate, delete, minimumBy) -- intercalate, 
-- import System.Random (randomRIO)
import Control.Monad (forM, forM_, replicateM, zipWithM)
import Utility (Tp, Expr, Item(..), say, errorString, interpretIO, fnStr, returnType, randomType, fnInTp, skeleton, flatten)
-- , pick, typeNode, polyTypeNode, undef
import Data.HashMap.Lazy (HashMap, empty, insert, elems, (!), mapWithKey)
import qualified Data.HashMap.Lazy as HashMap -- (fromList, toList)
-- import Data.Set (Set)
-- import qualified Data.Set -- (fromList)
-- import Data.List.Key (group)
import GHC.Exts (groupWith)
import Data.Ord (compare)
import Debug.Dump (d)

-- | An example function.
main :: IO ()
main = do
    r <- runInterpreter testHint
    case r of
        Left err -> putStrLn $ errorString err
        Right () -> return ()

-- TODO: polyary functions
testHint :: Interpreter ()
testHint = do
    let toMapBy xs fn = HashMap.fromList $ zip xs $ fn <$> xs
    let modules = ["Prelude", "Data.List", "Test.QuickCheck"]
    setImports modules

    -- alternative to ScopedTypeVariables: https://stackoverflow.com/q/14540704/1502035
    let fn_bodies = insert "not" "\\b -> let _b = (b :: Bool) in not b" $
                    insert "not_" "not" $
                    insert "id" "id"
                    empty
    -- say [d| fn_bodies |]
    fn_type_strs :: HashMap String String <- mapM typeOf fn_bodies
    -- say [d| fn_type_strs |]
    -- let fn_hole_exprs :: HashMap String Expr = skeleton <$> fn_type_strs
    -- let tp_strs = nub $ elems fn_type_strs
    fn_in_types :: HashMap String Tp <- mapM fnInTp fn_type_strs
    -- say [d| fn_in_types |]
    let in_types :: [Tp] = nub $ elems fn_in_types
    -- say [d| in_types |]
    let str_in_types :: [String] = prettyPrint <$> in_types
    -- say [d| str_in_types |]
    let fn_str_in_types :: HashMap String String = prettyPrint <$> fn_in_types
    -- say [d| fn_str_in_types |]
    in_type_instantiations_ :: [Item Tp] <- lift $ mapM instantiateType in_types
    -- say [d| in_type_instantiations_ |]
    -- casting keys to string cuz Type isn't hashable 😐
    let in_type_instantiations :: HashMap String [Tp] = HashMap.fromList $ zip str_in_types $ nub . flatten <$> in_type_instantiations_
    -- say [d| in_type_instantiations |]
    let type_instantiations :: [Tp] = nub $ concat $ elems in_type_instantiations
    -- say [d| type_instantiations |]
    let str_instantiations :: [String] = prettyPrint <$> type_instantiations
    -- say [d| str_instantiations |]
    let str_in_type_instantiations :: HashMap String [String] = fmap prettyPrint <$> in_type_instantiations
    -- say [d| str_in_type_instantiations |]
    -- do sample generation not for each function but for each function input type
    -- same shit :|
    instantiation_inputs :: HashMap String String <- HashMap.fromList . zip str_instantiations <$> mapM genInputs str_instantiations
    -- say [d| instantiation_inputs |]
    -- let in_type_inputs :: HashMap String (HashMap String String) = (\instantiations -> let ks = instantiations in (toMapBy ks $ (!) instantiation_inputs)) <$> str_in_type_instantiations
    let fnOutputs :: String -> String -> Interpreter (HashMap String (String, String)) = \ k in_type -> let
             instantiations = str_in_type_instantiations ! in_type
             inputs = (!) instantiation_inputs <$> instantiations
             fn_str = fn_bodies ! k
             fn_type_str = fn_type_strs ! k
         in
             HashMap.fromList . zip instantiations <$> mapM (uncurry $ handleInTp fn_type_str fn_str) (zip inputs instantiations)
    fn_in_type_instance_outputs :: (HashMap String (HashMap String (String, String))) <- sequence $ mapWithKey fnOutputs fn_str_in_types
    say [d| fn_in_type_instance_outputs |]
    -- group functions with identical type signatures
    --  :: [(a,b)] -> HashMap b [a]
    let groupByVal = HashMap.fromList . fmap (\pairs -> (snd $ head pairs, fst <$> pairs)) . groupWith snd
    -- let tEST_ = (fmap (\pairs -> (snd $ head pairs, fst <$> pairs)) . groupWith snd) $ HashMap.toList fn_type_strs
    let tEST_ = groupWith snd $ HashMap.toList fn_type_strs
    say [d| tEST_ |]
    let type_sig_fns :: HashMap String [String] = groupByVal $ HashMap.toList fn_type_strs
    say [d| type_sig_fns |]
    -- group functions with identical type signatures + io examples, i.e. functions that are actually equivalent
    let type_sig_io_fns :: HashMap String (HashMap String [String]) = (\ks -> groupByVal $ zip ks $ show . (!) fn_in_type_instance_outputs <$> ks) <$> type_sig_fns
    say [d| type_sig_io_fns |]
    -- deduplicate functions by identical types + io, keeping the shortest
    type_sig_io_fns_filtered :: HashMap String (HashMap String String) <- forM type_sig_io_fns $ mapM $ \fns -> do
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
    say [d| type_sig_io_fns_filtered |]
    let kept_fns :: [String] = concat $ elems <$> elems type_sig_io_fns_filtered
    say [d| kept_fns |]
    let fn_bodies_ = toMapBy kept_fns $ (!) fn_bodies
    say [d| fn_bodies_ |]

    forM_ (HashMap.toList fn_bodies_) $ \kv -> do
        let (k, fn_str) = kv
        let fn_type_str = fn_type_strs ! k
        -- let fn_hole_expr = fn_hole_exprs ! k
        -- let fn_in_type = fn_in_types ! k
        -- let in_type_instantiation = in_type_instantiations ! fn_in_type
        -- let fn_in_str = prettyPrint fn_in_type
        -- let in_type_input = in_type_inputs ! fn_in_str
        let fn_instance_ios = fn_in_type_instance_outputs ! k
        say $ k ++ " :: " ++ fn_type_str ++ " = " ++ fn_str
        forM_ (HashMap.toList fn_instance_ios) $ \instance_ios -> do
            let (in_type_str, (out_type_str, io_pairs)) = instance_ios
            say $ fnStr in_type_str out_type_str ++ ": " ++ io_pairs

handleInTp :: String -> String -> String -> String -> Interpreter (String, String)
handleInTp fn_type_str fn_str ins in_type_str = do
    io_pairs <- interpret ("show $ zip (" ++ ins ++ ") $ (" ++ fn_str ++ ") <$> (" ++ ins ++ ")") (as :: String)
    out_type_str <- returnType fn_type_str in_type_str
    return (out_type_str, io_pairs)

-- TODO: evaluate function calls from AST i/o from interpreter, or move to module and import to typecheck
genInputs :: String -> Interpreter String
genInputs in_type_str = interpretIO $ "let n = 10; seed = 0 in show <$> nub <$> sample' (resize n $ variant seed arbitrary :: Gen (" ++ in_type_str ++ "))"

-- TODO: this isn't right, I shouldn't replace each type variable instance, but find the type variables and their occurrences, then for each one (e.g. a) instantiate types and substitute all occurrences for these.
instantiateType :: Tp -> IO (Item Tp)
instantiateType tp = case tp of
                        TyCon _l qname -> return $ One [TyCon _l qname]
                        TyVar _l _name -> Many . fmap (One . pure) <$> replicateM maxInstances (randomType nestLimit)
                        -- TyApp _l a b -> Many [instantiateType ?]
                        x -> fail $ "unexpected " ++ show x
                      where
                          maxInstances = 5  -- may get less after nub filters out duplicate type instances
                          nestLimit = 0 -- high values make for big logs while debugging...

-- -- Bool/Int types are bs substitutes for a/b to statically test if this compiles for above
-- genIO :: (Bool -> Int) -> IO String
-- genIO fn = do
--     ins <- let n = 10; seed = 0 in nub <$> sample' (resize n $ variant seed arbitrary :: Gen (Bool))
--     return $ show $ zip ins $ (fn) <$> ins
