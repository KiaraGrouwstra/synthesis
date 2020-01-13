{-# LANGUAGE ImpredicativeTypes #-}

-- | find holes in an AST
module FindHoles (findHolesExpr) where

import Language.Haskell.Exts.Syntax ( Exp(..), SpecialCon(..), QName(..) )
-- , Name(..), Binds(..), Alt(..), Pat(..), RPat(..), Stmt(..), Rhs(..), GuardedRhs(..), Decl(..), Splice(..), PatField(..), Type(..)
-- import Language.Haskell.Exts.SrcLoc ( SrcSpanInfo(..) )
-- , SrcSpan(..), srcInfoSpan, srcInfoPoints
-- import Types (Expr)
import Control.Lens
-- import Control.Lens.Getter
-- import Control.Lens.Setter

-- | look for holes in an expression. to simplify extracting type, we will only look for holes as part of an ExpTypeSig, e.g. `_ :: Bool`
-- findHolesExpr :: Exp l1 -> [Lens' (Exp l2) (Exp l2)]
findHolesExpr :: Functor f => Exp l1 -> [(Exp l2 -> f (Exp l2)) -> Exp l2 -> f (Exp l2)]
findHolesExpr expr = let f = findHolesExpr in case expr of
    -- Var _l qname -> findHolesQname qname
    -- TupleSection _l boxed maybeExps -> maybeExps >>= (\case Nothing -> []; Just xpr -> f xpr)
    -- Case _l xpr alts -> f xpr ++ (alts >>= findHolesAlt)
    -- Let _l _binds xpr -> f xpr -- ++ findHolesBinds binds
    Let _l _binds xpr -> (lens gtr str .) <$> f xpr
        where
            gtr x = case x of (Let _l _binds xp) -> xp; _ -> x
            -- str (Let l binds _exp) = Let l binds
            str x xp = case x of (Let l binds _exp) -> Let l binds xp; _ -> x
    -- InfixApp _l exp1 qop exp2 -> f exp1 ++ f exp2
    -- App _l exp1 exp2 -> f exp1 ++ f exp2
    App _l exp1 exp2 ->  ((lens gtr1 str1 .) <$> f exp1) ++ ((lens gtr2 str2 .) <$> f exp2)
        where
            gtr1 x = case x of (App _l xp1 _exp2) -> xp1; _ -> x
            -- str1 (App l _exp1 xp2) xp1 = App l xp1 xp2
            str1 x xp1 = case x of (App l _exp1 xp2) -> App l xp1 xp2; _ -> x
            gtr2 x = case x of (App _l _exp1 xp2) -> xp2; _ -> x
            str2 x xp2 = case x of (App l xp1 _exp2) -> App l xp1 xp2; _ -> x
    -- Lambda _l _pats xpr -> f xpr
    Lambda _l _pats xpr -> (lens gtr str .) <$> f xpr
        where
            gtr x = case x of (Lambda _l _pats xp) -> xp; _ -> x
            -- str (Lambda l pats _exp) = Lambda l pats
            str x xp = case x of (Lambda l pats _exp) -> Lambda l pats xp; _ -> x
    -- If _l  exp1 exp2 exp3 -> f exp1 ++ f exp2 ++ f exp3
    -- Tuple _l boxed exps -> exps >>= f
    -- List _l exps -> exps >>= f
    -- Paren _l xpr -> f xpr
    Paren _l xpr -> (lens gtr str .) <$> f xpr
        where
            gtr x = case x of (Paren _l xp) -> xp; _ -> x
            -- str (Paren l _exp) = Paren l
            str x xp = case x of (Paren l _exp) -> Paren l xp; _ -> x
    ExpTypeSig _l xpr _tp -> case xpr of
        Var _l qname -> case qname of
            Special _l specialCon -> case specialCon of
                ExprHole _l -> [lns]
                    where
                        lns = lens id $ flip const
                _ -> (lens gtr str .) <$> f xpr
            _ -> (lens gtr str .) <$> f xpr
        _ -> (lens gtr str .) <$> f xpr
        where
            gtr x = case x of (ExpTypeSig _l xp _tp) -> xp; _ -> x
            -- str (ExpTypeSig l _exp tp) xp = ExpTypeSig l xp tp
            str x xp = case x of (ExpTypeSig l _exp tp) -> ExpTypeSig l xp tp; _ -> x
    _ -> []

-- findHolesBinds :: Binds l -> [Exp l]
-- findHolesBinds binds = let f = findHolesBinds in case binds of
--     BDecls _l decls -> decls >>= findHolesDecl

-- findHolesAlt :: Alt l -> [Exp l]
-- findHolesAlt alt = case alt of
--     Alt _l pat rhs maybeBinds -> findHolesPat pat ++ findHolesRhs rhs
--     --  ++ case maybeBinds of
--     --     Nothing -> []
--     --     Just binds -> findHolesBinds binds

-- findHolesRPat :: RPat l -> [Exp l]
-- findHolesRPat rpat = let f = findHolesRPat in case rpat of
--     RPOp _l rpat rpatop -> f rpat -- ++ findHolesRPatOp rpatop
--     RPEither _l rpat1 rpat2 -> f rpat1 ++ f rpat2
--     RPSeq _l rpats -> rpats >>= f
--     RPGuard _l pat stmts -> findHolesPat pat ++ (stmts >>= findHolesStmt)
--     RPCAs _l name rpat -> f rpat
--     RPAs _l name rpat -> f rpat
--     RPParen _l rpat -> f rpat
--     RPPat _l pat -> findHolesPat pat

-- findHolesPat :: Pat l -> [Exp l]
-- findHolesPat pat = let f = findHolesPat in case pat of
--     PVar _l name -> findHolesName name
--     -- PNPlusK _l name int -> findHolesName name
--     -- PInfixApp _l pat1 qname pat2 -> f pat1 ++ findHolesQname qname ++ f pat2
--     -- PApp _l qname pats -> findHolesQname qname ++ (pats >>= f)
--     -- PTuple _l boxed pats -> pats >>= f
--     -- PUnboxedSum _l int1 int2 pat -> f pat
--     -- PList _l pats -> pats >>= f
--     -- PParen _l pat -> f pat
--     -- PRec _l qname patfields -> findHolesQname qname ++ (patfields >>= findHolesPatField)
--     -- PAsPat _l name pat -> f pat -- ++ findHolesName name
--     -- PIrrPat _l pat -> f pat
--     -- PatTypeSig _l pat tp -> f pat
--     -- PViewPat _l xpr pat -> findHolesExpr xpr ++ f pat
--     -- PRPat _l rpats -> rpats >>= findHolesRPat
--     -- PSplice _l splice -> findHolesSplice splice
--     -- PBangPat _l pat -> f pat
--     _ -> []

-- findHolesPatField :: PatField l -> [Exp l]
-- findHolesPatField patField = case patField of
--     PFieldPat _l qname pat -> findHolesQname qname ++ findHolesPat pat
--     PFieldPun _l qname -> findHolesQname qname
--     _ -> []

-- findHolesSplice :: Splice l -> [Exp l]
-- findHolesSplice splice = case splice of
--     ParenSplice _l xpr -> findHolesExpr xpr
--     -- TParenSplice _l xpr -> findHolesExpr xpr
--     _ -> []

-- findHolesStmt :: Stmt l -> [Exp l]
-- findHolesStmt stmt = let f = findHolesStmt in case stmt of
--     Generator _l pat xpr -> findHolesPat pat ++ findHolesExpr xpr
--     Qualifier _l xpr -> findHolesExpr xpr
--     -- LetStmt _l binds -> findHolesBinds binds
--     RecStmt _l stmts -> stmts >>= f

-- findHolesGuardedRhs :: GuardedRhs l -> [Exp l]
-- findHolesGuardedRhs guardedRhs = case guardedRhs of
--     GuardedRhs _l stmts xpr -> (stmts >>= findHolesStmt) ++ findHolesExpr xpr

-- findHolesQname :: QName l -> [Exp l]
-- findHolesQname qname = case qname of
--     -- UnQual _l name -> findHolesName name
--     Special _l specialCon -> findHolesSpecialCon specialCon
--     _ -> []

-- findHolesRhs :: Rhs l -> [Exp l]
-- findHolesRhs rhs = case rhs of
--     UnGuardedRhs _l xpr -> findHolesExpr xpr
--     -- GuardedRhss _l guardedRhss -> guardedRhss >>= findHolesGuardedRhs

-- findHolesDecl :: Decl l -> [Exp l]
-- findHolesDecl decl = let f = findHolesDecl in case decl of
--     PatBind _l pat rhs maybeBinds -> findHolesPat pat ++ findHolesRhs rhs ++ case maybeBinds of
--         Nothing -> []
--         Just binds -> findHolesBinds binds
--     _ -> []

-- findHolesSpecialCon :: SpecialCon l -> [Exp l]
-- findHolesSpecialCon specialCon = case specialCon of
--     ExprHole _l -> [specialCon]
--     -- ListCon _l -> []
--     _ -> []
