{-# LANGUAGE ImpredicativeTypes, RankNTypes, FlexibleContexts #-}

-- | find holes in an AST
module FindHoles (gtrExpr, strExpr, findHolesExpr) where

import Language.Haskell.Exts.Syntax ( Exp(..), SpecialCon(..), QName(..) )
import Utility (composeSetters)
import Types (Expr)

-- | get the first sub-expression
gtrExpr :: Exp l -> Exp l
gtrExpr x = case x of
    (Let _l _binds xp) -> xp
    (App _l xp _exp2) -> xp
    (Paren _l xp) -> xp
    (ExpTypeSig _l xp _tp) -> xp
    _ -> x

-- | set the first sub-expression
strExpr :: Exp l -> Exp l -> Exp l
strExpr x xp = case x of
    (Let l binds _exp) -> Let l binds xp
    (App l _exp1 xp2) -> App l xp xp2
    (Paren l _exp) -> Paren l xp
    (ExpTypeSig l _exp tp) -> ExpTypeSig l xp tp
    _ -> x

-- | look for holes in an expression. to simplify extracting type, we will only look for holes as part of an ExpTypeSig, e.g. `_ :: Bool`
-- | I couldn't figure out how to get this to typecheck as a whole lens, so instead I'm taking them as getter/setter pairs...
findHolesExpr :: Exp l1 -> [(Exp l2 -> Exp l2, Exp l3 -> Exp l3 -> Exp l3)]
findHolesExpr expr = let
        f = findHolesExpr
        -- by the time we use the lens, we already know exactly how we're navigating.
        -- however, at compile-time this isn't known, so we have to pretend we're still checking here.
        mapLenses (a, b) = (a . gtrExpr, composeSetters strExpr gtrExpr b)
    in case expr of
    Let _l _binds xpr -> mapLenses <$> f xpr
    App _l exp1 exp2 ->  (mapLenses <$> f exp1) ++ (mapLenses2 <$> f exp2)
        where
            gtr2 x = case x of (App _l _exp1 xp2) -> xp2; _ -> x
            str2 x xp2 = case x of (App l xp1 _exp2) -> App l xp1 xp2; _ -> x
            mapLenses2 (a, b) = (a . gtr2, composeSetters str2 gtr2 b)
    Paren _l xpr -> mapLenses <$> f xpr
    ExpTypeSig _l xpr _tp -> case xpr of
        Var _l qname -> case qname of
            Special _l specialCon -> case specialCon of
                ExprHole _l -> [(id, flip const)]
                _ -> mapLenses <$> f xpr
            _ -> mapLenses <$> f xpr
        _ -> mapLenses <$> f xpr
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
