{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1998

\section[PatSyn]{@PatSyn@: Pattern synonyms}
-}

{-# LANGUAGE CPP, DeriveDataTypeable #-}

module PatSyn (
        -- * Main data types
        PatSyn, mkPatSyn,

        -- ** Type deconstruction
        patSynName, patSynArity, patSynIsInfix,
        patSynArgs, patSynType,
        patSynMatcher, patSynBuilder,
        patSynExTyVars, patSynSig,
        patSynInstArgTys, patSynInstResTy, patSynFieldLabels,
        patSynFieldType,

        tidyPatSynIds
    ) where

#include "HsVersions.h"

import Type
import TcType( mkSpecSigmaTy )
import Name
import Outputable
import Unique
import Util
import BasicTypes
import Var
import FieldLabel

import qualified Data.Data as Data
import qualified Data.Typeable
import Data.Function
import Data.List

{-
************************************************************************
*                                                                      *
\subsection{Pattern synonyms}
*                                                                      *
************************************************************************
-}

-- | A pattern synonym
-- See Note [Pattern synonym representation]
-- See Note [Pattern synonym signatures]
data PatSyn
  = MkPatSyn {
        psName        :: Name,
        psUnique      :: Unique,       -- Cached from Name

        psArgs        :: [Type],
        psArity       :: Arity,        -- == length psArgs
        psInfix       :: Bool,         -- True <=> declared infix
        psFieldLabels :: [FieldLabel], -- List of fields for a
                                       -- record pattern synonym
                                       -- INVARIANT: either empty if no
                                       -- record pat syn or same length as
                                       -- psArgs

        psUnivTyVars  :: [TyVar],      -- Universially-quantified type variables
        psReqTheta    :: ThetaType,    -- Required dictionaries
                                       -- these constraints are very much like
                                       -- stupid thetas (which is a useful
                                       -- guideline when implementing)
                                       -- but are actually needed.
        psExTyVars    :: [TyVar],      -- Existentially-quantified type vars
        psProvTheta   :: ThetaType,    -- Provided dictionaries
        psOrigResTy   :: Type,         -- Mentions only psUnivTyVars

        -- See Note [Matchers and builders for pattern synonyms]
        psMatcher     :: (Id, Bool),
             -- Matcher function.
             -- If Bool is True then prov_theta and arg_tys are empty
             -- and type is
             --   forall (v :: Levity) (r :: TYPE v) univ_tvs.
             --                          req_theta
             --                       => res_ty
             --                       -> (forall ex_tvs. Void# -> r)
             --                       -> (Void# -> r)
             --                       -> r
             --
             -- Otherwise type is
             --   forall (v :: Levity) (r :: TYPE v) univ_tvs.
             --                          req_theta
             --                       => res_ty
             --                       -> (forall ex_tvs. prov_theta => arg_tys -> r)
             --                       -> (Void# -> r)
             --                       -> r

        psBuilder     :: Maybe (Id, Bool)
             -- Nothing  => uni-directional pattern synonym
             -- Just (builder, is_unlifted) => bi-directional
             -- Builder function, of type
             --  forall univ_tvs, ex_tvs. (req_theta, prov_theta)
             --                       =>  arg_tys -> res_ty
             -- See Note [Builder for pattern synonyms with unboxed type]
  }
  deriving Data.Typeable.Typeable

{- Note [Pattern synonym signatures]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In a pattern synonym signature we write
   pattern P :: req => prov => t1 -> ... tn -> res_ty

Note that the "required" context comes first, then the "provided"
context.  Moreover, the "required" context must not mention
existentially-bound type variables; that is, ones not mentioned in
res_ty.  See lots of discussion in Trac #10928.

If there is no "provided" context, you can omit it; but you
can't omit the "required" part (unless you omit both).

Example 1:
      pattern P1 :: (Num a, Eq a) => b -> Maybe (a,b)
      pattern P1 x = Just (3,x)

  We require (Num a, Eq a) to match the 3; there is no provided
  context.

Example 2:
      data T2 where
        MkT2 :: (Num a, Eq a) => a -> a -> T2

      pattern P2 :: () => (Num a, Eq a) => a -> T2
      pattern P2 x = MkT2 3 x

  When we match against P2 we get a Num dictionary provided.
  We can use that to check the match against 3.

Example 3:
      pattern P3 :: Eq a => a -> b -> T3 b

   This signature is illegal because the (Eq a) is a required
   constraint, but it mentions the existentially-bound variable 'a'.
   You can see it's existential because it doesn't appear in the
   result type (T3 b).

Note [Pattern synonym representation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider the following pattern synonym declaration

        pattern P x = MkT [x] (Just 42)

where
        data T a where
              MkT :: (Show a, Ord b) => [b] -> a -> T a

so pattern P has type

        b -> T (Maybe t)

with the following typeclass constraints:

        requires: (Eq t, Num t)
        provides: (Show (Maybe t), Ord b)

In this case, the fields of MkPatSyn will be set as follows:

  psArgs       = [b]
  psArity      = 1
  psInfix      = False

  psUnivTyVars = [t]
  psExTyVars   = [b]
  psProvTheta  = (Show (Maybe t), Ord b)
  psReqTheta   = (Eq t, Num t)
  psOrigResTy  = T (Maybe t)

Note [Matchers and builders for pattern synonyms]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For each pattern synonym P, we generate

  * a "matcher" function, used to desugar uses of P in patterns,
    which implements pattern matching

  * A "builder" function (for bidirectional pattern synonyms only),
    used to desugar uses of P in expressions, which constructs P-values.

For the above example, the matcher function has type:

        $mP :: forall (r :: ?) t. (Eq t, Num t)
            => T (Maybe t)
            -> (forall b. (Show (Maybe t), Ord b) => b -> r)
            -> (Void# -> r)
            -> r

with the following implementation:

        $mP @r @t $dEq $dNum scrut cont fail
          = case scrut of
              MkT @b $dShow $dOrd [x] (Just 42) -> cont @b $dShow $dOrd x
              _                                 -> fail Void#

Notice that the return type 'r' has an open kind, so that it can
be instantiated by an unboxed type; for example where we see
     f (P x) = 3#

The extra Void# argument for the failure continuation is needed so that
it is lazy even when the result type is unboxed.

For the same reason, if the pattern has no arguments, an extra Void#
argument is added to the success continuation as well.

For *bidirectional* pattern synonyms, we also generate a "builder"
function which implements the pattern synonym in an expression
context. For our running example, it will be:

        $bP :: forall t b. (Eq t, Num t, Show (Maybe t), Ord b)
            => b -> T (Maybe t)
        $bP x = MkT [x] (Just 42)

NB: the existential/universal and required/provided split does not
apply to the builder since you are only putting stuff in, not getting
stuff out.

Injectivity of bidirectional pattern synonyms is checked in
tcPatToExpr which walks the pattern and returns its corresponding
expression when available.

Note [Builder for pattern synonyms with unboxed type]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For bidirectional pattern synonyms that have no arguments and have an
unboxed type, we add an extra Void# argument to the builder, else it
would be a top-level declaration with an unboxed type.

        pattern P = 0#

        $bP :: Void# -> Int#
        $bP _ = 0#

This means that when typechecking an occurrence of P in an expression,
we must remember that the builder has this void argument. This is
done by TcPatSyn.patSynBuilderOcc.


************************************************************************
*                                                                      *
\subsection{Instances}
*                                                                      *
************************************************************************
-}

instance Eq PatSyn where
    (==) = (==) `on` getUnique
    (/=) = (/=) `on` getUnique

instance Ord PatSyn where
    (<=) = (<=) `on` getUnique
    (<) = (<) `on` getUnique
    (>=) = (>=) `on` getUnique
    (>) = (>) `on` getUnique
    compare = compare `on` getUnique

instance Uniquable PatSyn where
    getUnique = psUnique

instance NamedThing PatSyn where
    getName = patSynName

instance Outputable PatSyn where
    ppr = ppr . getName

instance OutputableBndr PatSyn where
    pprInfixOcc = pprInfixName . getName
    pprPrefixOcc = pprPrefixName . getName

instance Data.Data PatSyn where
    -- don't traverse?
    toConstr _   = abstractConstr "PatSyn"
    gunfold _ _  = error "gunfold"
    dataTypeOf _ = mkNoRepType "PatSyn"

{-
************************************************************************
*                                                                      *
\subsection{Construction}
*                                                                      *
************************************************************************
-}

-- | Build a new pattern synonym
mkPatSyn :: Name
         -> Bool                 -- ^ Is the pattern synonym declared infix?
         -> ([TyVar], ThetaType) -- ^ Universially-quantified type variables
                                 --   and required dicts
         -> ([TyVar], ThetaType) -- ^ Existentially-quantified type variables
                                 --   and provided dicts
         -> [Type]               -- ^ Original arguments
         -> Type                 -- ^ Original result type
         -> (Id, Bool)           -- ^ Name of matcher
         -> Maybe (Id, Bool)     -- ^ Name of builder
         -> [FieldLabel]         -- ^ Names of fields for
                                 --   a record pattern synonym
         -> PatSyn
mkPatSyn name declared_infix
         (univ_tvs, req_theta)
         (ex_tvs, prov_theta)
         orig_args
         orig_res_ty
         matcher builder field_labels
    = MkPatSyn {psName = name, psUnique = getUnique name,
                psUnivTyVars = univ_tvs, psExTyVars = ex_tvs,
                psProvTheta = prov_theta, psReqTheta = req_theta,
                psInfix = declared_infix,
                psArgs = orig_args,
                psArity = length orig_args,
                psOrigResTy = orig_res_ty,
                psMatcher = matcher,
                psBuilder = builder,
                psFieldLabels = field_labels
                }

-- | The 'Name' of the 'PatSyn', giving it a unique, rooted identification
patSynName :: PatSyn -> Name
patSynName = psName

patSynType :: PatSyn -> Type
-- The full pattern type, used only in error messages
-- See Note [Pattern synonym signatures]
patSynType (MkPatSyn { psUnivTyVars = univ_tvs, psReqTheta = req_theta
                     , psExTyVars   = ex_tvs,   psProvTheta = prov_theta
                     , psArgs = orig_args, psOrigResTy = orig_res_ty })
  = mkSpecSigmaTy univ_tvs req_theta $  -- use mkSpecSigmaTy because it
    mkSpecSigmaTy ex_tvs prov_theta $   -- prints better
    mkFunTys orig_args orig_res_ty

-- | Should the 'PatSyn' be presented infix?
patSynIsInfix :: PatSyn -> Bool
patSynIsInfix = psInfix

-- | Arity of the pattern synonym
patSynArity :: PatSyn -> Arity
patSynArity = psArity

patSynArgs :: PatSyn -> [Type]
patSynArgs = psArgs

patSynFieldLabels :: PatSyn -> [FieldLabel]
patSynFieldLabels = psFieldLabels

-- | Extract the type for any given labelled field of the 'DataCon'
patSynFieldType :: PatSyn -> FieldLabelString -> Type
patSynFieldType ps label
  = case find ((== label) . flLabel . fst) (psFieldLabels ps `zip` psArgs ps) of
      Just (_, ty) -> ty
      Nothing -> pprPanic "dataConFieldType" (ppr ps <+> ppr label)

patSynExTyVars :: PatSyn -> [TyVar]
patSynExTyVars = psExTyVars

patSynSig :: PatSyn -> ([TyVar], ThetaType, [TyVar], ThetaType, [Type], Type)
patSynSig (MkPatSyn { psUnivTyVars = univ_tvs, psExTyVars = ex_tvs
                    , psProvTheta = prov, psReqTheta = req
                    , psArgs = arg_tys, psOrigResTy = res_ty })
  = (univ_tvs, req, ex_tvs, prov, arg_tys, res_ty)

patSynMatcher :: PatSyn -> (Id,Bool)
patSynMatcher = psMatcher

patSynBuilder :: PatSyn -> Maybe (Id, Bool)
patSynBuilder = psBuilder

tidyPatSynIds :: (Id -> Id) -> PatSyn -> PatSyn
tidyPatSynIds tidy_fn ps@(MkPatSyn { psMatcher = matcher, psBuilder = builder })
  = ps { psMatcher = tidy_pr matcher, psBuilder = fmap tidy_pr builder }
  where
    tidy_pr (id, dummy) = (tidy_fn id, dummy)

patSynInstArgTys :: PatSyn -> [Type] -> [Type]
-- Return the types of the argument patterns
-- e.g.  data D a = forall b. MkD a b (b->a)
--       pattern P f x y = MkD (x,True) y f
--          D :: forall a. forall b. a -> b -> (b->a) -> D a
--          P :: forall c. forall b. (b->(c,Bool)) -> c -> b -> P c
--   patSynInstArgTys P [Int,bb] = [bb->(Int,Bool), Int, bb]
-- NB: the inst_tys should be both universal and existential
patSynInstArgTys (MkPatSyn { psName = name, psUnivTyVars = univ_tvs
                           , psExTyVars = ex_tvs, psArgs = arg_tys })
                 inst_tys
  = ASSERT2( length tyvars == length inst_tys
          , text "patSynInstArgTys" <+> ppr name $$ ppr tyvars $$ ppr inst_tys )
    map (substTyWith tyvars inst_tys) arg_tys
  where
    tyvars = univ_tvs ++ ex_tvs

patSynInstResTy :: PatSyn -> [Type] -> Type
-- Return the type of whole pattern
-- E.g.  pattern P x y = Just (x,x,y)
--         P :: a -> b -> Just (a,a,b)
--         (patSynInstResTy P [Int,Bool] = Maybe (Int,Int,Bool)
-- NB: unlikepatSynInstArgTys, the inst_tys should be just the *universal* tyvars
patSynInstResTy (MkPatSyn { psName = name, psUnivTyVars = univ_tvs
                          , psOrigResTy = res_ty })
                inst_tys
  = ASSERT2( length univ_tvs == length inst_tys
           , text "patSynInstResTy" <+> ppr name $$ ppr univ_tvs $$ ppr inst_tys )
    substTyWith univ_tvs inst_tys res_ty
