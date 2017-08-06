module Ives.ExampleGen.Conc (concretify, send) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Control.Monad
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
                              
send :: Name -> Q Exp
send nm = do
  return $ VarE nm

preferredTys :: Q [Type]
preferredTys = do
  Just intTy <- lookupTypeName "Int"
  Just boolTy <- lookupTypeName "Bool"
  Just charTy <- lookupTypeName "Char"
  return $ map ConT [intTy, boolTy, charTy]

preferredTyCons :: Q [Type]
preferredTyCons = do
  Just maybeTy <- lookupTypeName "Maybe"
  Just eitherTy <- lookupTypeName "Either"
  return $ ListT:(map ConT [maybeTy, eitherTy])

concretify :: Name -> Q Type
concretify nm = do
  info <- reify nm
  let ty = case info of
        VarI _ ty _ _ -> ty
        ClassOpI _ ty _ _ -> ty
  prefTys <- preferredTys
  prefTyCons <- preferredTyCons
  let getTy = getType prefTys Map.empty
  let getTyCon = getType prefTyCons Map.empty
  conc getTy getTyCon ty

-- concrete type generator -> concrete type constructor generator -> function type -> concrete function type
conc :: (Name -> Type) -> (Name -> Type) -> Type -> Q Type
-- replace type variable with a concrete one
conc getTy getTyCon (VarT nm) = conc getTy getTyCon ty
  where ty = getTy nm
conc getTy getTyCon (AppT ArrowT ty) = do
  newTy <- conc getTy getTyCon ty
  return $ AppT ArrowT newTy
conc getTy getTyCon (AppT tyCon tyVar) = do
  -- if the type constructor is variable, get a type constructor instead of a type variable
  newTyCon <- case tyCon of
    VarT nm -> return $ getTyCon nm
    otherwise -> conc getTy getTyCon tyCon
  newTyVar <- conc getTy getTyCon tyVar
  return $ AppT newTyCon newTyVar
conc _ _ (ForallT _ cxt ty) = do
  constraints <- processConstraints cxt Map.empty
  -- curry getType with preferred types and constraints
  prefTys <- preferredTys
  prefTyCons <- preferredTyCons
  let getTy = getType prefTys constraints
  let getTyCon = getType prefTyCons constraints
  conc getTy getTyCon ty
conc _ _ ty = return ty

-- Gets type from a map given a name and a list of preffered types
getType :: [Type] -> Map.Map Name (Set.Set Type) -> Name -> Type
getType [] m nm = case Map.lookup nm m of
  Just instances -> Set.elemAt (Set.size instances - 1) instances
  Nothing -> ListT -- shouldn't ever happen
getType (ty:tys) m nm = case Map.lookup nm m of
  Just instances -> if Set.member ty instances then ty else getType tys m nm
  Nothing -> ty

-- Cxt = [Pred] = [Type]
processConstraints :: Cxt -> Map.Map Name (Set.Set Type) -> Q (Map.Map Name (Set.Set Type))
processConstraints [] m = return m
processConstraints (AppT (ConT cls) (VarT var):xs) m = do
  tys <- getInstances cls
  let newM = Map.insertWith Set.intersection var (Set.fromList tys) m
  processConstraints xs newM

getInstances :: Name -> Q [Type]
getInstances nm = do
  ClassI _ ins <- reify nm
  let nms = map (\(InstanceD _ (AppT _ ty) _) -> ty) ins
  return nms