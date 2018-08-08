{-# LANGUAGE TemplateHaskell #-}

module System.Console.GetOpt.Annotations
  ( Option (..),
    OptionValue (..),
    makeOptions
  ) where

import Control.Monad
import qualified Data.Map as M
import Data.Default
import Language.Haskell.TH
import qualified System.Console.GetOpt as G

import System.Console.GetOpt.Annotations.Types

getRecordAnnotations :: Name -> Q [FieldAnnotation]
getRecordAnnotations typeName = do
    TyConI def <- reify typeName
    case def of
      DataD [] _ [] _ [con] _ -> processConstructor con
      DataD {} -> fail "only single-constructor types without context and boundaries are supported for now"
      _ -> fail $ show typeName ++ " is not a type declaration"
  where
    processConstructor :: Con -> Q [FieldAnnotation]
    processConstructor (RecC conName fields) = do
      forM fields $ \(fieldName, _, fieldType) -> do
        let name = nameBase fieldName
        anns <- reifyAnnotations $ AnnLookupName fieldName
        runIO $ putStrLn $ name ++ ": " ++ show anns
        let opts = case anns of
                     [] -> Auto name
                     [ann] -> ann
                     _ -> foldr1 (<>) anns
        return $ FieldAnnotation fieldName fieldType opts
    processConstructor _ = fail "only plain record constructors are supported by now"

parseValue' :: OptionValue a => String -> a
parseValue' str = either error id $ parseValue str

mkArgDescr :: FieldAnnotation -> Q Exp
mkArgDescr (FieldAnnotation fieldName fieldType opt) = do
    case fieldType of
      ConT n | nameBase n == "Bool" -> mkSetTrue 
      AppT (ConT c) t | nameBase c == "Maybe" -> mkSetJust t
      _ -> mkSet fieldType
  where
    getString :: Type -> Q Exp
    getString t = [| parseValue' |]

    mkSet :: Type -> Q Exp
    mkSet t = do
      parse <- getString t
      arg <- newName "arg"
      options <- newName "options"
      let res = LamE [VarP arg, VarP options] $ RecUpdE (VarE options) [(fieldName, AppE parse (VarE arg))]
      let ph = getPlaceholder opt
      [| G.ReqArg $(return res) ph |]

    mkSetTrue :: Q Exp
    mkSetTrue = do
      options <- newName "options"
      let res = LamE [VarP options] $ RecUpdE (VarE options) [(fieldName, ConE (mkName "True"))]
      return $ AppE (ConE $ mkName "NoArg") res

    mkSetJust :: Type -> Q Exp
    mkSetJust t = do
      parse <- getString t
      arg <- newName "arg"
      mbArg <- newName "mbArg"
      options <- newName "options"
      let ph = getPlaceholder opt
      let nothing = ConP (mkName "Nothing") []
          just n = ConP (mkName "Just") [VarP n]
          setJust = RecUpdE (VarE options) [(fieldName, AppE (ConE $ mkName "Just") $ AppE parse (VarE arg))]
          idE = VarE options
          res = LamE [VarP mbArg, VarP options] $
            CaseE (VarE mbArg) [
                Match nothing (NormalB idE) [],
                Match (just arg) (NormalB setJust) []
              ]
      [| G.OptArg $(return res) ph |]

mkOption :: FieldAnnotation -> Q Exp
mkOption f@(FieldAnnotation fieldName fieldType opt) = do
  let descr = mkArgDescr f
  let s = getShort opt
      v = getLong opt
      h = getHelp opt
  [| G.Option s v $(descr) h |]

makeOptions :: Name -> Q Exp
makeOptions name = do
  anns <- getRecordAnnotations name
  opts <- forM anns mkOption
  return $ ListE opts

testTH :: Name -> Q [Dec]
testTH name = do
  anns <- getRecordAnnotations name
  runIO $ print anns

  forM anns $ \ann@(FieldAnnotation fieldName fieldType opt) -> do
    exp <- mkArgDescr ann
    let name = mkName $ nameBase fieldName ++ "_arg"
    return $ ValD (VarP name) (NormalB exp) []

