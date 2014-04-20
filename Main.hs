-- | A simple module for instrumenting C code with calls to an external
-- library to elicit interesting thread interleavings

-- Annotates command-line-specified variables with calls
-- to interleave_schedule_point

-- the following pthread functions are rewritten
-- pthread_create -> interleave_pthread_create
-- pthread_join -> interleave_pthread_join
-- pthread_mutex_lock -> interleave_pthread_mutex_lock
-- pthread_mutex_unlock -> interleave_pthread_mutex_unlock

-- assignment/computation expressions rewritten (though likely unsound)
-- (e.g. e1 += e2 gets rewritten to e1 = e1 + e2)

-- shouldn't use prefix or postfix operators on shared variables

-- currently requires that __sm_thread_id be in scope whenever
-- interleave_schedule_point is called. this could be replaced by
-- some magic using pthread_self, but that would be more
-- complicated

module Main where

import System.Environment(getArgs)
import System.Exit
import Control.Monad
import Control.Applicative((<|>))
import Data.List(find)
import System.Directory
import System.FilePath

import Language.C
import Language.C.Data.Ident
import Language.C.System.GCC
import Data.Generics

main :: IO ()
main = do
  (ifName, ofName, vars) <- getArgs >>= checkArgs
  epf <- parseCFile (newGCC "gcc")  Nothing [] ifName
  case epf of
    Left err -> print err >> exitFailure
    Right ast ->
      writeFile ofName $ show $ pretty $ annotateProgram ast vars

type InputFile = FilePath
type OutputFile = FilePath
type Var = String

checkArgs :: [Var] -> IO (InputFile, OutputFile, [Var])
checkArgs (inputFile : outputFile : vars) = do
  fear' <- fear inputFile
  when (not fear') $ do
    putStrLn "Input file doesn't exist or isn't readable"
    usageAndExit
  deaw' <- deaw $ takeDirectory outputFile
  when (not deaw') $ do
    putStrLn "Directory containing output file doesn't exist or isn't writable"
    usageAndExit
  return (inputFile, outputFile, vars)

  where
    -- file exists and is readable
    fear :: FilePath -> IO Bool
    fear f =
      liftM2 (&&) (doesFileExist f)
      (getPermissions f >>= return . readable)

    -- directory exists and is writable
    deaw :: FilePath -> IO Bool
    deaw d = liftM2 (&&) (doesDirectoryExist d)
             (getPermissions d >>= return . writable)
checkArgs _ = usageAndExit

usageAndExit :: IO a
usageAndExit = do
  putStrLn "Usage: interleave <inputfile> <outputfile> <shared memory vars> ..."
  exitFailure

data VarUse = READ_USE | WRITE_USE
            deriving (Show, Eq)

annotateProgram :: CTranslUnit -> [Var] -> CTranslUnit
annotateProgram ctu smVars =
  everywhere (mkT replacePThreadIds) $
  everywhere (mkT unwrapFnVars) $
  everywhere (mkT (wrapWrites smVars)) $
  everywhere (mkT unwrapLHSs) $
  everywhere (mkT (wrapVarUses smVars)) $
  everywhere (mkT simplifyAssignments) ctu

simplifyAssignments :: CExpr -> CExpr
simplifyAssignments e@(CAssign CAssignOp e1 e2 ni) = e
simplifyAssignments e@(CAssign aOp e1 e2 ni) =
  (CAssign CAssignOp e1 (CBinary (aOpToBOp aOp) e1 e2 undefNode) ni)
  where
    aOpToBOp :: CAssignOp -> CBinaryOp
    aOpToBOp CMulAssOp = CMulOp
    aOpToBOp CDivAssOp = CDivOp
    aOpToBOp CRmdAssOp = CRmdOp
    aOpToBOp CAddAssOp = CAddOp
    aOpToBOp CSubAssOp = CSubOp
    aOpToBOp CShlAssOp = CShlOp
    aOpToBOp CShrAssOp = CShrOp
    aOpToBOp CAndAssOp = CAndOp
    aOpToBOp CXorAssOp = CXorOp
    aOpToBOp COrAssOp  = COrOp
    aOpToBOp CAssignOp = error "CAssignOp in aOpToBOp"
simplifyAssignments a = a

replacePThreadIds :: CExpr -> CExpr
replacePThreadIds e@(CVar (Ident x a b) c) =
  case lookup x idMap of
    Nothing -> e
    Just y -> (CVar (Ident y a b) c)
  where
    idMap = map (\x -> (x, "interleave_" ++ x))
            [ "pthread_create"
            , "pthread_join"
            , "pthread_mutex_lock"
            , "pthread_mutex_unlock"
            ]
replacePThreadIds e = e

wrapVarUses :: [Var] -> CExpr -> CExpr
wrapVarUses vars v@(CVar (Ident i _ _) _)
  | i `elem` vars || (length vars == 0) = wrapReadExpr v i
wrapVarUses _ a = a

wrapWrites :: [Var] -> CExpr -> CExpr
wrapWrites vars v@(CAssign o l r ni) =
  case interestingLHS vars l of
    Nothing -> v
    (Just lv) -> (CAssign o l (wrapWriteExpr r lv) ni)
wrapWrites _ a = a

interestingLHS :: [Var] -> CExpr -> Maybe String
interestingLHS vars = everything (<|>)
                      (mkQ Nothing (\x -> find (`elem` vars) [x]))

threadIdVar :: String
threadIdVar = "__sm_thread_id"

wrapReadExpr :: CExpr -> String -> CExpr
wrapReadExpr e i =
  (CStatExpr
   (CCompound []
    [ CBlockStmt
      (CExpr
        (Just
        (CCall
         (CVar (internalIdent "interleave_schedule_point") undefNode)
         [ (CVar (internalIdent $ show READ_USE) undefNode)
         , (CVar (internalIdent "__FILE__") undefNode)
         , (CVar (internalIdent "__LINE__") undefNode)
         , (CConst (CStrConst (cString i) undefNode))
         , (CVar (internalIdent threadIdVar) undefNode)
         ] undefNode)) undefNode)
    , CBlockStmt (CExpr (Just e) undefNode)
    ]
    undefNode)
   undefNode)

tmpVar :: String
tmpVar = "__smp_tmp"

wrapWriteExpr :: CExpr -> String -> CExpr
wrapWriteExpr e i =
  (CStatExpr
   (CCompound []
    [ CBlockDecl (CDecl
                  [CTypeSpec (CTypeOfExpr e undefNode)]
                  [(Just
                    (CDeclr
                     (Just (internalIdent tmpVar)) [] Nothing [] undefNode)
                   , (Just (CInitExpr e undefNode))
                   , Nothing
                   )
                  ]
                 undefNode)
    , CBlockStmt
      (CExpr
        (Just
        (CCall
         (CVar (internalIdent "interleave_schedule_point") undefNode)
         [ (CVar (internalIdent $ show WRITE_USE) undefNode)
         , (CVar (internalIdent "__FILE__") undefNode)
         , (CVar (internalIdent "__LINE__") undefNode)
         , (CConst (CStrConst (cString i) undefNode))
         , (CVar (internalIdent threadIdVar) undefNode)
         ] undefNode)) undefNode)
    , CBlockStmt (CExpr
                  (Just (CVar (internalIdent tmpVar) undefNode))
                 undefNode)
    ]
    undefNode)
   undefNode)

unwrapFnVars :: CExpr -> CExpr
unwrapFnVars
  (CCall
   (CStatExpr
    (CCompound []
     [ CBlockStmt (CExpr (Just (CCall (CVar (Ident "interleave_schedule_point" _ _) _) _ _)) _)
     , CBlockStmt (CExpr (Just v) _)
     ]
     _)
    _)
   args
  ni) = (CCall v args ni)
unwrapFnVars a = a

-- unwrap all var uses on the LHS
unwrapLHSs :: CExpr -> CExpr
unwrapLHSs (CAssign o l r ni) =
  (CAssign o (everywhere (mkT unwrap) l) r ni)
unwrapLHSs a = a

unwrap :: CExpr -> CExpr
unwrap
  (CStatExpr
   (CCompound []
    [ CBlockStmt (CExpr (Just (CCall (CVar (Ident "interleave_schedule_point" _ _) _) _ _)) _)
    , CBlockStmt (CExpr (Just v) _)
    ]
    _)
   _) = v
unwrap a = a
