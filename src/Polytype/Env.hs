{-|
Description : Handling of system environment variables
-}

module Polytype.Env
  ( Env(..)
  , mkEnv
  , computeEnv
  ) where

import Data.Default (Default(def))
import Data.Map (Map)
import Data.Set (Set)

import qualified Data.Maybe
import qualified Data.Map
import qualified Data.Set
import qualified System.Environment

data Env = Env {
    envData :: Map String String -- ^ Environment variables, can be used to override pass-thru variables
  , envPassthru :: Set String    -- ^ Environment variables to pass-thru from parent environment
  } deriving (Eq, Show, Ord)

instance Default Env where
  def = mkEnv [("POLYTYPE", "true")] ["PATH"]

-- | Create new `Env` from pairs of variable -> value
-- and a list of variables to copy from system environment.
mkEnv :: [(String, String)] -> [String] -> Env
mkEnv edata pass = Env (Data.Map.fromList edata) (Data.Set.fromList pass)

-- | Lookup environments variable to copy from system.
systemEnvPassthru :: [String] -> IO (Map String String)
systemEnvPassthru pt =
      Data.Map.fromList . Data.Maybe.catMaybes
  <$> mapM
        (\name -> fmap (\x -> (name, x))
        <$> System.Environment.lookupEnv name)
      pt

-- | Compute environment in the form accepted
-- by standard functions from provided `Env` data.
computeEnv :: Env -> IO [(String, String)]
computeEnv Env{..} = do
  p <- systemEnvPassthru $ Data.Set.toList envPassthru
  return $ Data.Map.toList (envData `Data.Map.union` p)


