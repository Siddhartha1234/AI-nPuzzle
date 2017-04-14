module Klotski where
	
import Prelude

import Data.Foldable
import Data.Maybe
import Data.List 
import Data.Ix

import Control.Applicative
import Control.Monad 
import Control.Arrow

import qualified Data.Set as Set
import qualified Data.Map.Strict as Map


type Pos = (Int, Int)
type Shape = Set.Set Pos
type Unit = (Pos, Shape)
type Transition = Map.Map Int Block

data Board = Board {
	row :: Int,
	columns :: Int,
	unit :: Unit,
	targetUnitID :: Int,
	targetPos :: Pos
} deriving Show

