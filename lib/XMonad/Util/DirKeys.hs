module XMonad.Util.DirKeys(dirs,arrowDirs,ljikDirs) where

import XMonad(KeySym)
import qualified XMonad as K
import XMonad.Util.Types(Direction2D(..))

dirs :: [Direction2D]
dirs = [R, L, U, D]

arrowKeys :: [KeySym]
arrowKeys = [K.xK_Right
            ,K.xK_Left
            ,K.xK_Up
            ,K.xK_Down]

ljikKeys :: [KeySym]
ljikKeys = [K.xK_l
           ,K.xK_j
           ,K.xK_i
           ,K.xK_k]

arrowDirs :: [(KeySym, Direction2D)]
arrowDirs = zip arrowKeys dirs

ljikDirs :: [(KeySym, Direction2D)]
ljikDirs = zip ljikKeys dirs
