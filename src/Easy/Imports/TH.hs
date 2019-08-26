{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE PackageImports    #-}

module Easy.Imports.TH where

import "lens"  Control.Lens
import "Cabal" Distribution.Types.CondTree

makeLensesFor [("condTreeData","condTreeDataLens")
              ,("condTreeConstraints","condTreeConstraintsLens")] ''CondTree
