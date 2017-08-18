{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
module Network.Protocol.Pris.Types

where
import           Data.Set (Set)
import           Data.Word (Word16)
import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)


data ParkeerStatus =
    GeenStatus
  | AutomatischBedrijf
  | Vrij
  | Reserved
  | Vol
  | Gesloten
  deriving (Enum, Eq, Show, Bounded)

data ParkeerStoring =
    Kaartuitgifte
  | Lusdetectie
  | Slagboom
  | Lamp
  | Overig
  | Onbetrouwbaar
  | Handbediening
  | Centralebediening
  deriving (Enum, Eq, Show, Bounded, Ord)

type CategorieId = Word16
type RuimteId = Word16

data CategorieStatus = CategorieStatus { aantalParkeerders :: Word16
                                       , aantalInrijders :: Word16
                                       , aantalUitrijders :: Word16
                                       }
                     deriving (Show)
data  RuimteStatus f = RuimteStatus { ruimteStatus :: ParkeerStatus
                                 , storingsToestand :: Set ParkeerStoring
                                 , categorien :: f CategorieStatus
                                 }
                    
-- deriving instance (Show (t CategorieStatus)) => Show (RuimteStatus t) 
deriving instance Show (RuimteStatus [])
deriving instance Show (RuimteStatus (Map CategorieId))

data RuimteConfig f = RuimteConfig { capaciteitRuimte :: Word16
                                   , ruimteCategorien :: f CategorieConfig
                                   }

deriving instance Show (RuimteConfig [])
deriving instance Show (RuimteConfig (Map CategorieId))

data CategorieConfig = CategorieConfig { capaciteitParkeerders :: Word16
                                       }
                       deriving (Show)
newtype Config = Config (Map RuimteId (Map CategorieId CategorieConfig))

data Packet = PollConfig
            | PollStatus
            | WijzigStatus (Map RuimteId (RuimteStatus (Map CategorieId)))
            | WijzigConfig (Map RuimteId (Map CategorieId CategorieConfig))
            | ConfigBericht ([RuimteConfig []])
            | StatusBericht ([RuimteStatus []])
            | AcceptatieStatus
            | AcceptatieConfig
         deriving Show
