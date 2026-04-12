module Mica.Cute where 

import Data.Text 

class Cute a where 
    cute :: a -> Text 
