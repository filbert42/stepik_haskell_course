module Course1_step2_3 where

class Printable a where
    toString:: a -> String
    
instance Printable Bool  where
    toString True = "true"
    toString False = "false"
    
instance Printable () where
    toString () = "unit type"

instance (Printable a, Printable b) => Printable (a,b) where
   toString (a,b) = "(" ++ toString a ++ "," ++ toString b ++ ")"