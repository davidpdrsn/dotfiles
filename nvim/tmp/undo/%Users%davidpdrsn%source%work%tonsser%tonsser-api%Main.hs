Vim�UnDo� &��e�FKڰ����Fv "�p�v���I�j�      foo ls =            S       S   S   S    UxQ�     _�                             ����                                                                                                                                                                                                                                                                                                                                                             UxM     �                   5�_�                            ����                                                                                                                                                                                                                                                                                                                                                             UxM     �                  5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             UxM     �                 main :: IO ()5�_�                            ����                                                                                                                                                                                                                                                                                                                                                             UxM     �                 �             5�_�                            ����                                                                                                                                                                                                                                                                                                                                                             UxM     �                  5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             UxM&     �                     x <- T.readFile ""5�_�                       "    ����                                                                                                                                                                                                                                                                                                                                                             UxM)    �                     �               5�_�      	                     ����                                                                                                                                                                                                                                                                                                                                                             UxM@     �             �             5�_�      
           	          ����                                                                                                                                                                                                                                                                                                                                                             UxMB     �         	      import qualified Data.Text as T5�_�   	              
      '    ����                                                                                                                                                                                                                                                                                                                                                             UxMH    �         	      'import qualified Data.Text.Lazy.IO as T5�_�   
                    	    ����                                                                                                                                                                                                                                                                                                                                                             UxMO    �      	   	      $    x <- T.readFile "production.log"5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             UxMZ    �         	      import qualified Data.Text as T5�_�                       #    ����                                                                                                                                                                                                                                                                                                                                                             UxM    �         	      -import qualified Data.Text.Internal.Lazy as T5�_�                           ����                                                                                                                                                                                                                                                                                                                                      	          V       UxM�     �      	   	      &    x <- TIO.readFile "production.log"5�_�                           ����                                                                                                                                                                                                                                                                                                                                      	          V       UxM�    �      	   	      *    x <- <$> TIO.readFile "production.log"5�_�                           ����                                                                                                                                                                                                                                                                                                                                      	          V       UxM�    �         
       �         	    5�_�                    	       ����                                                                                                                                                                                                                                                                                                                            	          
          V       UxM�     �      
   
      5    x <- T.filter _ <$> TIO.readFile "production.log"5�_�                    
       ����                                                                                                                                                                                                                                                                                                                            	          
          V       UxM�     �   	              (    putStrLn $ show $ length $ T.lines x5�_�                    
       ����                                                                                                                                                                                                                                                                                                                            	          
          V       UxM�   
 �   	                  putStrLn und5�_�                    	       ����                                                                                                                                                                                                                                                                                                                            	          
          V       UxM�    �      
   
      ?    x <- T.filter _ . T.lines <$> TIO.readFile "production.log"5�_�                    	       ����                                                                                                                                                                                                                                                                                                                            	          
          V       UxM�    �      
   
      2    x <- T.lines <$> TIO.readFile "production.log"5�_�                    	       ����                                                                                                                                                                                                                                                                                                                            	          
          V       UxN     �      
   
      =    x <- filter _ . T.lines <$> TIO.readFile "production.log"5�_�                    	       ����                                                                                                                                                                                                                                                                                                                            	          
          V       UxN     �      
   
      >    x <- filter () . T.lines <$> TIO.readFile "production.log"5�_�                    	       ����                                                                                                                                                                                                                                                                                                                            	          
          V       UxN     �      
   
      @    x <- filter (()) . T.lines <$> TIO.readFile "production.log"5�_�                    	       ����                                                                                                                                                                                                                                                                                                                            	          
          V       UxN     �      
   
      B    x <- filter ((||)) . T.lines <$> TIO.readFile "production.log"5�_�                           ����                                                                                                                                                                                                                                                                                                                            	          
          V       UxN     �         
    �         
    5�_�                           ����                                                                                                                                                                                                                                                                                                                            
                    V       UxN     �               import Control.Applicative5�_�                    
       ����                                                                                                                                                                                                                                                                                                                            
                    V       UxN     �   	            I    x <- filter ((||) <$> ()) . T.lines <$> TIO.readFile "production.log"5�_�                    
       ����                                                                                                                                                                                                                                                                                                                            
                    V       UxN     �   	            K    x <- filter ((||) <$> ("")) . T.lines <$> TIO.readFile "production.log"5�_�                    
   $    ����                                                                                                                                                                                                                                                                                                                            
                    V       UxN!     �   	            R    x <- filter ((||) <$> ("Started")) . T.lines <$> TIO.readFile "production.log"5�_�                     
   &    ����                                                                                                                                                                                                                                                                                                                            
                    V       UxN"     �   	            U    x <- filter ((||) <$> ("Started" ``)) . T.lines <$> TIO.readFile "production.log"5�_�      !               
   1    ����                                                                                                                                                                                                                                                                                                                            
                    V       UxN%     �   	            ^    x <- filter ((||) <$> ("Started" `isInfixOf`)) . T.lines <$> TIO.readFile "production.log"5�_�       "           !   
   7    ����                                                                                                                                                                                                                                                                                                                            
                    V       UxN*     �   	            e    x <- filter ((||) <$> ("Started" `isInfixOf`) <*> ()) . T.lines <$> TIO.readFile "production.log"5�_�   !   #           "   
   8    ����                                                                                                                                                                                                                                                                                                                            
                    V       UxN+     �   	            g    x <- filter ((||) <$> ("Started" `isInfixOf`) <*> ("")) . T.lines <$> TIO.readFile "production.log"5�_�   "   $           #   
   D    ����                                                                                                                                                                                                                                                                                                                            
                    V       UxN/    �   	            s    x <- filter ((||) <$> ("Started" `isInfixOf`) <*> ("Completed" ``)) . T.lines <$> TIO.readFile "production.log"5�_�   #   %           $   
   &    ����                                                                                                                                                                                                                                                                                                                            
                    V       UxNX     �   	            |    x <- filter ((||) <$> ("Started" `isInfixOf`) <*> ("Completed" `isInfixOf`)) . T.lines <$> TIO.readFile "production.log"5�_�   $   &           %   
   '    ����                                                                                                                                                                                                                                                                                                                            
                    V       UxNY     �   	            ~    x <- filter ((||) <$> ("Started" `T>isInfixOf`) <*> ("Completed" `isInfixOf`)) . T.lines <$> TIO.readFile "production.log"5�_�   %   '           &   
   F    ����                                                                                                                                                                                                                                                                                                                            
                    V       UxN[    �   	            ~    x <- filter ((||) <$> ("Started" `T.isInfixOf`) <*> ("Completed" `isInfixOf`)) . T.lines <$> TIO.readFile "production.log"5�_�   &   (           '   
   G    ����                                                                                                                                                                                                                                                                                                                            
                    V       UxNe     �   
          �   
          5�_�   '   )           (   
       ����                                                                                                                                                                                                                                                                                                                            
                    V       UxNf     �   	             �    x <- filter ((||) <$> ("Started" `T.isInfixOf`) <*> ("Completed" `T.isInfixOf`)) . T.lines <$> TIO.readFile "production.log"5�_�   (   *           )   
       ����                                                                                                                                                                                                                                                                                                                                         Q       v   Q    UxNj     �   	   
          �    -- x <- filter ((||) <$> ("Started" `T.isInfixOf`) <*> ("Completed" `T.isInfixOf`)) . T.lines <$> TIO.readFile "production.log"5�_�   )   +           *          ����                                                                                                                                                                                                                                                                                                                            
          
   Q       v   Q    UxN�     �                 �              5�_�   *   ,           +          ����                                                                                                                                                                                                                                                                                                                                         Q       v   Q    UxN�     �                {}5�_�   +   -           ,           ����                                                                                                                                                                                                                                                                                                                                         Q       v   Q    UxN�     �             5�_�   ,   .           -          ����                                                                                                                                                                                                                                                                                                                                         Q       v   Q    UxN�    �                !{-# LANGUAGE PostfixOperators #-}5�_�   -   /           .           ����                                                                                                                                                                                                                                                                                                                                         Q       v   Q    UxN�    �                import Data.List5�_�   .   0           /          ����                                                                                                                                                                                                                                                                                                                                         Q       v   Q    UxN�    �                     putStrLn undefined5�_�   /   1           0          ����                                                                                                                                                                                                                                                                                                                                         Q       v   Q    UxN�    �                   �             5�_�   0   2           1          ����                                                                                                                                                                                                                                                                                                                                         Q       v   Q    UxP�    �   
            �    x <- filter ((||) <$> ("Started" `T.isInfixOf`) <*> ("Completed" `T.isInfixOf`)) . T.lines <$> TIO.readFile "production.log"5�_�   1   3           2          ����                                                                                                                                                                                                                                                                                                                                         Q       v   Q    UxP�    �                     mapM_ print x5�_�   2   4           3          ����                                                                                                                                                                                                                                                                                                                                                 V       UxP�     �                     mapM_ print ls5�_�   3   5           4          ����                                                                                                                                                                                                                                                                                                                                                 V       UxP�    �                     �               5�_�   4   6           5          ����                                                                                                                                                                                                                                                                                                                                                 V       UxP�    �                 foo = id5�_�   5   7           6          ����                                                                                                                                                                                                                                                                                                                                                 V       UxP�    �                 foo ls = id5�_�   6   8           7      	    ����                                                                                                                                                                                                                                                                                                                                                 V       UxP�     �                 foo ls = ls5�_�   7   9           8      	    ����                                                                                                                                                                                                                                                                                                                                                 V       UxQ&     �                 	foo ls = 5�_�   8   :           9      
    ����                                                                                                                                                                                                                                                                                                                                                 V       UxQ&     �                 foo ls = []5�_�   9   ;           :          ����                                                                                                                                                                                                                                                                                                                                                 V       UxQ'     �                 foo ls = [""]5�_�   :   <           ;      
    ����                                                                                                                                                                                                                                                                                                                                                 V       UxQ*     �                 foo ls = ["web.1"]5�_�   ;   =           <          ����                                                                                                                                                                                                                                                                                                                                                 V       UxQ,     �                 foo ls = [ "web.1"]5�_�   <   >           =          ����                                                                                                                                                                                                                                                                                                                                                 V       UxQ-     �               	         �             5�_�   =   ?           >          ����                                                                                                                                                                                                                                                                                                                                                 V       UxQ.     �                        , ""5�_�   >   @           ?          ����                                                                                                                                                                                                                                                                                                                                                 V       UxQ0     �             �             5�_�   ?   A           @          ����                                                                                                                                                                                                                                                                                                                                                 V       UxQ1     �                        , "web.2"5�_�   @   B           A           ����                                                                                                                                                                                                                                                                                                                                                 V       UxQ6     �                �             5�_�   A   C           B          ����                                                                                                                                                                                                                                                                                                                                                 V       UxQ@     �               	foo :: []5�_�   B   D           C          ����                                                                                                                                                                                                                                                                                                                                                 V       UxQB     �               foo :: [T.Text]5�_�   C   E           D          ����                                                                                                                                                                                                                                                                                                                                                 V       UxQD     �               foo :: [T.Text] -> []5�_�   D   F           E          ����                                                                                                                                                                                                                                                                                                                                                 V       UxQD    �               foo :: [T.Text] -> [[]]5�_�   E   G           F           ����                                                                                                                                                                                                                                                                                                                                                 V       UxQ[     �                
         ]�                         , "web.3"�                         , "web.2"�                foo ls = [ "web.1"�                foo :: [T.Text] -> [[T.Text]]�                 5�_�   F   H           G           ����                                                                                                                                                                                                                                                                                                                                                 V       UxQ\    �                   �             5�_�   G   I           H          ����                                                                                                                                                                                                                                                                                                                                                 V       UxQg     �                foo = id    5�_�   H   J           I           ����                                                                                                                                                                                                                                                                                                                                                 V       UxQg    �                --          ]�                --          , "web.3"�                --          , "web.2"�                -- foo ls = [ "web.1"�                 -- foo :: [T.Text] -> [[T.Text]]5�_�   I   K           J          ����                                                                                                                                                                                                                                                                                                                                                 V       UxQw     �               foo ls = [ "web.1"5�_�   J   L           K           ����                                                                                                                                                                                                                                                                                                                                         
       V       UxQ{     �                
         ]�                         , "web.3"�                         , "web.2"5�_�   K   M           L          ����                                                                                                                                                                                                                                                                                                                                         
       V       UxQ|     �                   where x =[ "web.1"5�_�   L   N           M           ����                                                                                                                                                                                                                                                                                                                                                       UxQ}     �                             ]�                             , "web.3"�                             , "web.2"5�_�   M   O           N          ����                                                                                                                                                                                                                                                                                                                                                       UxQ~    �               foo ls =5�_�   N   P           O      	    ����                                                                                                                                                                                                                                                                                                                                                       UxQ�     �               foo ls = ls5�_�   O   Q           P      
    ����                                                                                                                                                                                                                                                                                                                                                       UxQ�    �               foo ls = []5�_�   P   R           Q      	    ����                                                                                                                                                                                                                                                                                                                                                       UxQ�     �               foo ls = [ls]5�_�   Q   S           R          ����                                                                                                                                                                                                                                                                                                                                                       UxQ�    �               foo ls = map ()5�_�   R               S          ����                                                                                                                                                                                                                                                                                                                                                       UxQ�     �               foo ls = map _ ls5��