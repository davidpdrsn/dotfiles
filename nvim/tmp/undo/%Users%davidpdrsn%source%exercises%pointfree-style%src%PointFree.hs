Vim�UnDo� U��-��Uw�O7����$�J��JD�0      $hasUppercaseWord = any $ all isUpper            Q   Q   Q   Q   P    T��t   - _�                            ����                                                                                                                                                                                                                                                                                                                                                             T���     �      	         hasEven xs = any even xs5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             T���    �      	         hasEven = any even xs5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             T��    �      	         hasEven = any even5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             T��    �      	         hasEven = any even laks5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             T��1     �   
            *hasUppercaseWord xs = any (all isUpper) xs5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             T��3     �   
            'hasUppercaseWord = any (all isUpper) xs�             5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             T��4     �   
            %hasUppercaseWord = any all isUpper xs5�_�      	                 '    ����                                                                                                                                                                                                                                                                                                                                                             T��4    �   
            'hasUppercaseWord = any . all isUpper xs5�_�      
           	          ����                                                                                                                                                                                                                                                                                                                                                             T��:     �   
            $hasUppercaseWord = any . all isUpper5�_�   	              
      %    ����                                                                                                                                                                                                                                                                                                                                                             T��;    �   
            %hasUppercaseWord = any . (all isUpper5�_�   
                        ����                                                                                                                                                                                                                                                                                                                                                             T��F     �   
            &hasUppercaseWord = any . (all isUpper)5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             T��H    �   
            &hasUppercaseWord = any $ (all isUpper)�             5�_�                       
    ����                                                                                                                                                                                                                                                                                                                                                             T���     �               isDigit c = elem c "1234567890"5�_�                       
    ����                                                                                                                                                                                                                                                                                                                                                             T���     �               isDigit = elem c "1234567890"5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             T���     �               "isDigit = flip elem c "1234567890"5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             T���     �               $isDigit = flip elem $ c "1234567890"5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             T���   	 �               #isDigit = flip elem $c "1234567890"5�_�                       	    ����                                                                                                                                                                                                                                                                                                                                                             T��     �               double x = 2 * x5�_�                       	    ����                                                                                                                                                                                                                                                                                                                                                             T��     �               double = 2 * x5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             T��   
 �               double = (2 * x5�_�                       	    ����                                                                                                                                                                                                                                                                                                                                                             T��     �               triple x = x * 35�_�                       	    ����                                                                                                                                                                                                                                                                                                                                                             T��      �               triple = x * 35�_�                       	    ����                                                                                                                                                                                                                                                                                                                                                             T��      �               triple =  * 35�_�                       	    ����                                                                                                                                                                                                                                                                                                                                                             T��      �               triple = * 35�_�                       
    ����                                                                                                                                                                                                                                                                                                                                                             T��!     �               triple = ()* 35�_�                           ����                                                                                                                                                                                                                                                                                                                                                             T��"     �               triple = (* 35�_�                           ����                                                                                                                                                                                                                                                                                                                                                             T��#    �               triple = (* 3)5�_�                       
    ����                                                                                                                                                                                                                                                                                                                                                             T��)     �               double = (*2)5�_�                       
    ����                                                                                                                                                                                                                                                                                                                                                             T��)    �               double = (2)�             5�_�                       
    ����                                                                                                                                                                                                                                                                                                                                                             T��+     �               double = (2*)5�_�                        
    ����                                                                                                                                                                                                                                                                                                                                                             T��+    �               double = (*)�             5�_�      !                      ����                                                                                                                                                                                                                                                                                                                                                             T���     �               6reverseWords string = unwords (reverse (words string))5�_�       "           !          ����                                                                                                                                                                                                                                                                                                                                                             T���     �               /reverseWords = unwords (reverse (words string))5�_�   !   #           "          ����                                                                                                                                                                                                                                                                                                                                                             T���     �               0reverseWords = unwords. (reverse (words string))5�_�   "   $           #          ����                                                                                                                                                                                                                                                                                                                                                             T���     �               1reverseWords = unwords . (reverse (words string))�             5�_�   #   %           $           ����                                                                                                                                                                                                                                                                                                                                                             T���     �               /reverseWords = unwords . reverse (words string)5�_�   $   &           %      $    ����                                                                                                                                                                                                                                                                                                                                                             T���     �               1reverseWords = unwords . reverse . (words string)�             5�_�   %   '           &      /    ����                                                                                                                                                                                                                                                                                                                                                             T���    �               /reverseWords = unwords . reverse . words string5�_�   &   (           '          ����                                                                                                                                                                                                                                                                                                                                                             T���     �               (lowestEven xs = minimum (filter even xs)5�_�   '   )           (          ����                                                                                                                                                                                                                                                                                                                                                             T���     �               %lowestEven = minimum (filter even xs)5�_�   (   *           )          ����                                                                                                                                                                                                                                                                                                                                                             T���     �               'lowestEven = minimum . (filter even xs)�             5�_�   )   5           *      %    ����                                                                                                                                                                                                                                                                                                                                                             T���    �               %lowestEven = minimum . filter even xs5�_�   *   6   +       5      
    ����                                                                                                                                                                                                                                                                                                                               &                 v       T��     �                 M    head (sortBy (compare `on` length) (filter (all isLower) (words string)))�               5�_�   5   7           6      	    ����                                                                                                                                                                                                                                                                                                                               &                 v       T��     �                 K    head sortBy (compare `on` length) (filter (all isLower) (words string))5�_�   6   C           7      
    ����                                                                                                                                                                                                                                                                                                                               &                 v       T��    �                L    head $sortBy (compare `on` length) (filter (all isLower) (words string))5�_�   7   D   :       C      '    ����                                                                                                                                                                                                                                                                                                                               &                 v       T���    �                 M    head $ sortBy (compare `on` length) (filter (all isLower) (words string))5�_�   C   E           D      +    ����                                                                                                                                                                                                                                                                                                                               &                 v       T���     �                 O    head $ sortBy (compare `on` length) $ (filter (all isLower) (words string))�               5�_�   D   F           E      ?    ����                                                                                                                                                                                                                                                                                                                               &                 v       T��	   ! �                 M    head $ sortBy (compare `on` length) $ filter (all isLower) (words string)5�_�   E   G           F      B    ����                                                                                                                                                                                                                                                                                                                               &                 v       T��   " �                 O    head $ sortBy (compare `on` length) $ filter (all isLower) $ (words string)�               5�_�   F   H           G      M    ����                                                                                                                                                                                                                                                                                                                               &                 v       T��   # �                 M    head $ sortBy (compare `on` length) $ filter (all isLower) $ words string5�_�   G   J           H          ����                                                                                                                                                                                                                                                                                                                               &                 v       T��   $ �               longestLowercaseWord string =5�_�   H   K   I       J      	    ����                                                                                                                                                                                                                                                                                                                               &                 v       T��    % �                 F    head $ sortBy (compare `on` length) $ filter (all isLower) $ words5�_�   J   L           K      ?    ����                                                                                                                                                                                                                                                                                                                               &                 v       T��(   & �                 F    head . sortBy (compare `on` length) $ filter (all isLower) $ words5�_�   K   M           L      ?    ����                                                                                                                                                                                                                                                                                                                               &                 v       T��*     �                 F    head . sortBy (compare `on` length) $ filter (all isLower) . words5�_�   L   N           M      ?    ����                                                                                                                                                                                                                                                                                                                               &                 v       T��*   ' �                 E    head . sortBy (compare `on` length) $ filter (all isLower)  words5�_�   M   O           N      >    ����                                                                                                                                                                                                                                                                                                                               &                 v       T��/   ( �                 D    head . sortBy (compare `on` length) $ filter (all isLower) words5�_�   N   P           O      (    ����                                                                                                                                                                                                                                                                                                                               &                 v       T��4   ) �                 >    head . sortBy (compare `on` length) $ filter (all isLower)5�_�   O   Q           P      >    ����                                                                                                                                                                                                                                                                                                                               &                 v       T��7   - �                 >    head . sortBy (compare `on` length) . filter (all isLower)5�_�   P               Q          ����                                                                                                                                                                                                                                                                                                                               &                 v       T��t   + �   
            $hasUppercaseWord = any . all isUpper5�_�   H           J   I          ����                                                                                                                                                                                                                                                                                                                               &                 v       T��     �                D    head sortBy (compare `on` length) $ filter (all isLower) $ words5�_�   7   ;   8   C   :      ?    ����                                                                                                                                                                                                                                                                                                                               &                 v       T��7     �              �                K    head $ sortBy (compare `on` length) (filter (all isLower) words string)5�_�   :   <           ;      >    ����                                                                                                                                                                                                                                                                                                                               &                 v       T��8    �                M    head $ sortBy (compare `on` length) (filter (all isLower) $ words string)5�_�   ;   =           <      )    ����                                                                                                                                                                                                                                                                                                                               &                 v       T��j     �              �                K    head $ sortBy (compare `on` length) filter (all isLower) $ words string5�_�   <   >           =      (    ����                                                                                                                                                                                                                                                                                                                               &                 v       T��k    �                M    head $ sortBy (compare `on` length) $ filter (all isLower) $ words string5�_�   =   ?           >      M    ����                                                                                                                                                                                                                                                                                                                               &                 v       T��q     �                F    head $ sortBy (compare `on` length) $ filter (all isLower) $ words5�_�   >   @           ?          ����                                                                                                                                                                                                                                                                                                                               &                 v       T��s    �               longestLowercaseWord =5�_�   ?   A           @      ?    ����                                                                                                                                                                                                                                                                                                                               &                 v       T���    �                F    head $ sortBy (compare `on` length) $ filter (all isLower) . words5�_�   @   B           A      2    ����                                                                                                                                                                                                                                                                                                                               &                 v       T���     �              �                D    head $ sortBy (compare `on` length) $ filter all isLower . words5�_�   A               B      1    ����                                                                                                                                                                                                                                                                                                                               &                 v       T���    �                F    head $ sortBy (compare `on` length) $ filter $ all isLower . words5�_�   7   9       :   8      1    ����                                                                                                                                                                                                                                                                                                                               &                 v       T��/     �              �                K    head $ sortBy (compare `on` length) (filter all isLower (words string))5�_�   8               9      0    ����                                                                                                                                                                                                                                                                                                                               &                 v       T��0    �                M    head $ sortBy (compare `on` length) (filter $ all isLower (words string))5�_�   *   ,       5   +      	    ����                                                                                                                                                                                                                                                                                                                                                             T���     �                O    head $ (sortBy (compare `on` length) (filter (all isLower) (words string)))5�_�   +   -           ,          ����                                                                                                                                                                                                                                                                                                                                                             T���    �              �                M    head $ sortBy (compare `on` length) (filter (all isLower) (words string))5�_�   ,   .           -          ����                                                                                                                                                                                                                                                                                                                                                             T���     �                O    head $ sortBy . (compare `on` length) (filter (all isLower) (words string))5�_�   -   /           .          ����                                                                                                                                                                                                                                                                                                                                                             T���     �              �                M    head $ sortBy . compare `on` length (filter (all isLower) (words string))5�_�   .   0           /      '    ����                                                                                                                                                                                                                                                                                                                                                             T���    �                O    head $ sortBy . compare `on` length $ (filter (all isLower) (words string))5�_�   /   1           0          ����                                                                                                                                                                                                                                                                                                                                         *       v       T���     �              �                S    head $ sortBy . ( compare `on` length ) $ (filter (all isLower) (words string))5�_�   0   2           1          ����                                                                                                                                                                                                                                                                                                                                         *       v       T���     �                R    head $ sortBy . (compare `on` length ) $ (filter (all isLower) (words string))5�_�   1   3           2      (    ����                                                                                                                                                                                                                                                                                                                                         *       v       T���    �                Q    head $ sortBy . (compare `on` length) $ (filter (all isLower) (words string))5�_�   2   4           3          ����                                                                                                                                                                                                                                                                                                                                         *       v       T���    �                Q    head $ sortBy $ (compare `on` length) $ (filter (all isLower) (words string))5�_�   3               4          ����                                                                                                                                                                                                                                                                                                                                         *       v       T���    �              �                O    head $ sortBy $ compare `on` length $ (filter (all isLower) (words string))5��