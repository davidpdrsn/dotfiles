Vim�UnDo� ��4���Q��p>�Q���>��>PiB�c�      #        greater = filter (> x) rest            P       P   P   P    VFek    _�                             ����                                                                                                                                                                                                                                                                                                                                                             VFb�     �                   5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             VFb�     �                6module ${1:`!v vim2hs#haskell#snippets#module_name()`}�                 $0�                   ( $2�                  mod5�_�                          ����                                                                                                                                                                                                                                                                                                                                         
       v   
    VFb�     �                   ( 5�_�                          ����                                                                                                                                                                                                                                                                                                                                       
       v   
    VFb�    �                   ( bubble5�_�                            ����                                                                                                                                                                                                                                                                                                                                       
       v   
    VFb�     �                  5�_�      	                     ����                                                                                                                                                                                                                                                                                                                                       
       v   
    VFb�     �                 bubbleSort :: []5�_�      
           	          ����                                                                                                                                                                                                                                                                                                                                       
       v   
    VFb�     �                 bubbleSort :: []5�_�   	              
          ����                                                                                                                                                                                                                                                                                                                                       
       v   
    VFb�     �                 bubbleSort :: [Int]5�_�   
                        ����                                                                                                                                                                                                                                                                                                                                       
       v   
    VFb�     �                 bubbleSort :: [Int] -> []5�_�                           ����                                                                                                                                                                                                                                                                                                                                       
       v   
    VFb�     �                  �               5�_�                           ����                                                                                                                                                                                                                                                                                                                                       
       v   
    VFb�    �                 bubbleSort xs = er5�_�                           ����                                                                                                                                                                                                                                                                                                                                              v       VFb�    �               bubbleSort :: [Int] -> [Int]5�_�                           ����                                                                                                                                                                                                                                                                                                                                              V       VFc*     �                 bubbleSort xs = error "todo"5�_�                    	       ����                                                                                                                                                                                                                                                                                                                             &       	           V        VFcT     �                 '                  else bubbleBiggest xs5�_�                    	   3    ����                                                                                                                                                                                                                                                                                                                             &       	           V        VFcV    �                 3                  else bubbleSort (bubbleBiggest xs5�_�                    	   2    ����                                                                                                                                                                                                                                                                                                                             &       	           V        VFc`     �   	                                �   	            5�_�                           ����                                                                                                                                                                                                                                                                                                                             &       	           V        VFcd     �   
              sorted :: []5�_�                           ����                                                                                                                                                                                                                                                                                                                             &       	           V        VFc�     �                 	sorted []5�_�                           ����                                                                                                                                                                                                                                                                                                                             &       	           V        VFc�     �               �               5�_�                           ����                                                                                                                                                                                                                                                                                                                             &       	           V        VFc�     �                 sorted [] = True5�_�                           ����                                                                                                                                                                                                                                                                                                                             &       	           V        VFc�     �                 sorted [x] = True5�_�                           ����                                                                                                                                                                                                                                                                                                                             &       	           V        VFc�     �               �               5�_�                       
    ����                                                                                                                                                                                                                                                                                                                             &       	           V        VFc�     �                 sorted [_] = True5�_�                           ����                                                                                                                                                                                                                                                                                                                             &       	           V        VFc�     �                 sorted () = True5�_�                           ����                                                                                                                                                                                                                                                                                                                             &       	           V        VFc�     �                 sorted (x:y:xs) = True5�_�                           ����                                                                                                                                                                                                                                                                                                                             &       	           V        VFc�     �                 sorted (x : y : xs) = True5�_�                       '    ����                                                                                                                                                                                                                                                                                                                             &       	           V        VFc�     �                 (sorted (x : y : xs) = x < y && sorted ()5�_�                       +    ����                                                                                                                                                                                                                                                                                                                             +          ,       v   ,    VFc�    �                 .sorted (x : y : xs) = x < y && sorted (y : xs)5�_�                            ����                                                                                                                                                                                                                                                                                                                          	          	          V       VFc�     �                  �               5�_�      !                      ����                                                                                                                                                                                                                                                                                                                          	          	          V       VFc�     �                 bubbleBiggest :: []5�_�       "           !          ����                                                                                                                                                                                                                                                                                                                          	          	          V       VFc�     �                 bubbleBiggest :: []5�_�   !   #           "          ����                                                                                                                                                                                                                                                                                                                          	          	          V       VFc�     �                 bubbleBiggest :: [a] -> []5�_�   "   $           #          ����                                                                                                                                                                                                                                                                                                                          	          	          V       VFc�     �                  �               5�_�   #   %           $          ����                                                                                                                                                                                                                                                                                                                          	          	          V       VFc�     �                 bubbleBiggest []5�_�   $   &           %          ����                                                                                                                                                                                                                                                                                                                          	          	          V       VFc�     �                 bubbleBiggest [] = []5�_�   %   '           &          ����                                                                                                                                                                                                                                                                                                                          	          	          V       VFc�     �               �               5�_�   &   (           '          ����                                                                                                                                                                                                                                                                                                                          	          	          V       VFc�     �                 bubbleBiggest [] = []5�_�   '   )           (          ����                                                                                                                                                                                                                                                                                                                          	          	          V       VFc�     �                 bubbleBiggest [_] = []5�_�   (   *           )          ����                                                                                                                                                                                                                                                                                                                          	          	          V       VFc�     �                 bubbleBiggest [_] = [_]5�_�   )   +           *          ����                                                                                                                                                                                                                                                                                                                          	          	          V       VFc�     �                 bubbleBiggest [x] = [_]5�_�   *   ,           +          ����                                                                                                                                                                                                                                                                                                                          	          	          V       VFc�     �                  �               5�_�   +   -           ,          ����                                                                                                                                                                                                                                                                                                                          	          	          V       VFc�     �                 bubbleBiggest ()5�_�   ,   .           -          ����                                                                                                                                                                                                                                                                                                                          	          	          V       VFc�     �                 bubbleBiggest (x : y : rest)5�_�   -   /           .      9    ����                                                                                                                                                                                                                                                                                                                          	          	          V       VFd      �                 :                                 then x : bubbleBiggest ()5�_�   .   0           /      @    ����                                                                                                                                                                                                                                                                                                                          	          	          V       VFd     �                 !                                 �               5�_�   /   1           0      9    ����                                                                                                                                                                                                                                                                                                                          	          	          V       VFd
    �                 :                                 else y : bubbleBiggest ()5�_�   0   2           1          ����                                                                                                                                                                                                                                                                                                                          	          	          V       VFd     �               bubbleBiggest :: [a] -> [a]5�_�   1   3           2      
    ����                                                                                                                                                                                                                                                                                                                          	          	          V       VFd    �   
            sorted :: [a] -> Bool5�_�   2   4           3          ����                                                                                                                                                                                                                                                                                                                          	          	          V       VFd     �               bubbleSort :: [a] -> [a]5�_�   3   5           4           ����                                                                                                                                                                                                                                                                                                                          	          	          V       VFd%   	 �                 module Sort       ( bubbleSort       )     where    5�_�   4   6           5   	       ����                                                                                                                                                                                                                                                                                                                           	          	          v       VFd�     �      
         2sorted (x : y : rest) = x < y && sorted (y : rest)5�_�   5   7           6   	   '    ����                                                                                                                                                                                                                                                                                                                           	          	          v       VFd�     �      
         <sorted (x : y : rest) = x < y || x == y && sorted (y : rest)5�_�   6   8           7   	   "    ����                                                                                                                                                                                                                                                                                                                           	          	          v       VFd�     �      
         8sorted (x : y : rest) = x < y || () && sorted (y : rest)5�_�   7   9           8   	   "    ����                                                                                                                                                                                                                                                                                                                           	          	          v       VFd�     �      
         8sorted (x : y : rest) = x < y || () && sorted (y : rest)5�_�   8   :           9   	   '    ����                                                                                                                                                                                                                                                                                                                           	          	          v       VFd�     �      
         >sorted (x : y : rest) = x < y || (not ()) && sorted (y : rest)5�_�   9   ;           :   	   -    ����                                                                                                                                                                                                                                                                                                                           	          	          v       VFd�     �      
         Csorted (x : y : rest) = x < y || (not (x < y)) && sorted (y : rest)5�_�   :   <           ;   	   6    ����                                                                                                                                                                                                                                                                                                                           	          	          v       VFd�   
 �      
         Msorted (x : y : rest) = x < y || (not (x < y) && not ()) && sorted (y : rest)5�_�   ;   =           <           ����                                                                                                                                                                                                                                                                                                                           	          	          v       VFd�     �                 !                                 �               5�_�   <   >           =          ����                                                                                                                                                                                                                                                                                                                           	          	          v       VFd�     �                 quicksort :: Ord a => []5�_�   =   ?           >          ����                                                                                                                                                                                                                                                                                                                           	          	          v       VFe      �                 quicksort :: Ord a => [a] -> []5�_�   >   @           ?          ����                                                                                                                                                                                                                                                                                                                           	          	          v       VFe     �                 quicksort []5�_�   ?   A           @          ����                                                                                                                                                                                                                                                                                                                           	          	          v       VFe     �                 quicksort [] = []5�_�   @   B           A          ����                                                                                                                                                                                                                                                                                                                           	          	          v       VFe     �                  �               5�_�   A   C           B          ����                                                                                                                                                                                                                                                                                                                           	          	          v       VFe     �                 quicksort ()5�_�   B   D           C          ����                                                                                                                                                                                                                                                                                                                           	          	          v       VFe     �                 quicksort ()5�_�   C   E           D          ����                                                                                                                                                                                                                                                                                                                           	          	          v       VFe     �                 quicksort (x:rest)5�_�   D   F           E          ����                                                                                                                                                                                                                                                                                                                           	          	          v       VFe     �                 quicksort (x : rest)5�_�   E   H           F      !    ����                                                                                                                                                                                                                                                                                                                           	          	          v       VFe?     �                 "quicksort (x : rest) = lower ++ []5�_�   F   I   G       H          ����                                                                                                                                                                                                                                                                                                                           	          	          v       VFeH     �                 .quicksort (x : rest) = lower ++ [x] ++ greater5�_�   H   J           I          ����                                                                                                                                                                                                                                                                                                                           	          	          v       VFeI     �                  �               5�_�   I   K           J          ����                                                                                                                                                                                                                                                                                                                           	          	          v       VFeT     �                   where lesser = filter ()5�_�   J   L           K          ����                                                                                                                                                                                                                                                                                                                           	          	          v       VFeV     �                   where lesser = filter (< x)5�_�   K   M           L      !    ����                                                                                                                                                                                                                                                                                                                           	          	          v       VFeW     �                         �               5�_�   L   N           M          ����                                                                                                                                                                                                                                                                                                                           	          	          v       VFe\     �                         greater = filter ()5�_�   M   O           N          ����                                                                                                                                                                                                                                                                                                                              !          "       V   "    VFeb     �               "  where lesser = filter (< x) rest5�_�   N   P           O          ����                                                                                                                                                                                                                                                                                                                              !          "       V   "    VFeh    �                 #        greater = filter (> x) rest5�_�   O               P          ����                                                                                                                                                                                                                                                                                                                              !          "       V   "    VFej    �                 -        greater = quicksort filter (> x) rest5�_�   F           H   G          ����                                                                                                                                                                                                                                                                                                                           	          	          v       VFeG     �                -quicksort (x : rest) =lower ++ [x] ++ greater5�_�                           ����                                                                                                                                                                                                                                                                                                                                       
       v   
    VFb�     �                   ( Bubble5�_�                           ����                                                                                                                                                                                                                                                                                                                                         
       v   
    VFb�     �              5��