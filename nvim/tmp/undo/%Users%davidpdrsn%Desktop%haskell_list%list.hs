Vim�UnDo� �NA-�*)p�UA����yz��>�^$��      bind Nil _ = Nil            >       >   >   >    UCB�    _�                             ����                                                                                                                                                                                                                                                                                                                                                             UCAb     �               5�_�                            ����                                                                                                                                                                                                                                                                                                                                                             UCAd    �                   �                 5�_�                            ����                                                                                                                                                                                                                                                                                                                                                             UCAj     �                  5�_�                       	    ����                                                                                                                                                                                                                                                                                                                                                             UCAs     �                 data List = Cons5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             UCAt    �                 data List a = Cons5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             UCA�     �                 data List a = Cons a               | Nil5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             UCA�     �                 data List a = Cons a | Nil5�_�      	                     ����                                                                                                                                                                                                                                                                                                                                                             UCA�    �                 data List a = Cons () | Nil5�_�      
           	          ����                                                                                                                                                                                                                                                                                                                                                             UCA�     �                             �               5�_�   	              
      
    ����                                                                                                                                                                                                                                                                                                                                                             UCA�     �                 l = Cons ()5�_�   
                        ����                                                                                                                                                                                                                                                                                                                                                             UCA�     �                 l = Cons (1, Cons ())5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             UCA�    �                 l = Cons (1, Cons (2, Cons ()))5�_�                          ����                                                                                                                                                                                                                                                                                                                                                             UCA�     �                 %l = Cons (1, Cons (2, Cons (3, Nil)))5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             UCA�    �                 %l = Cons (1, Cons (2, Cons (4, Nil)))5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             UCA�     �                  �               5�_�                            ����                                                                                                                                                                                                                                                                                                                                                             UCA�     �                 5�_�                            ����                                                                                                                                                                                                                                                                                                                                                             UCA�     �                 �             5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             UCA�    �         	      import Prelude hiding ()5�_�                    	        ����                                                                                                                                                                                                                                                                                                                                                             UCA�     �   	               �   	            5�_�                    
       ����                                                                                                                                                                                                                                                                                                                                                             UCA�     �   	              map :: List a 5�_�                    
       ����                                                                                                                                                                                                                                                                                                                                                             UCA�     �   	              map :: f -> List a 5�_�                    
       ����                                                                                                                                                                                                                                                                                                                                                             UCA�     �   	              map :: f -> List a5�_�                    
       ����                                                                                                                                                                                                                                                                                                                                                             UCA�     �   	              map :: () -> List a5�_�                    
       ����                                                                                                                                                                                                                                                                                                                                                             UCA�     �   	              map :: (a ->) -> List a5�_�                    
       ����                                                                                                                                                                                                                                                                                                                                                             UCA�     �   	              	map :: ()5�_�                    
       ����                                                                                                                                                                                                                                                                                                                                                             UCA�    �   	              map :: (a -> b)5�_�                    
   "    ����                                                                                                                                                                                                                                                                                                                                                             UCA�   	 �   
               �   
            5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             UCA�   
 �   
              map f Nil = Nil5�_�                            ����                                                                                                                                                                                                                                                                                                                                                             UCA�     �               �               5�_�      !                      ����                                                                                                                                                                                                                                                                                                                                                             UCA�     �                 map _ Nil = Nil5�_�       "           !          ����                                                                                                                                                                                                                                                                                                                                                             UCB      �                 map f Nil = Nil5�_�   !   #           "          ����                                                                                                                                                                                                                                                                                                                                                             UCB     �                 map f () = Nil5�_�   "   $           #          ����                                                                                                                                                                                                                                                                                                                                                             UCB    �                 map f (Cons ()) = Nil5�_�   #   %           $          ����                                                                                                                                                                                                                                                                                                                                                             UCB     �                 map f (Cons (head, tail)) = Nil5�_�   $   &           %      "    ����                                                                                                                                                                                                                                                                                                                                                             UCB	    �                 #map f (Cons (head, tail)) = Cons ()5�_�   %   '           &      $    ����                                                                                                                                                                                                                                                                                                                               $          '       v   '    UCB     �                 5map f (Cons (head, tail)) = Cons (f head, map f tail)5�_�   &   (           '      ,    ����                                                                                                                                                                                                                                                                                                                               ,          /       v   /    UCB    �                 1map f (Cons (hd, tail)) = Cons (f hd, map f tail)5�_�   '   )           (          ����                                                                                                                                                                                                                                                                                                                                                v       UCB2     �                  �               5�_�   (   *           )      
    ����                                                                                                                                                                                                                                                                                                                                                v       UCB;     �                 l' = map ()5�_�   )   +           *          ����                                                                                                                                                                                                                                                                                                                                                v       UCB=    �                 l' = map (+1)5�_�   *   ,           +          ����                                                                                                                                                                                                                                                                                                                                                v       UCB?     �                  �               5�_�   +   -           ,          ����                                                                                                                                                                                                                                                                                                                                                v       UCBA     �                 main :: IO ()5�_�   ,   .           -           ����                                                                                                                                                                                                                                                                                                                                                v       UCBD     �                �             5�_�   -   /           .          ����                                                                                                                                                                                                                                                                                                                                                v       UCBH     �               l :: List Int5�_�   .   0           /           ����                                                                                                                                                                                                                                                                                                                                                v       UCBI    �                  5�_�   /   1           0          ����                                                                                                                                                                                                                                                                                                                                                v       UCBT    �                     print l'5�_�   0   2           1          ����                                                                                                                                                                                                                                                                                                                                                v       UCB^     �               $data List a = Cons (a, List a) | Nil5�_�   1   3           2          ����                                                                                                                                                                                                                                                                                                                                                v       UCB^     �                           �             5�_�   2   4           3          ����                                                                                                                                                                                                                                                                                                                                                v       UCBd    �                           deriving ()5�_�   3   5           4          ����                                                                                                                                                                                                                                                                                                                                                v       UCB�     �                �             5�_�   4   6           5      	    ����                                                                                                                                                                                                                                                                                                                                                v       UCB�     �               
bind :: ()5�_�   5   7           6          ����                                                                                                                                                                                                                                                                                                                                                v       UCB�     �               bind :: List a -> ()5�_�   6   8           7          ����                                                                                                                                                                                                                                                                                                                                                v       UCB�     �               bind :: List a -> (a -> List b)5�_�   7   9           8      (    ����                                                                                                                                                                                                                                                                                                                                                v       UCB�     �                �             5�_�   8   :           9          ����                                                                                                                                                                                                                                                                                                                                                v       UCB�     �             �             5�_�   9   ;           :          ����                                                                                                                                                                                                                                                                                                                                                v       UCB�     �               bind Nil _ = Nil5�_�   :   <           ;          ����                                                                                                                                                                                                                                                                                                                                                v       UCB�     �               bind () _ = Nil5�_�   ;   =           <          ����                                                                                                                                                                                                                                                                                                                                                v       UCB�     �               bind (Cons ()) _ = Nil5�_�   <   >           =          ����                                                                                                                                                                                                                                                                                                                                                v       UCB�    �               bind (Cons (hd, tl)) _ = Nil5�_�   =               >          ����                                                                                                                                                                                                                                                                                                                                                v       UCB�    �               bind (Cons (hd, tl)) _ = _5�_�                          ����                                                                                                                                                                                                                                                                                                                                                             UCA�     �                &l = Cons (1, Cons (2, Cons ('', Nil)))5�_�                            ����                                                                                                                                                                                                                                                                                                                                                             UCA�    �                'l = Cons (1, Cons (2, Cons ('a', Nil)))5��