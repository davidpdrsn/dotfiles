Vim�UnDo� 0�@'�aCK{�n8� w���Ȋ�:��[��           x <- parseFile "sample.lisp"            M       M   M   M    V)��   0 _�                             ����                                                                                                                                                                                                                                                                                                                                                             V)PJ     �                   5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             V)PJ    �                 :main = ${1:error "undefined: \`main' in `!v expand('%')`"}�                  main5�_�                           ����                                                                                                                                                                                                                                                                                                                                                 V        V)U�     �                 /main = error "undefined: `main' in src/Main.hs"5�_�                           ����                                                                                                                                                                                                                                                                                                                                                 V        V)U�     �                     x <- readFile ""5�_�                           ����                                                                                                                                                                                                                                                                                                                                                 V        V)U�     �                     �               5�_�                           ����                                                                                                                                                                                                                                                                                                                                                 V        V)U�     �                     puts5�_�                           ����                                                                                                                                                                                                                                                                                                                                                 V        V)U�    �                     putStrLn5�_�      
                     ����                                                                                                                                                                                                                                                                                                                                                 V        V)U�    �                 �             5�_�         	       
          ����                                                                                                                                                                                                                                                                                                                                                 V        V)U�    �                     putStrLn x5�_�   
                        ����                                                                                                                                                                                                                                                                                                                                                 V        V)\+     �      	   	          �      	       5�_�                    	       ����                                                                                                                                                                                                                                                                                                                                                 V        V)\1    �                     putStrLn $ show $ parse x5�_�                           ����                                                                                                                                                                                                                                                                                                                                                 V        V)\6     �      	   	          ast <- parse x5�_�                           ����                                                                                                                                                                                                                                                                                                                                                 V        V)\8    �      	   	          let ast <- parse x5�_�                    	       ����                                                                                                                                                                                                                                                                                                                                                 V        V)\I     �   	                  �   	            5�_�                    
       ����                                                                                                                                                                                                                                                                                                                                                 V        V)\K     �   	                  puts5�_�                    
       ����                                                                                                                                                                                                                                                                                                                                                 V        V)\K     �   	                  putStrLn5�_�                    
       ����                                                                                                                                                                                                                                                                                                                                                 V        V)\K     �   	                  putStrLn ""5�_�                    
       ����                                                                                                                                                                                                                                                                                                                                                 V        V)\L     �   
                  �   
            5�_�                           ����                                                                                                                                                                                                                                                                                                                                                 V        V)\M     �   
                  puts5�_�                           ����                                                                                                                                                                                                                                                                                                                                                 V        V)\N     �   
                  putStrLn5�_�                           ����                                                                                                                                                                                                                                                                                                                                                 V        V)\V    �                     return ()5�_�                           ����                                                                                                                                                                                                                                                                                                                                                 V        V)\Z     �             �             5�_�                           ����                                                                                                                                                                                                                                                                                                                                                 V        V)\[   	 �               import Parse5�_�                          ����                                                                                                                                                                                                                                                                                                                                                 V        V)\~     �                   interpret ast5�_�                           ����                                                                                                                                                                                                                                                                                                                                                 V        V)\�   
 �                   (lift interpret ast5�_�                           ����                                                                                                                                                                                                                                                                                                                                                 V        V)\�     �                   (lift interpret) ast5�_�                           ����                                                                                                                                                                                                                                                                                                                                                 V        V)\�     �                     Left err -> puts5�_�                           ����                                                                                                                                                                                                                                                                                                                                                 V        V)\�     �                     Left err -> putStrLn5�_�                            ����                                                                                                                                                                                                                                                                                                                                                 V        V)\�     �                     �             5�_�      !                      ����                                                                                                                                                                                                                                                                                                                                                 V        V)\�    �                     Right()5�_�       "           !          ����                                                                                                                                                                                                                                                                                                                                                 V        V)\�    �                     Left err -> putStrLn err5�_�   !   #           "          ����                                                                                                                                                                                                                                                                                                                                                  V        V)�D     �               "      Right ast' -> interpret ast'5�_�   "   $           #          ����                                                                                                                                                                                                                                                                                                                                                  V        V)�H    �                       interpret ast'5�_�   #   %           $          ����                                                                                                                                                                                                                                                                                                                                                  V        V)�R    �                       �             5�_�   $   &           %      	    ����                                                                                                                                                                                                                                                                                                                                                  V        V)�a     �               	        _5�_�   %   '           &          ����                                                                                                                                                                                                                                                                                                                                                  V        V)�e    �                       return ()5�_�   &   (           '          ����                                                                                                                                                                                                                                                                                                                                                  V        V)�h    �                       res <- interpret ast'5�_�   '   )           (          ����                                                                                                                                                                                                                                                                                                                                                  V        V)�i     �                       _ <- interpret ast'5�_�   (   *           )          ����                                                                                                                                                                                                                                                                                                                                                  V        V)�i     �                        <- interpret ast'5�_�   )   +           *          ����                                                                                                                                                                                                                                                                                                                                                  V        V)�i     �                       <- interpret ast'5�_�   *   ,           +          ����                                                                                                                                                                                                                                                                                                                                                  V        V)�i     �                       - interpret ast'5�_�   +   -           ,          ����                                                                                                                                                                                                                                                                                                                                                  V        V)�i    �                        interpret ast'5�_�   ,   .           -          ����                                                                                                                                                                                                                                                                                                                                                  V        V)�l     �                     Right ast' -> do5�_�   -   /           .          ����                                                                                                                                                                                                                                                                                                                                                  V        V)�m     �                     Right ast' ->           interpret ast'5�_�   .   0           /          ����                                                                                                                                                                                                                                                                                                                                                  V        V)�m     �               "      Right ast' -> interpret ast'           return ()5�_�   /   1           0      "    ����                                                                                                                                                                                                                                                                                                                                                  V        V)�n    �               ,      Right ast' -> interpret ast' return ()5�_�   0   3           1          ����                                                                                                                                                                                                                                                                                                                                                  V        V)�r    �                    return ()5�_�   1   4   2       3          ����                                                                                                                                                                                                                                                                                                                                                  V        V)��     �                 /      Right ast' -> interpret ast' >> return ()5�_�   3   5           4      !    ����                                                                                                                                                                                                                                                                                                                                                  V        V)��     �                 !      interpret ast' >> return ()5�_�   4   6           5          ����                                                                                                                                                                                                                                                                                                                                                  V        V)��     �                       interpret ast'5�_�   5   7           6          ����                                                                                                                                                                                                                                                                                                                                                  V        V)��     �                        �               5�_�   6   9           7          ����                                                                                                                                                                                                                                                                                                                                                  V        V)��    �                          Rigth _ -> return ()5�_�   7   :   8       9          ����                                                                                                                                                                                                                                                                                                                                                  V        V)��     �                        Left err -> show err5�_�   9   ;           :          ����                                                                                                                                                                                                                                                                                                                                                  V        V)��     �               !         Left err -> putsshow err5�_�   :   <           ;          ����                                                                                                                                                                                                                                                                                                                                                  V        V)��    �               %         Left err -> putStrLnshow err5�_�   ;   =           <           ����                                                                                                                                                                                                                                                                                                                                                V       V)�     �                         Rigth _ -> return ()�                (         Left err -> putStrLn $ show err�                       case res of5�_�   <   >           =           ����                                                                                                                                                                                                                                                                                                                                                V       V)�    �                      �             5�_�   =   ?           >          ����                                                                                                                                                                                                                                                                                                                                                V       V)�     �                     Right ast' ->5�_�   >   @           ?          ����                                                                                                                                                                                                                                                                                                                                                V       V)�     �                       _5�_�   ?   A           @           ����                                                                                                                                                                                                                                                                                                                                                V       V)�    ! �                        --   Rigth _ -> return ()�                +       --   Left err -> putStrLn $ show err�                       -- case res of5�_�   @   C           A      	    ����                                                                                                                                                                                                                                                                                                                                                V       V)�#   $ �                          Rigth _ -> return ()5�_�   A   D   B       C   
       ����                                                                                                                                                                                                                                                                                                                                                V       V)�   % �   
                 putStrLn ""�   	                 putStrLn $ show $ ast5�_�   C   E           D   
       ����                                                                                                                                                                                                                                                                                                                                                V       V)�b   & �   
                 -- putStrLn ""�   	                 -- putStrLn $ show $ ast5�_�   D   F           E          ����                                                                                                                                                                                                                                                                                                                                                V       V)�]     �               %      Left err -> putStrLn $ show err5�_�   E   G           F          ����                                                                                                                                                                                                                                                                                                                                                V       V)�`   ' �                     Left err -> return ()5�_�   F   H           G          ����                                                                                                                                                                                                                                                                                                                                                V       V)�g   * �                     Left err -> return ()5�_�   G   I           H   
       ����                                                                                                                                                                                                                                                                                                                                                V       V)�Y   + �   
                 putStrLn ""�   	                 putStrLn $ show $ ast5�_�   H   K           I   
        ����                                                                                                                                                                                                                                                                                                                            
                    V       V)��   - �   
                 -- putStrLn ""�   	                 -- putStrLn $ show $ ast5�_�   I   L   J       K      	    ����                                                                                                                                                                                                                                                                                                                            
                    V       V)�w   / �      	             x <- readFile "sample.lisp"5�_�   K   M           L   	       ����                                                                                                                                                                                                                                                                                                                            
                    V       V)��     �      	              let ast = parse x5�_�   L               M          ����                                                                                                                                                                                                                                                                                                                            	          
          V       V)��   0 �      	              x <- parseFile "sample.lisp"5�_�   I           K   J      	    ����                                                                                                                                                                                                                                                                                                                            
                    V       V)�u     �      	             x <- parse5�_�   A           C   B          ����                                                                                                                                                                                                                                                                                                                                                V       V)��     �                         Right  -> return ()5�_�   7           9   8          ����                                                                                                                                                                                                                                                                                                                                                  V        V)��     �             �                        Left err -> s how err5�_�   1           3   2          ����                                                                                                                                                                                                                                                                                                                                                  V        V)�    �   
                 -- putStrLn ""�                    -- case ast of�                (    --   Left err -> putStrLn $ show err�                2    --   Right ast' -> interpret ast' >> return ()5�_�                           ����                                                                                                                                                                                                                                                                                                                                                 V        V)\|     �                   iftnterpret ast5�_�              
   	          ����                                                                                                                                                                                                                                                                                                                                                 V        V)U�     �      	              5��