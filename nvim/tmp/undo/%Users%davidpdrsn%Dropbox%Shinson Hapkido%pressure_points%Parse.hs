Vim�UnDo� ���������pt	�D^9�'=�ݍ�ٝ䦓�lV   8   P    n <- many1 . oneOf $ ['a'..'z'] ++ ['A'..'Z'] ++ "æøåÆØÅ-.()/:" ++ " "      G                       UA	�    _�                       
    ����                                                                                                                                                                                                                                                                                                                                                             UA8    �         8          me <- meridian5�_�      
                 
    ����                                                                                                                                                                                                                                                                                                                                                             UA;   	 �         8          nu <- number5�_�                
          ����                                                                                                                                                                                                                                                                                                                                                             UA�     �         8          me <- optionMaybe meridian5�_�   
                        ����                                                                                                                                                                                                                                                                                                                                                             UA�   
 �         8          nu <- optionMaybe number5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             UA�     �         8          symbol ","5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             UA�     �         8      %    _ <- optionMaybe $ try symbol ","5�_�                       	    ����                                                                                                                                                                                                                                                                                                                                                             UA�    �         8          _ <- try symbol ","5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             UA�    �         8          try symbol ","5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             UA	    �         8          try $ symbol ","5�_�                        G    ����                                                                                                                                                                                                                                                                                                                                                             UA	�    �         8      P    n <- many1 . oneOf $ ['a'..'z'] ++ ['A'..'Z'] ++ "æøåÆØÅ-.()/:" ++ " "5�_�             
      	        ����                                                                                                                                                                                                                                                                                                                                                             UA{    �   	   
   8    �      
   8      8p ++ ", " ++ m ++ " " ++ show noints :: Parser [A.Point]5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             UA�     �         8      $    me <- optionMaybe $ try meridian5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             UA�     �         8      !    nu <- optionMaybe $ trynumber5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             UA�     �         8      "    nu <- optionMaybe $ tryn umber5�_�      	                     ����                                                                                                                                                                                                                                                                                                                                                             UA�     �         8      !    nu <- optionMaybe $ trynumber5�_�                  	          ����                                                                                                                                                                                                                                                                                                                                                             UA�    �         8      "    nu <- optionMaybe $ try number5�_�                            ����                                                                                                                                                                                                                                                                                                                                                             UA9    �         8          return hi5��