Vim�UnDo� &�����*��q�u��FT:�ܛ2�P��������      &      toWidget [hamlet|Its all good!|]                            U�    _�                             ����                                                                                                                                                                                                                                                                                                                                       $           V        U�    �                postHomeR :: Handler Html   postHomeR = do   A    ((result, formWidget), formEnctype) <- runFormPost sampleForm   )    let handlerName = "postHomeR" :: Text   #        submission = case result of   '            FormSuccess res -> Just res               _ -> Nothing           defaultLayout $ do           aDomId <- newIdent   $        setTitle "Welcome To Yesod!"            $(widgetFile "homepage")    5�_�                            ����                                                                                                                                                                                                                                                                                                                                                  V        U�    �                E-- This is a handler function for the GET request method on the HomeR   A-- resource pattern. All of your resource patterns are defined in   -- config/routes   --   J-- The majority of the code you will write in Yesod lives in these handler   E-- functions. You can spread them across multiple files if you are so   0-- inclined, or create a single monolithic file.5�_�                            ����                                                                                                                                                                                                                                                                                                                                                  V        U�    �                    #sampleForm :: Form (FileInfo, Text)   6sampleForm = renderBootstrap3 BootstrapBasicForm $ (,)   $    <$> fileAFormReq "Choose a file"   E    <*> areq textField (withSmallInput "What's on the file?") Nothing5�_�      	             	        ����                                                                                                                                                                                                                                                                                                                            	                    V       U
    �      	          <    (formWidget, formEnctype) <- generateFormPost sampleForm   6    let submission = Nothing :: Maybe (FileInfo, Text)   (        handlerName = "getHomeR" :: Text5�_�      
          	   
       ����                                                                                                                                                                                                                                                                                                                            
          
          v       U�    �   	   
                  aDomId <- newIdent   $        setTitle "Welcome To Yesod!"5�_�   	              
   
       ����                                                                                                                                                                                                                                                                                                                                      
          V       U�     �   
            �   
            5�_�   
                         ����                                                                                                                                                                                                                                                                                                                                      
          V       U�     �   
          5�_�                           ����                                                                                                                                                                                                                                                                                                                                                       U�     �               getR :: Handler Html�               getHomeR :: Handler Html   getHomeR = do5�_�                           ����                                                                                                                                                                                                                                                                                                                                                       U�     �                          $(widgetFile "homepage")5�_�                       	    ����                                                                                                                                                                                                                                                                                                                                                       U�     �                 
        {}5�_�                       	    ����                                                                                                                                                                                                                                                                                                                                                       U�     �                 
        []5�_�                           ����                                                                                                                                                                                                                                                                                                                                                       U�     �                         [|hamlet||]5�_�                       	    ����                                                                                                                                                                                                                                                                                                                                                       U�    �                          [|hamlet|Its all good!|]5�_�                           ����                                                                                                                                                                                                                                                                                                                                                       U(    �                         [hamlet|Its all good!|]5�_�                           ����                                                                                                                                                                                                                                                                                                                                                V       UR    �                Iimport Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3,   -                              withSmallInput)5�_�                           ����                                                                                                                                                                                                                                                                                                                            
                    V       Un     �               �               5�_�                           ����                                                                                                                                                                                                                                                                                                                                         %       v   %    Uq    �                 (        toWidget [hamlet|Its all good!|]5�_�                           ����                                                                                                                                                                                                                                                                                                                                         %       v   %    U�     �                #        toWidget [hamlet|Hi again|]5�_�                           ����                                                                                                                                                                                                                                                                                                                                         %       v   %    U�     �                     �             5�_�                            ����                                                                                                                                                                                                                                                                                                                                                V       U�     �                (        toWidget [hamlet|Its all good!|]5�_�                       	    ����                                                                                                                                                                                                                                                                                                                                                V       U�     �               	      set5�_�                           ����                                                                                                                                                                                                                                                                                                                                                V       U�    �                     setMessage ""5�_�                            ����                                                                                                                                                                                                                                                                                                                                                V       U�    �                     �             5�_�                           ����                                                                                                                                                                                                                                                                                                                                                V       U�     �                      mmsg <- getMessage5�_�                           ����                                                                                                                                                                                                                                                                                                                                                V       U�    �                "      setMessage "message is here"5�_�                            ����                                                                                                                                                                                                                                                                                                                                                V       U�    �                      toWidget 1235�_�             	      
       ����                                                                                                                                                                                                                                                                                                                            	          	          V       U   
 �   	           5�_�                     
       ����                                                                                                                                                                                                                                                                                                                            	          	          V       U    �   	                      #(widgetFile "homepage")5�_�                          ����                                                                                                                                                                                                                                                                                                                                      	          V       U�     �              5�_�                     	       ����                                                                                                                                                                                                                                                                                                                            	          	          V       U�    �              5��