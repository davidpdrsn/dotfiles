Vim�UnDo� ����v�m%ٝ6
<oN�`�EBZ?���6                      ?       ?   ?   ?    U5�   ( _�                             ����                                                                                                                                                                                                                                                                                                                                                  V        U2r?     �      
       �      	       5�_�                           ����                                                                                                                                                                                                                                                                                                                                                  V        U2rA     �                   click_link "Sign in"5�_�                    	       ����                                                                                                                                                                                                                                                                                                                                                  V        U2rD     �      	              click_link "Sign in"5�_�                            ����                                                                                                                                                                                                                                                                                                                                                  V        U2rE     �      
       �      	       5�_�                    	       ����                                                                                                                                                                                                                                                                                                                                                  V        U2rF    �      
         &    fill_in "Password", with: "secret"5�_�                           ����                                                                                                                                                                                                                                                                                                                                                  V        U2r    �      	             �      	       5�_�                    
       ����                                                                                                                                                                                                                                                                                                                                       	           V        U2r�     �   	   
          3    fill_in "Password Confirmation", with: "secret"5�_�      	                      ����                                                                                                                                                                                                                                                                                                                                       	           V        U2r�    �                    save_and_open_page5�_�      
           	          ����                                                                                                                                                                                                                                                                                                                                                V       U2r�   	 �      
             �      
       5�_�   	              
   	       ����                                                                                                                                                                                                                                                                                                                                                V       U2r�     �      
             save_and_open_page5�_�   
                 	       ����                                                                                                                                                                                                                                                                                                                                                V       U2r�     �      
             ""5�_�                    	       ����                                                                                                                                                                                                                                                                                                                                                V       U2r�    �      
             click_button ""5�_�                    	       ����                                                                                                                                                                                                                                                                                                                                                V       U2r�    �   	                �   	          5�_�                    
       ����                                                                                                                                                                                                                                                                                                                                                V       U2r�    �   	   
              save_and_open_page5�_�                    	        ����                                                                                                                                                                                                                                                                                                                                                V       U2r�     �   	                �   	          5�_�                    
        ����                                                                                                                                                                                                                                                                                                                                                V       U2r�    �   	   
              	    click5�_�                           ����                                                                                                                                                                                                                                                                                                                                                V       U2r�    �                 scenario "user signs in" do5�_�                    
        ����                                                                                                                                                                                                                                                                                                                                              V       U2sH     �   
                �   
          5�_�                       &    ����                                                                                                                                                                                                                                                                                                                                              V       U2sP    �   
            '    expect(page).to_not have_content ""5�_�                       ,    ����                                                                                                                                                                                                                                                                                                                                              V       U2sZ     �             �             5�_�                       +    ����                                                                                                                                                                                                                                                                                                                                              V       U2s[    �               .    expect(page).to_not have_content "Sign in"5�_�                       ,    ����                                                                                                                                                                                                                                                                                                                                              V       U2s}     �             �             5�_�                           ����                                                                                                                                                                                                                                                                                                                                              V       U2s     �               .    expect(page).to_not have_content "Sign up"5�_�                           ����                                                                                                                                                                                                                                                                                                                                              V       U2s     �               -    expect(page).tonot have_content "Sign up"5�_�                           ����                                                                                                                                                                                                                                                                                                                                              V       U2s     �               ,    expect(page).toot have_content "Sign up"5�_�                           ����                                                                                                                                                                                                                                                                                                                                              V       U2s     �               +    expect(page).tot have_content "Sign up"5�_�                       '    ����                                                                                                                                                                                                                                                                                                                                              V       U2s�    �               *    expect(page).to have_content "Sign up"5�_�                    	       ����                                                                                                                                                                                                                                                                                                                                    	          V       U2s�     �   	                �   	          5�_�                            ����                                                                                                                                                                                                                                                                                                                                    	          V       U2s�     �   
              5�_�                            ����                                                                                                                                                                                                                                                                                                                                    	          V       U2s�    �   
              5�_�                     	        ����                                                                                                                                                                                                                                                                                                                                               V        U2Ҧ    �   	                �   	          5�_�      !                       ����                                                                                                                                                                                                                                                                                                                                               V        U2Ұ     �   
             5�_�       "           !          ����                                                                                                                                                                                                                                                                                                                                               V        U2ҵ     �   
                sign_up email: ""5�_�   !   #           "      $    ����                                                                                                                                                                                                                                                                                                                                               V        U2ҹ     �   
            $    sign_up email: "bob@example.com"5�_�   "   $           #      1    ����                                                                                                                                                                                                                                                                                                                                               V        U2һ    �   
            2    sign_up email: "bob@example.com", password: ""5�_�   #   %           $          ����                                                                                                                                                                                                                                                                                                                          	                    V   $    U2��    �      
              click_button "Sign up"�      	          &    fill_in "Password", with: "secret"�                ,    fill_in "Email", with: "bob@example.com"�                    click_link "Sign up"�                    visit root_path5�_�   $   &           %           ����                                                                                                                                                                                                                                                                                                                                    
           V       U2��    �                    # visit root_path       # click_link "Sign up"   .    # fill_in "Email", with: "bob@example.com"   (    # fill_in "Password", with: "secret"       # click_button "Sign up"    5�_�   %   '           &      $    ����                                                                                                                                                                                                                                                                                                                                               V       U2�
    �               8    sign_up email: "bob@example.com", password: "secret"5�_�   &   (           '          ����                                                                                                                                                                                                                                                                                                                                                           U5~     �                 scenario "user signs in" do5�_�   '   )           (          ����                                                                                                                                                                                                                                                                                                                                                           U5�     �                 scenario "$1" do�                 end�                   $2�                 sce�                 �             5�_�   (   *           )          ����                                                                                                                                                                                                                                                                                                                                                           U5�     �                   5�_�   )   +           *          ����                                                                                                                                                                                                                                                                                                                                                           U5�     �                   create :user, email: ""5�_�   *   ,           +      *    ����                                                                                                                                                                                                                                                                                                                                                           U5�     �               *    create :user, email: "bob@example.com"5�_�   +   -           ,      7    ����                                                                                                                                                                                                                                                                                                                                                           U5�     �               8    create :user, email: "bob@example.com", password: ""5�_�   ,   .           -      <    ����                                                                                                                                                                                                                                                                                                                                                           U5�     �                   �             5�_�   -   /           .          ����                                                                                                                                                                                                                                                                                                                                                           U5�     �                   �             5�_�   .   0           /          ����                                                                                                                                                                                                                                                                                                                                                           U5�     �                   click_link ""5�_�   /   1           0          ����                                                                                                                                                                                                                                                                                                                                                           U5�     �                   �             5�_�   0   2           1          ����                                                                                                                                                                                                                                                                                                                                                           U5�     �                   fill_in5�_�   1   3           2          ����                                                                                                                                                                                                                                                                                                                                                           U5�     �                   fill_in ""5�_�   2   4           3          ����                                                                                                                                                                                                                                                                                                                                                           U5�     �                   fill_in "Email", withL ""5�_�   3   5           4          ����                                                                                                                                                                                                                                                                                                                                                           U5�     �                   fill_in "Email", with: ""5�_�   4   6           5      *    ����                                                                                                                                                                                                                                                                                                                                                           U5�     �             �             5�_�   5   7           6          ����                                                                                                                                                                                                                                                                                                                                                           U5�     �               ,    fill_in "Email", with: "bob@example.com"5�_�   6   8           7          ����                                                                                                                                                                                                                                                                                                                                                           U5�     �               /    fill_in "Password", with: "bob@example.com"5�_�   7   9           8      $    ����                                                                                                                                                                                                                                                                                                                                                           U5�     �                   �             5�_�   8   :           9          ����                                                                                                                                                                                                                                                                                                                                                           U5�     �                   click_buttong ""5�_�   9   ;           :          ����                                                                                                                                                                                                                                                                                                                                                           U5�     �                   e�                   �             5�_�   :   =           ;      "    ����                                                                                                                                                                                                                                                                                                                                                           U5�   ! �               #    expect(page).to have_content ""5�_�   ;   >   <       =          ����                                                                                                                                                                                                                                                                                                                                                           U5�   # �                   click_buttong "Sign in"5�_�   =   ?           >          ����                                                                                                                                                                                                                                                                                                                                                           U5�     �                   �             5�_�   >               ?      &    ����                                                                                                                                                                                                                                                                                                                                                           U5�   ( �               '    expect(page).not_to have_content ""5�_�   ;           =   <          ����                                                                                                                                                                                                                                                                                                                                                           U5�     �                   click_buttong"Sign in"5��