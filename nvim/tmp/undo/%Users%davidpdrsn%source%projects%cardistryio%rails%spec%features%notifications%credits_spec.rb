Vim�UnDo� Չ�p��,R��kC�^��p��j�_�	wD�
   %   feature "creating move" do      	      %       %   %   %    U�1    _�                             ����                                                                                                                                                                                                                                                                                                                                                             U�,�     �                   �               5�_�                            ����                                                                                                                                                                                                                                                                                                                                                             U�,�     �                 �              5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             U�,�    �                   �               5�_�                           ����                                                                                                                                                                                                                                                                                                                               $          -       v   -    U�,�     �               %  scenario "creates the move", :js do5�_�                           ����                                                                                                                                                                                                                                                                                                                               $          -       v   -    U�-     �             5�_�                           ����                                                                                                                                                                                                                                                                                                                               $          -       v   -    U�-     �                  end�                    end�                0      expect(page).to have_content "@davidpdrsn"�                    within ".credits" do�                (    expect(page).to have_content "Sybil"�                 �                    click_button "Add move"�                    end�                      click_link "@davidpdrsn"�   
             '      fill_in "Username", with: "david"�   	                 within ".add-credits" do�      
          -    fill_in "Description", with: "Old school"�      	          !    fill_in "Name", with: "Sybil"�                !    visit new_move_path(as: user)�                /    user = create :user, username: "davidpdrsn"�                E  scenario "doesn't notifier users if they credit themselves", :js do5�_�                           ����                                                                                                                                                                                                                                                                                                                               $          -       v   -    U�-     �                 �             5�_�      	                     ����                                                                                                                                                                                                                                                                                                                               $          -       v   -    U�-     �                 end�                   $2�                 scenario "$1" do�                 sce5�_�      
           	          ����                                                                                                                                                                                                                                                                                                                            	   $       	   -       v   -    U�-     �                 scenario "" do5�_�   	              
          ����                                                                                                                                                                                                                                                                                                                            	   $       	   -       v   -    U�-     �                    5�_�   
                         ����                                                                                                                                                                                                                                                                                                                                                 V        U�-     �             �             5�_�                           ����                                                                                                                                                                                                                                                                                                                                      #           V        U�-     �                	  #   end�                2  #     expect(page).to have_content "@davidpdrsn"�                  #   within ".credits" do�                *  #   expect(page).to have_content "Sybil"�                 �                  #   click_button "Add move"�                	  #   end�   
                #     click_link "@davidpdrsn"�   	             )  #     fill_in "Username", with: "david"�      
            #   within ".add-credits" do�      	          /  #   fill_in "Description", with: "Old school"�                #  #   fill_in "Name", with: "Sybil"�                #  #   visit new_move_path(as: user)�                1  #   user = create :user, username: "davidpdrsn"5�_�                      $    ����                                                                                                                                                                                                                                                                                                                                      #           V        U�-     �         %    �         %    5�_�                           ����                                                                                                                                                                                                                                                                                                                                      $           V        U�-     �         &      /    user = create :user, username: "davidpdrsn"5�_�                           ����                                                                                                                                                                                                                                                                                                                                      $           V        U�-     �         &      *    = create :user, username: "davidpdrsn"5�_�                           ����                                                                                                                                                                                                                                                                                                                                      $           V        U�-     �         &      )     create :user, username: "davidpdrsn"5�_�                           ����                                                                                                                                                                                                                                                                                                                                      $           V        U�-     �         &      (    create :user, username: "davidpdrsn"5�_�                       !    ����                                                                                                                                                                                                                                                                                                                               !          %       v   %    U�-$     �   
      &      '      fill_in "Username", with: "david"5�_�                       %    ����                                                                                                                                                                                                                                                                                                                               !          %       v   %    U�-%     �   
      &      '      fill_in "Username", with: "bob[]"5�_�                           ����                                                                                                                                                                                                                                                                                                                               !          %       v   %    U�-'     �         &            click_link "@davidpdrsn"5�_�                      %    ����                                                                                                                                                                                                                                                                                                                               !          %       v   %    U�-+    �         &      0      expect(page).to have_content "@davidpdrsn"5�_�                           ����                                                                                                                                                                                                                                                                                                                               !          %       v   %    U�-<    �         &        scenario "notifiers users" do5�_�                           ����                                                                                                                                                                                                                                                                                                                               !          %       v   %    U�-G     �         &      !    create :user, username: "bob"5�_�                       	    ����                                                                                                                                                                                                                                                                                                                               !          %       v   %    U�-I     �         '          �         &    5�_�                           ����                                                                                                                                                                                                                                                                                                                               !          %       v   %    U�-P     �         (          visit notifications_path()5�_�                            ����                                                                                                                                                                                                                                                                                                                                                 V   #    U�-T     �         $       �                (    expect(page).to have_content "Sybil"       within ".credits" do   )      expect(page).to have_content "@bob"       end5�_�                           ����                                                                                                                                                                                                                                                                                                                                                 V   #    U�-T     �         %          e5�_�                           ����                                                                                                                                                                                                                                                                                                                                                 V   #    U�-U     �         %          expect().to 5�_�                            ����                                                                                                                                                                                                                                                                                                                                                 V   #    U�-V     �         %          expect(page).to 5�_�      !                  "    ����                                                                                                                                                                                                                                                                                                                                                 V   #    U�-W     �         %      #    expect(page).to have_content ""5�_�       "           !      %    ����                                                                                                                                                                                                                                                                                                                                                 V   #    U�-\     �         %      '    expect(page).to have_content "@#{}"5�_�   !   #           "      3    ����                                                                                                                                                                                                                                                                                                                                                 V   #    U�-^    �         %      4    expect(page).to have_content "@#{user.username}"5�_�   "   %           #      H    ����                                                                                                                                                                                                                                                                                                                               "          L       v   L    U�-o    �         %      N    expect(page).to have_content "@#{user.username} credited you in his video"5�_�   #       $       %      	    ����       L                                                                                                                                                                                                                                                                                                                      "          L       v   L    U�1     �         %      feature "creating move" do5�_�   #           %   $      I    ����       L                                                                                                                                                                                                                                                                                                                      "          L       v   L    U�-�     �         %      N    expect(page).to have_content "@#{user.username} credited you for his Move"5�_�                       #    ����                                                                                                                                                                                                                                                                                                                               !          %       v   %    U�-)     �         &      /      expect(page).to have_content udavidpdrsn"5�_�                       $    ����                                                                                                                                                                                                                                                                                                                                      #           V        U�-     �         %      (    user = create :user, username: "bob"5��