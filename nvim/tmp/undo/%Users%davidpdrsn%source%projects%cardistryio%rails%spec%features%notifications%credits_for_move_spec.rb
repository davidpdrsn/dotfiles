Vim�UnDo� d޶�pmq禉��<	���0h7��Y񻍌L�                    7       7   7   7    U�4    _�                             ����                                                                                                                                                                                                                                                                                                                                                             U�,�     �                   �               5�_�                            ����                                                                                                                                                                                                                                                                                                                                                             U�,�     �                 �              5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             U�,�    �                   �               5�_�                           ����                                                                                                                                                                                                                                                                                                                               $          -       v   -    U�,�     �               %  scenario "creates the move", :js do5�_�                           ����                                                                                                                                                                                                                                                                                                                               $          -       v   -    U�-     �             5�_�                           ����                                                                                                                                                                                                                                                                                                                               $          -       v   -    U�-     �                  end�                    end�                0      expect(page).to have_content "@davidpdrsn"�                    within ".credits" do�                (    expect(page).to have_content "Sybil"�                 �                    click_button "Add move"�                    end�                      click_link "@davidpdrsn"�   
             '      fill_in "Username", with: "david"�   	                 within ".add-credits" do�      
          -    fill_in "Description", with: "Old school"�      	          !    fill_in "Name", with: "Sybil"�                !    visit new_move_path(as: user)�                /    user = create :user, username: "davidpdrsn"�                E  scenario "doesn't notifier users if they credit themselves", :js do5�_�                           ����                                                                                                                                                                                                                                                                                                                               $          -       v   -    U�-     �                 �             5�_�      	                     ����                                                                                                                                                                                                                                                                                                                               $          -       v   -    U�-     �                 end�                   $2�                 scenario "$1" do�                 sce5�_�      
           	          ����                                                                                                                                                                                                                                                                                                                            	   $       	   -       v   -    U�-     �                 scenario "" do5�_�   	              
          ����                                                                                                                                                                                                                                                                                                                            	   $       	   -       v   -    U�-     �                    5�_�   
                         ����                                                                                                                                                                                                                                                                                                                                                 V        U�-     �             �             5�_�                           ����                                                                                                                                                                                                                                                                                                                                      #           V        U�-     �                	  #   end�                2  #     expect(page).to have_content "@davidpdrsn"�                  #   within ".credits" do�                *  #   expect(page).to have_content "Sybil"�                 �                  #   click_button "Add move"�                	  #   end�   
                #     click_link "@davidpdrsn"�   	             )  #     fill_in "Username", with: "david"�      
            #   within ".add-credits" do�      	          /  #   fill_in "Description", with: "Old school"�                #  #   fill_in "Name", with: "Sybil"�                #  #   visit new_move_path(as: user)�                1  #   user = create :user, username: "davidpdrsn"5�_�                      $    ����                                                                                                                                                                                                                                                                                                                                      #           V        U�-     �         %    �         %    5�_�                           ����                                                                                                                                                                                                                                                                                                                                      $           V        U�-     �         &      /    user = create :user, username: "davidpdrsn"5�_�                           ����                                                                                                                                                                                                                                                                                                                                      $           V        U�-     �         &      *    = create :user, username: "davidpdrsn"5�_�                           ����                                                                                                                                                                                                                                                                                                                                      $           V        U�-     �         &      )     create :user, username: "davidpdrsn"5�_�                           ����                                                                                                                                                                                                                                                                                                                                      $           V        U�-     �         &      (    create :user, username: "davidpdrsn"5�_�                       !    ����                                                                                                                                                                                                                                                                                                                               !          %       v   %    U�-$     �   
      &      '      fill_in "Username", with: "david"5�_�                       %    ����                                                                                                                                                                                                                                                                                                                               !          %       v   %    U�-%     �   
      &      '      fill_in "Username", with: "bob[]"5�_�                           ����                                                                                                                                                                                                                                                                                                                               !          %       v   %    U�-'     �         &            click_link "@davidpdrsn"5�_�                      %    ����                                                                                                                                                                                                                                                                                                                               !          %       v   %    U�-+    �         &      0      expect(page).to have_content "@davidpdrsn"5�_�                           ����                                                                                                                                                                                                                                                                                                                               !          %       v   %    U�-<    �         &        scenario "notifiers users" do5�_�                           ����                                                                                                                                                                                                                                                                                                                               !          %       v   %    U�-G     �         &      !    create :user, username: "bob"5�_�                       	    ����                                                                                                                                                                                                                                                                                                                               !          %       v   %    U�-I     �         '          �         &    5�_�                           ����                                                                                                                                                                                                                                                                                                                               !          %       v   %    U�-P     �         (          visit notifications_path()5�_�                            ����                                                                                                                                                                                                                                                                                                                                                 V   #    U�-T     �         $       �                (    expect(page).to have_content "Sybil"       within ".credits" do   )      expect(page).to have_content "@bob"       end5�_�                           ����                                                                                                                                                                                                                                                                                                                                                 V   #    U�-T     �         %          e5�_�                           ����                                                                                                                                                                                                                                                                                                                                                 V   #    U�-U     �         %          expect().to 5�_�                            ����                                                                                                                                                                                                                                                                                                                                                 V   #    U�-V     �         %          expect(page).to 5�_�      !                  "    ����                                                                                                                                                                                                                                                                                                                                                 V   #    U�-W     �         %      #    expect(page).to have_content ""5�_�       "           !      %    ����                                                                                                                                                                                                                                                                                                                                                 V   #    U�-\     �         %      '    expect(page).to have_content "@#{}"5�_�   !   #           "      3    ����                                                                                                                                                                                                                                                                                                                                                 V   #    U�-^    �         %      4    expect(page).to have_content "@#{user.username}"5�_�   "   %           #      H    ����                                                                                                                                                                                                                                                                                                                               "          L       v   L    U�-o    �         %      N    expect(page).to have_content "@#{user.username} credited you in his video"5�_�   #   &   $       %      	    ����       L                                                                                                                                                                                                                                                                                                                      "          L       v   L    U�1     �         %      feature "creating move" do5�_�   %   '           &           ����       L                                                                                                                                                                                                                                                                                                                   
                      V        U�1�     �         &          �         %    5�_�   &   (           '          ����       L                                                                                                                                                                                                                                                                                                                   
                      V        U�1�     �                    #5�_�   '   )           (          ����       L                                                                                                                                                                                                                                                                                                                   
                      V        U�1�     �                G  # scenario "doesn't notifier users if they credit themselves", :js do5�_�   (   *           )      E    ����       L                                                                                                                                                                                                                                                                                                                   
                      V        U�1�     �         %      E  scenario "doesn't notifier users if they credit themselves", :js do5�_�   )   +           *          ����       L                                                                                                                                                                                                                                                                                                                   
                      V        U�1�     �         %    �         %    5�_�   *   ,           +          ����       L                                                                                                                                                                                                                                                                                                                   
                      V        U�1�    �         &    5�_�   +   -           ,          ����       L                                                                                                                                                                                                                                                                                                                   
                      V        U�1�    �         '      =  scenario "doesn't notifier users if they credit themselves"5�_�   ,   .           -           ����       L                                                                                                                                                                                                                                                                                                                                         V        U�3�     �                    %  scenario "and that video is a link"5�_�   -   /           .           ����       L                                                                                                                                                                                                                                                                                                                                         V        U�3�     �         &          �         %    5�_�   .   0           /          ����       L                                                                                                                                                                                                                                                                                                                                         V        U�3�     �         &          click_link ""5�_�   /   1           0          ����       L                                                                                                                                                                                                                                                                                                                                         V        U�3�     �         &    5�_�   0   2           1          ����       L                                                                                                                                                                                                                                                                                                                                         V        U�3�     �         (          �         '    5�_�   1   3           2          ����       L                                                                                                                                                                                                                                                                                                                                         V        U�3�     �         (          e5�_�   2   4           3          ����       L                                                                                                                                                                                                                                                                                                                                         V        U�3�     �         (          expect().to 5�_�   3   5           4          ����       L                                                                                                                                                                                                                                                                                                                                         V        U�3�     �         (          expect(page).to 5�_�   4   6           5      "    ����       L                                                                                                                                                                                                                                                                                                                                         V        U�4    �         (      #    expect(page).to have_content ""5�_�   5   7           6      &    ����       L                                                                                                                                                                                                                                                                                                                                         V        U�4
    �   &   (            # end�   %   '          	  #   end�   $   &          2  #     expect(page).to have_content "@davidpdrsn"�   #   %            #   within ".credits" do�   "   $          *  #   expect(page).to have_content "Sybil"�   !   #           �       "            #   click_button "Add move"�      !          	  #   end�                    #     click_link "@davidpdrsn"�                )  #     fill_in "Username", with: "david"�                  #   within ".add-credits" do�                /  #   fill_in "Description", with: "Old school"�                #  #   fill_in "Name", with: "Sybil"�                #  #   visit new_move_path(as: user)�                1  #   user = create :user, username: "davidpdrsn"5�_�   6               7           ����       L                                                                                                                                                                                                                                                                                                                      &       !           V        U�4    �                    =  scenario "doesn't notifier users if they credit themselves"   /    user = create :user, username: "davidpdrsn"   !    visit new_move_path(as: user)   !    fill_in "Name", with: "Sybil"   -    fill_in "Description", with: "Old school"       within ".add-credits" do   '      fill_in "Username", with: "david"         click_link "@davidpdrsn"       end       click_button "Add move"       (    expect(page).to have_content "Sybil"       within ".credits" do   0      expect(page).to have_content "@davidpdrsn"       end     end5�_�   #           %   $      I    ����       L                                                                                                                                                                                                                                                                                                                      "          L       v   L    U�-�     �         %      N    expect(page).to have_content "@#{user.username} credited you for his Move"5�_�                       #    ����                                                                                                                                                                                                                                                                                                                               !          %       v   %    U�-)     �         &      /      expect(page).to have_content udavidpdrsn"5�_�                       $    ����                                                                                                                                                                                                                                                                                                                                      #           V        U�-     �         %      (    user = create :user, username: "bob"5��