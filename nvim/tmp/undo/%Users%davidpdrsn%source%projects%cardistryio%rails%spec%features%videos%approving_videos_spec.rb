Vim�UnDo� �s�1Ė2�F�4R�� ǡ#�=7���4��q   &       click_link "Videos"                             U�:o    _�                     %        ����                                                                                                                                                                                                                                                                                                                                                             U�g)     �   %   (   '        �   %   '   &    5�_�                    '       ����                                                                                                                                                                                                                                                                                                                                                             U�g*     �   (   *   *        end�   '   )   *          $2�   &   (   *        scenario "$1" do�   &   *   (        sce5�_�                    '       ����                                                                                                                                                                                                                                                                                                                                                             U�g*     �   &   (   *        scenario "" do5�_�                    (        ����                                                                                                                                                                                                                                                                                                                                                  V        U�g2     �   (   6   *    �   (   )   *    5�_�                    (       ����                                                                                                                                                                                                                                                                                                                                                  V        U�g3     �   '   (              5�_�                    ,        ����                                                                                                                                                                                                                                                                                                                            ,          .          V       U�gB     �   +   ,              click_link "All videos"       /    expect(page).not_to have_content video.name5�_�                    /        ����                                                                                                                                                                                                                                                                                                                            ,          ,          V       U�gF     �   .   /              click_link "All videos"5�_�      	              /        ����                                                                                                                                                                                                                                                                                                                            ,          ,          V       U�gF     �   /   1   3          �   /   1   2    5�_�      
           	   0   !    ����                                                                                                                                                                                                                                                                                                                            ,          ,          V       U�gM     �   0   2   3    5�_�   	              
   2       ����                                                                                                                                                                                                                                                                                                                            ,          ,          V       U�gN     �   1   3   4      +    expect(page).to have_content video.name5�_�   
                 2       ����                                                                                                                                                                                                                                                                                                                            ,          ,          V       U�gN     �   1   3   4          e5�_�                    2       ����                                                                                                                                                                                                                                                                                                                            ,          ,          V       U�gO     �   1   3   4          expect().to 5�_�                    2       ����                                                                                                                                                                                                                                                                                                                            ,          ,          V       U�gQ    �   1   3   4          expect(page).to 5�_�                    '        ����                                                                                                                                                                                                                                                                                                                            '   *       3          V   *    U���    �   &   '          ,  scenario "the user gets a notification" do   %    admin = create :user, admin: true   *    video = create :video, approved: false           visit root_path(as: admin)           click_link "Approve videos"       click_button "Approve"       #    visit root_path(as: video.user)       4    expect(page).to have_content "Notifications (1)"     end5�_�                    &        ����                                                                                                                                                                                                                                                                                                                            '   *       '          V   *    U���    �   %   &           5�_�                    	       ����                                                                                                                                                                                                                                                                                                                                                             U�6V     �      
   &          click_link "All videos"5�_�                    	       ����                                                                                                                                                    	                                                                                                                                                                                                       U�6V    �      
   &          click_link "videos"5�_�                           ����                                                                                                                                                    	                                                                                                                                                                                                       U�6^     �         &          click_link "All videos"5�_�                           ����                                                                                                                                                    	                                                                                                                                                                                                       U�6^    �         &          click_link "videos"5�_�                    	       ����                                                                                                                                                    	                                                                                                                                                                                                       U�:i     �      
   &          click_link "Videos"5�_�                            ����                                                                                                                                                    	                                                                                                                                                                                                       U�:n    �         &          click_link "Videos"5��