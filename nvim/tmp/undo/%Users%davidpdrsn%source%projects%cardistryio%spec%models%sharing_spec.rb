Vim�UnDo� ��k*�Ρ/��>��YȾ����[�!(                    7       7   7   7    U��    _�                             ����                                                                                                                                                                                                                                                                                                                                                             U��     �                   5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             U��     �               &describe `!p snip.rv = spec_name()` do�                   $0�                 it "$2" do�                  spec5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             U��     �               
  it "" do5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             U��     �                    5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             U��     �               
  it "" do�                  end5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             U��     �             �             5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             U��     �                 it { should belong_to :user }5�_�      	                     ����                                                                                                                                                                                                                                                                                                                                                             U��     �             �             5�_�      
           	          ����                                                                                                                                                                                                                                                                                                                                                             U��     �             5�_�   	              
          ����                                                                                                                                                                                                                                                                                                                                                             U��     �                  it { should belong_to :video }5�_�   
                    $    ����                                                                                                                                                                                                                                                                                                                                                             U��     �               +  it { should validate_presence_of :video }5�_�                       +    ����                                                                                                                                                                                                                                                                                                                                                             U��     �      	       �             5�_�                       $    ����                                                                                                                                                                                                                                                                                                                                                             U��    �      	   	      .  it { should validate_presence_of :video_id }5�_�                       *    ����                                                                                                                                                                                                                                                                                                                                                             U��     �         
        �      
   	    5�_�                    
       ����                                                                                                                                                                                                                                                                                                                                                             U��     �                 end�   
                $2�   	              describe "$1" do�   	              des5�_�                    
       ����                                                                                                                                                                                                                                                                                                                                                             U��     �   	              describe "" do5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             U��     �   
                5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             U��     �                   end�                     $2�   
                it "$1" do�   
                it5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             U��    �   
                it "" do5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             U��     �                     5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             U��     �                     �             5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             U��     �                     video = create 5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             U��     �             5�_�                            ����                                                                                                                                                                                                                                                                                                                                                             U��     �                 5�_�                            ����                                                                                                                                                                                                                                                                                                                                                             U��     �                   �             5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             U��     �                   end�                     $2�                   it "$1" do�                   it5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             U��     �                   it "" do5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             U��     �                      5�_�                            ����                                                                                                                                                                                                                                                                                                                                                 V       U��     �                    &    it "excludes unapproved videos" do       end5�_�                       *    ����                                                                                                                                                                                                                                                                                                                                                 V       U��    �               :      video = create :video, private: true, approved: true5�_�                        )    ����                                                                                                                                                                                                                                                                                                                                         )       V   )    U��     �             �             5�_�      !                      ����                                                                                                                                                                                                                                                                                                                                         )       V   )    U��     �               *      video = create :video, private: true5�_�       "           !          ����                                                                                                                                                                                                                                                                                                                                         )       V   )    U��     �               $      = create :video, private: true5�_�   !   #           "          ����                                                                                                                                                                                                                                                                                                                                         )       V   )    U��     �               #       create :video, private: true5�_�   "   $           #          ����                                                                                                                                                                                                                                                                                                                                                V       U��     �                     Sharing.create!()�                     �             5�_�   #   %           $      ,    ����                                                                                                                                                                                                                                                                                                                                                V       U�     �                     �             5�_�   $   &           %          ����                                                                                                                                                                                                                                                                                                                                                V       U�     �                     e5�_�   %   '           &          ����                                                                                                                                                                                                                                                                                                                                                V       U�     �                     expect().to 5�_�   &   (           '      6    ����                                                                                                                                                                                                                                                                                                                                                V       U�    �               6      expect(Sharing.videos_shared_with_user(bob)).to 5�_�   '   )           (      1    ����                                                                                                                                                                                                                                                                                                                                                V       U�!     �               @      expect(Sharing.videos_shared_with_user(bob)).to eq [video]5�_�   (   *           )      I    ����                                                                                                                                                                                                                                                                                                                                                V       U�(   	 �               J      expect(Sharing.videos_shared_with_user(bob).map(&:id)).to eq [video]5�_�   )   +           *      8    ����                                                                                                                                                                                                                                                                                                                                                V       U�+     �               M      expect(Sharing.videos_shared_with_user(bob).map(&:id)).to eq [video.id]5�_�   *   ,           +      L    ����                                                                                                                                                                                                                                                                                                                                                V       U�-     �               O      expect(Sharing.videos_shared_with_user(bob).map(&:name)).to eq [video.id]5�_�   +   -           ,      *    ����                                                                                                                                                                                                                                                                                                                                                V       U�/     �               *      video = create :video, private: true5�_�   ,   .           -      "    ����                                                                                                                                                                                                                                                                                                                                                V       U�3   
 �               "      create :video, private: true5�_�   -   /           .          ����                                                                                                                                                                                                                                                                                                                               0                 v       U�}     �               Q      expect(Sharing.videos_shared_with_user(bob).map(&:name)).to eq [video.name]5�_�   .   0           /          ����                                                                                                                                                                                                                                                                                                                               0                 v       U��     �                     �             5�_�   /   1           0          ����                                                                                                                                                                                                                                                                                                                               0                 v       U��     �                     shared_videos = �             5�_�   0   2           1          ����                                                                                                                                                                                                                                                                                                                               0                 v       U��    �               9      shared_videos = haring.videos_shared_with_user(bob)5�_�   1   3           2           ����                                                                                                                                                                                                                                                                                                                               0                 v       U��     �                 �             5�_�   2   4           3          ����                                                                                                                                                                                                                                                                                                                               0                 v       U��     �                 end�                   $2�                 describe "$1" do�                 des5�_�   3   5           4          ����                                                                                                                                                                                                                                                                                                                               0                 v       U��     �                 describe "" do5�_�   4   6           5          ����                                                                                                                                                                                                                                                                                                                               0                 v       U��     �                    5�_�   5   7           6           ����                                                                                                                                                                                                                                                                                                                                                 V       U��     �                      describe "" do5�_�   6               7          ����                                                                                                                                                                                                                                                                                                                                                 V       U��    �                  end5��