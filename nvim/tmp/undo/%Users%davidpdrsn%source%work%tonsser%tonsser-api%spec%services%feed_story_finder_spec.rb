Vim�UnDo� ������3�i��'���^~�O-��N�����   �   require "rails_helper"             M   M   M   M   L    U�{g   ( _�                     �       ����                                                                                                                                                                                                                                                                                                                                                             Uph     �   �   �   �          �   �   �   �    5�_�                    �       ����                                                                                                                                                                                                                                                                                                                                                             Upi     �   �   �   �          end�   �   �   �            $2�   �   �   �          it "$1" do�   �   �   �          it5�_�                    �       ����                                                                                                                                                                                                                                                                                                                                                             Upj     �   �   �   �          it "" do5�_�                   �       ����                                                                                                                                                                                                                                                                                                                                                             Upv     �   �   �   �          it "finds stories" do5�_�                    �       ����                                                                                                                                                                                                                                                                                                                                                             Up�     �   �   �   �      8    it "doesn't include stories from other countries" do5�_�      	              �       ����                                                                                                                                                                                                                                                                                                                                                             Up�     �   �   �   �            5�_�      
          	   �   (    ����                                                                                                                                                                                                                                                                                                                                                             Up�     �   �   �   �            �   �   �   �    5�_�   	              
   �   (    ����                                                                                                                                                                                                                                                                                                                                                             Up�     �   �   �   �      )      country = create :country, name: ""5�_�   
                 �   (    ����                                                                                                                                                                                                                                                                                                                                                             Up�     �   �   �   �      8      country = create :country, name: "Denmark", code: 5�_�                    �   3    ����                                                                                                                                                                                                                                                                                                                                                             Up�     �   �   �   �      3      country = create :country, name: "NO", code: 5�_�                   �   (    ����                                                                                                                                                                                                                                                                                                                                                             Up�     �   �   �   �      5      country = create :country, name: "NO", code: ""5�_�                    �   8    ����                                                                                                                                                                                                                                                                                                                                                             Up�    �   �   �   �      9      country = create :country, name: "Norway", code: ""5�_�                    �   .    ����                                                                                                                                                                                                                                                                                                                            �   9       �   /       V   9    Up�    �   �   �   �            �   �   �   �    5�_�                    �   @    ����                                                                                                                                                                                                                                                                                                                            �   9       �   /       V   9    Up�     �   �   �   �            �   �   �   �    5�_�                    �       ����                                                                                                                                                                                                                                                                                                                            �   9       �   /       V   9    Up�     �   �   �   �            �   �   �   �    5�_�                    �        ����                                                                                                                                                                                                                                                                                                                            �   9       �   /       V   9    Up�     �   �   �               �   �   �   �    5�_�                    �   (    ����                                                                                                                                                                                                                                                                                                                            �   9       �   /       V   9    Up�     �   �   �              �   �   �       5�_�                    �       ����                                                                                                                                                                                                                                                                                                                            �   9       �   /       V   9    Up     �   �   �              league = 5�_�                    �       ����                                                                                                                                                                                                                                                                                                                            �   9       �   /       V   9    Up     �   �   �              fc5�_�                    �       ����                                                                                                                                                                                                                                                                                                                            �   9       �   /       V   9    Up     �   �   �               = create :5�_�                    �       ����                                                                                                                                                                                                                                                                                                                            �   9       �   /       V   9    Up    �   �   �              league = create :league5�_�                    �       ����                                                                                                                                                                                                                                                                                                                            �   9       �   /       V   9    Up     �   �   �              �   �   �      5�_�                    �   	    ����                                                                                                                                                                                                                                                                                                                            �   9       �   /       V   9    Up    �   �   �        	      pry5�_�                    �       ����                                                                                                                                                                                                                                                                                                                            �   9       �   /       V   9    Up     �   �   �               require "pry"; binding.pry5�_�                    �       ����                                                                                                                                                                                                                                                                                                                            �   9       �   /       V   9    Up    �   �   �              pp FeedStoryFinder.new()5�_�                    �   A    ����                                                                                                                                                                                                                                                                                                                            �   -       �   -       V   -    Up/     �   �   �        A      story = create :feed_story, triggering_entity_id: region.id5�_�                    �   U    ����                                                                                                                                                                                                                                                                                                                            �   -       �   -       V   -    Up3   
 �   �   �        V      story = create :feed_story, triggering_entity_id: region.id, feed_story_type: ""5�_�                     �       ����                                                                                                                                                                                                                                                                                                                            �   -       �   -       V   -    Up|     �   �   �        .      pp FeedStoryFinder.new(bob).find_stories5�_�      !               �       ����                                                                                                                                                                                                                                                                                                                            �   -       �   -       V   -    Up     �   �   �              �   �   �      5�_�       "           !   �   	    ����                                                                                                                                                                                                                                                                                                                            �   -       �   -       V   -    Up�    �   �   �        	      pry5�_�   !   #           "   �       ����                                                                                                                                                                                                                                                                                                                            �   -       �   -       V   -    Up�    �   �   �      5�_�   "   $           #   �       ����                                                                                                                                                                                                                                                                                                                            �   -       �   -       V   -    Up	     �   �   �      5�_�   #   %           $   �        ����                                                                                                                                                                                                                                                                                                                            �   -       �   -       V   -    Up	     �   �   �           5�_�   $   &           %   �        ����                                                                                                                                                                                                                                                                                                                            �           �           V        Up	F     �   �   �               require "pry"; binding.pry5�_�   %   '           &   �       ����                                                                                                                                                                                                                                                                                                                            �           �           V        Up	F     �   �   �              e5�_�   &   (           '   �       ����                                                                                                                                                                                                                                                                                                                            �           �           V        Up	G     �   �   �              expect().to 5�_�   '   )           (   �       ����                                                                                                                                                                                                                                                                                                                            �           �           V        Up	H     �   �   �              expect(stories).to 5�_�   (   *           )   �       ����                                                                                                                                                                                                                                                                                                                            �           �           V        Up	I    �   �   �              expect(stories).to eq []5�_�   )   +           *   �   /    ����                                                                                                                                                                                                                                                                                                                            �           �           V        Up
�     �   �   �        /      region = create :region, country: country5�_�   *   ,           +   �   8    ����                                                                                                                                                                                                                                                                                                                            �           �           V        Up
�    �   �   �        9      region = create :region, country: country, name: ""5�_�   +   -           ,           ����                                                                                                                                                                                                                                                                                                                            �           �           V        Up    �                   �           5�_�   ,   .           -          ����                                                                                                                                                                                                                                                                                                                            �           �           V        Up*     �                         pp stories.first5�_�   -   /           .   �        ����                                                                                                                                                                                                                                                                                                                            �           �           V        Up+    �   �   �      �   �   �      5�_�   .   0           /   �        ����                                                                                                                                                                                                                                                                                                                            �           �           V        Up7    �   �   �                    pp stories.first5�_�   /   1           0   k        ����                                                                                                                                                                                                                                                                                                                            �          �          V       UpB    �   k   m                �   k   m      5�_�   0   2           1   l       ����                                                                                                                                                                                                                                                                                                                            �          �          V       UpN    �   k   l                  pp stories5�_�   1   3           2   �       ����                                                                                                                                                                                                                                                                                                                                                             UuAE     �   �   �              �   �   �      5�_�   2   4           3   �       ����                                                                                                                                                                                                                                                                                                                                                             UuAG     �   �   �  	      	      end�   �   �  	      
        $2�   �   �  	            it "$1" do�   �   �              it5�_�   3   5           4   �   
    ����                                                                                                                                                                                                                                                                                                                                                             UuAH     �   �   �  	            it "" do5�_�   4   6           5   �       ����                                                                                                                                                                                                                                                                                                                                                             UuAJ     �   �   �  	              5�_�   5   7           6   �   "    ����                                                                                                                                                                                                                                                                                                                                                             UuAO     �   �   �  
              �   �   �  	    5�_�   6   8           7   �       ����                                                                                                                                                                                                                                                                                                                                                             UuAV     �   �   �      �   �   �      5�_�   7   9           8   �       ����                                                                                                                                                                                                                                                                                                                                                             UuAX     �   �   �                pp mvp_team.region5�_�   8   :           9   �       ����                                                                                                                                                                                                                                                                                                                                                             UuAZ     �   �   �                pp mvp_team.region5�_�   9   ;           :   �       ����                                                                                                                                                                                                                                                                                                                                                             UuA_     �   �   �                �   �   �      5�_�   :   <           ;   �       ����                                                                                                                                                                                                                                                                                                                                                             UuAb     �   �   �                mvp_team.update()5�_�   ;   =           <   �   ;    ����                                                                                                                                                                                                                                                                                                                                                             UuAo     �   �   �        ;        mvp_team.update group: :region, group_id: region.od5�_�   <   >           =   �   "    ����                                                                                                                                                                                                                                                                                                                                                             UuAp     �   �   �                �   �   �      5�_�   =   ?           >   �   
    ����                                                                                                                                                                                                                                                                                                                                                             UuAs     �   �   �        
        fc5�_�   >   @           ?   �       ����                                                                                                                                                                                                                                                                                                                                                             UuAs   ! �   �   �                 = create :5�_�   ?   A           @   �       ����                                                                                                                                                                                                                                                                                                                                                             UuA�     �   �   �                �   �   �      5�_�   @   B           A   �   
    ����                                                                                                                                                                                                                                                                                                                                                             UuA�     �   �   �        
        fc5�_�   A   C           B   �       ����                                                                                                                                                                                                                                                                                                                                                             UuA�     �   �   �                 = create :5�_�   B   D           C   �       ����                                                                                                                                                                                                                                                                                                                                                             UuA�     �   �   �                league = create :league5�_�   C   E           D   �   .    ����                                                                                                                                                                                                                                                                                                                                                             UuA�     �   �   �                �   �   �      5�_�   D   F           E   �   
    ����                                                                                                                                                                                                                                                                                                                                                             UuA�     �   �   �        
        fc5�_�   E   G           F   �       ����                                                                                                                                                                                                                                                                                                                                                             UuA�     �   �   �                 = create :5�_�   F   H           G   �       ����                                                                                                                                                                                                                                                                                                                                                             UuA�     �   �   �                �   �   �      5�_�   G   I           H   �       ����                                                                                                                                                                                                                                                                                                                                                             UuA�     �   �   �                bob5�_�   H   J           I   �       ����                                                                                                                                                                                                                                                                                                                                                             UuA�   " �   �   �                �   �   �      5�_�   I   K           J   �        ����                                                                                                                                                                                                                                                                                                                            �   	       �           V       UuA�   $ �   �   �                    it "foo" do           region = create :region   /        league = create :league, region: region           team = create :team   #        mvp_team = create :mvp_team   ;        mvp_team.update group: :region, group_id: region.id           bob = create :user           bob.teams << team               pp mvp_team.group           pp mvp_team.find_group   	      end5�_�   J   L           K   Y        ����                                                                                                                                                                                                                                                                                                                            Y          n           V       UuA�     �   X   Y          J      it "is included if the user plays in the same league as the TOTW" do   )        bob_mvp = create :mvp_team_player           team = create :team           league = team.league   "        bob_mvp.user.teams << team               alice = create :user   5        alice.teams = [create(:team, league: league)]               mvp_team = create(             :mvp_team,   &          mvp_team_players: [bob_mvp],             group_id: league.id   	        )       8        FeedStoryCreator.create_relevant_story(mvp_team)       9        stories = FeedStoryFinder.new(alice).find_stories       A        expect(stories.map(&:feed_story_type)).to eq ["mvp-team"]   	      end    5�_�   K   M           L   W        ����                                                                                                                                                                                                                                                                                                                            W           n          V       UuA�   ( �   V   W                  describe "TOTW type" do   D      it "is not included if the user doesn't play in the league" do   )        bob_mvp = create :mvp_team_player           team = create :team           league = team.league   "        bob_mvp.user.teams << team               alice = create :user   %        alice.teams = [create(:team)]               mvp_team = create(             :mvp_team,   &          mvp_team_players: [bob_mvp],             group_id: league.id   	        )       8        FeedStoryCreator.create_relevant_story(mvp_team)       9        stories = FeedStoryFinder.new(alice).find_stories       7        expect(stories.map(&:feed_story_type)).to eq []   	      end       end5�_�   L               M           ����                                                                                                                                                                                                                                                                                                                                                             U�{g     �          �      jequire "rails_helper"5�_�                    �   3    ����                                                                                                                                                                                                                                                                                                                                                             Up�     �   �   �        �   �   �   �       5�_�              	      �   (    ����                                                                                                                                                                                                                                                                                                                                                             Up�     �   �   �   �      (      region = create :region, country: 5�_�                    �       ����                                                                                                                                                                                                                                                                                                                                                             Upt     �   �   �   �          it "finds storiec"s" do5��