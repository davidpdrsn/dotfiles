Vim�UnDo� .	�^oW���-o��W�w���㉉/��Q x          pp json            P       P   P   P    UT�    _�                             ����                                                                                                                                                                                                                                                                                                                                                             UT�g     �                   5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             UT�h    �               &describe `!p snip.rv = spec_name()` do�                   $0�                 it "$2" do�                  spec5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             UT�j     �               
  it "" do5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             UT�n     �                   5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             UT�o     �                   e5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             UT�q     �                   expect().to 5�_�                       !    ����                                                                                                                                                                                                                                                                                                                                                             UT�t     �               !    expect(MvpTeamSerializer).to 5�_�      	                 +    ����                                                                                                                                                                                                                                                                                                                                                             UT�v     �               ,    expect(MvpTeamSerializer).to serialize()5�_�      
           	      6    ����                                                                                                                                                                                                                                                                                                                                                             UT�y     �               7    expect(MvpTeamSerializer).to serialize_attributes()5�_�   	              
          ����                                                                                                                                                                                                                                                                                                                                                             UT�z    �         	            �             5�_�   
                    
    ����                                                                                                                                                                                                                                                                                                                                                             UT��     �         	    �         	    5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             UT��     �                      :slug5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             UT��     �         	      Q  attributes :slug, :period, :agegroup, :group, :mvp_slug, :start_date, :end_date5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             UT��     �                F  :slug, :period, :agegroup, :group, :mvp_slug, :start_date, :end_date5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             UT��     �         	      J      :slug, :period, :agegroup, :group, :mvp_slug, :start_date, :end_date5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             UT��     �      	   
      C      :period, :agegroup, :group, :mvp_slug, :start_date, :end_date5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             UT��     �      
         :      :agegroup, :group, :mvp_slug, :start_date, :end_date5�_�                    	       ����                                                                                                                                                                                                                                                                                                                                                             UT��     �               /      :group, :mvp_slug, :start_date, :end_date5�_�                    
       ����                                                                                                                                                                                                                                                                                                                                                             UT��     �   	            '      :mvp_slug, :start_date, :end_date5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             UT��     �   
                  :start_date, :end_date5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             UT��    �                     :end_date5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             UT��     �             �             5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             UT��   
 �                     :end_date,5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             UT��     �                 �             5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             UT�      �                 end�                   $2�                 it "$1" do�                 it5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             UT�    �               
  it "" do5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             UT�     �      $       �             5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             UT�     �                    5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             UT�     �         $      @    let(:norway) { create :country, name: "Norway", code: "NO" }5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             UT�     �         $      =    (:norway) { create :country, name: "Norway", code: "NO" }5�_�                            ����                                                                                                                                                                                                                                                                                                                                                             UT�     �         $      <    :norway) { create :country, name: "Norway", code: "NO" }5�_�      !                  
    ����                                                                                                                                                                                                                                                                                                                                                             UT�     �         $      ;    norway) { create :country, name: "Norway", code: "NO" }5�_�       "           !          ����                                                                                                                                                                                                                                                                                                                                                             UT�     �         $      =    norway =) { create :country, name: "Norway", code: "NO" }5�_�   !   #           "          ����                                                                                                                                                                                                                                                                                                                                                             UT�     �         $      <    norway = { create :country, name: "Norway", code: "NO" }�         $    5�_�   "   $           #          ����                                                                                                                                                                                                                                                                                                                                                             UT�     �                <    let(:norway_region) { create(:region, country: norway) }5�_�   #   %           $           ����                                                                                                                                                                                                                                                                                                                                                  V        UT�     �                6    it "returns the country when group is a league" do   5      league = create(:league, region: norway_region)   ,      team = build :mvp_team, group: :league   &      team.update(group_id: league.id)       '      expect(team.country).to eq norway       end    5�_�   $   &           %          ����                                                                                                                                                                                                                                                                                                                                                  V        UT�     �                6    it "returns the country when group is a region" do5�_�   %   '           &          ����                                                                                                                                                                                                                                                                                                                                                  V        UT�     �                    end5�_�   &   (           '           ����                                                                                                                                                                                                                                                                                                                                                V        UT�    �                '      expect(team.country).to eq norway�                -      team.update(group_id: norway_region.id)�                ,      team = build :mvp_team, group: :region5�_�   '   )           (          ����                                                                                                                                                                                                                                                                                                                                              V       UT�    �               *    team = build :mvp_team, group: :region5�_�   (   *           )          ����                                                                                                                                                                                                                                                                                                                                              V       UT�/     �             �             5�_�   )   +           *          ����                                                                                                                                                                                                                                                                                                                                              V       UT�/     �               <    let(:norway_region) { create(:region, country: norway) }5�_�   *   ,           +          ����                                                                                                                                                                                                                                                                                                                                              V       UT�0     �               9    (:norway_region) { create(:region, country: norway) }5�_�   +   -           ,          ����                                                                                                                                                                                                                                                                                                                                              V       UT�0     �               8    :norway_region) { create(:region, country: norway) }5�_�   ,   .           -          ����                                                                                                                                                                                                                                                                                                                                              V       UT�1     �               7    norway_region) { create(:region, country: norway) }5�_�   -   /           .          ����                                                                                                                                                                                                                                                                                                                                              V       UT�2     �               9    norway_region =) { create(:region, country: norway) }5�_�   .   0           /          ����                                                                                                                                                                                                                                                                                                                                              V       UT�3    �               8    norway_region = { create(:region, country: norway) }�             5�_�   /   1           0          ����                                                                                                                                                                                                                                                                                                                                              V       UT��     �      +       �             5�_�   0   2           1          ����                                                                                                                                                                                                                                                                                                                                              V       UT��     �             	   *  it "includes the name of the country" do   8    norway = create :country, name: "Norway", code: "NO"   4    norway_region = create(:region, country: norway)       +    team = create :mvp_team, group: :region   +    team.update(group_id: norway_region.id)       %    expect(team.country).to eq norway     end5�_�   1   3           2           ����                                                                                                                                                                                                                                                                                                                                              V       UT��     �       "              end�      !          '      expect(team.country).to eq norway�                -      team.update(group_id: norway_region.id)�                ,      team = build :mvp_team, group: :region�                6    it "returns the country when group is a region" do�                    end�                '      expect(team.country).to eq norway�                &      team.update(group_id: league.id)�                ,      team = build :mvp_team, group: :league�                5      league = create(:league, region: norway_region)�                6    it "returns the country when group is a league" do�                <    let(:norway_region) { create(:region, country: norway) }�                @    let(:norway) { create :country, name: "Norway", code: "NO" }5�_�   2   4           3           ����                                                                                                                                                                                                                                                                                                                             2                 V   2    UT��     �                4  it "returns the country when group is a league" do   3    league = create(:league, region: norway_region)   *    team = build :mvp_team, group: :league   $    team.update(group_id: league.id)       %    expect(team.country).to eq norway     end5�_�   3   5           4           ����                                                                                                                                                                                                                                                                                                                             2                 V   2    UT��    �                 5�_�   4   6           5           ����                                                                                                                                                                                                                                                                                                                             2                 V   2    UT��     �                >  let(:norway) { create :country, name: "Norway", code: "NO" }   :  let(:norway_region) { create(:region, country: norway) }5�_�   5   7           6           ����                                                                                                                                                                                                                                                                                                                              2                 V   2    UT��     �             �             5�_�   6   8           7           ����                                                                                                                                                                                                                                                                                                                              2                 V   2    UT��     �                 5�_�   7   9           8           ����                                                                                                                                                                                                                                                                                                                                               V       UT��     �               >  let(:norway) { create :country, name: "Norway", code: "NO" }   :  let(:norway_region) { create(:region, country: norway) }5�_�   8   :           9          ����                                                                                                                                                                                                                                                                                                                                               V       UT��     �               @    let(:norway) { create :country, name: "Norway", code: "NO" }5�_�   9   ;           :          ����                                                                                                                                                                                                                                                                                                                                               V       UT��     �               =    (:norway) { create :country, name: "Norway", code: "NO" }5�_�   :   <           ;          ����                                                                                                                                                                                                                                                                                                                                               V       UT��     �               <    :norway) { create :country, name: "Norway", code: "NO" }5�_�   ;   =           <          ����                                                                                                                                                                                                                                                                                                                                               V       UT��     �               >    :norway =) { create :country, name: "Norway", code: "NO" }5�_�   <   >           =          ����                                                                                                                                                                                                                                                                                                                                               V       UT��     �               =    :norway = { create :country, name: "Norway", code: "NO" }5�_�   =   ?           >          ����                                                                                                                                                                                                                                                                                                                                               V       UT��     �               <    :norway ={ create :country, name: "Norway", code: "NO" }5�_�   >   @           ?      ;    ����                                                                                                                                                                                                                                                                                                                                               V       UT��     �               ;    :norway = create :country, name: "Norway", code: "NO" }5�_�   ?   A           @          ����                                                                                                                                                                                                                                                                                                                                               V       UT��     �               <    let(:norway_region) { create(:region, country: norway) }5�_�   @   B           A          ����                                                                                                                                                                                                                                                                                                                                                      UT��    �               9    :norway = create :country, name: "Norway", code: "NO"   5    :norway_region = create(:region, country: norway)5�_�   A   C           B           ����                                                                                                                                                                                                                                                                                                                                               V       UT��     �                   �             5�_�   B   D           C          ����                                                                                                                                                                                                                                                                                                                                               V       UT��     �                   MvpTeamSerializer.new()5�_�   C   E           D          ����                                                                                                                                                                                                                                                                                                                                               V       UT��     �                   MvpTeamSerializer.new(team)5�_�   D   F           E          ����                                                                                                                                                                                                                                                                                                                                               V       UT��     �               #    MvpTeamSerializer.new(team).jas5�_�   E   G           F      0    ����                                                                                                                                                                                                                                                                                                                                               V       UT��     �               0    MvpTeamSerializer.new(team, root: false).jas5�_�   F   H           G          ����                                                                                                                                                                                                                                                                                                                                               V       UT��     �               4    MvpTeamSerializer.new(team, root: false).as_json5�_�   G   I           H      
    ����                                                                                                                                                                                                                                                                                                                                               V       UT�      �                   �             5�_�   H   J           I          ����                                                                                                                                                                                                                                                                                                                                               V       UT�    �                %    expect(team.country).to eq norway5�_�   I   K           J          ����                                                                                                                                                                                                                                                                                                                                               V       UT�     �                   pp json5�_�   J   L           K          ����                                                                                                                                                                                                                                                                                                                                               V       UT�     �                   e5�_�   K   M           L          ����                                                                                                                                                                                                                                                                                                                                               V       UT�     �                   expect().to 5�_�   L   N           M          ����                                                                                                                                                                                                                                                                                                                                               V       UT�     �                   expect(json[]).to 5�_�   M   O           N          ����                                                                                                                                                                                                                                                                                                                                               V       UT�     �                   expect(json[:country]).to 5�_�   N   P           O      "    ����                                                                                                                                                                                                                                                                                                                                               V       UT�    �               #    expect(json[:country]).to eq ""5�_�   O               P      "    ����                                                                                                                                                                                                                                                                                                                                               V       UT�    �               *    expect(json[:country]).to eq "Denmark"5��