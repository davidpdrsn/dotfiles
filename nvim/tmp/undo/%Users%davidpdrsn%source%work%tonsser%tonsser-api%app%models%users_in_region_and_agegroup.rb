Vim�UnDo� �������|�_�A�ϞoYƏ6�P�;�      module UsersInRegionAndAgegroup                              V/�c    _�                             ����                                                                                                                                                                                                                                                                                                                                                  V        V.&D     �         
            �         	    5�_�                           ����                                                                                                                                                                                                                                                                                                                                                  V        V.&I     �         
            UsersInAgegroup.find()5�_�                           ����                                                                                                                                                                                                                                                                                                                                                  V        V.&N     �                
      User5�_�                           ����                                                                                                                                                                                                                                                                                                                                                  V        V.&P     �         	      4      UsersInAgegroup.find(agegroup_id: agegroup_id)5�_�                       .    ����                                                                                                                                                                                                                                                                                                                               .          G       v   G    V.&U    �         
      K        .where(leagues: { region_id: region_id, agegroup_id: agegroup_id })5�_�                           ����                                                                                                                                                                                                                                                                                                                               .          G       v   G    V.&[    �                3        .joins(team_memberships: { team: :league })5�_�                            ����                                                                                                                                                                                                                                                                                                                                               V       V/<"     �         
          �         	    5�_�      
                 @    ����                                                                                                                                                                                                                                                                                                                                               V       V/<4    �         
      _    # TODO: Somehow fix duplication between these three classes, maybe use scopes on user class5�_�         	       
           ����                                                                                                                                                                                                                                                                                                                                               V       V/�     �      	          1        .where(leagues: { region_id: region_id })5�_�   
                         ����                                                                                                                                                                                                                                                                                                                                               V       V/�     �      	               �      	       5�_�                           ����                                                                                                                                                                                                                                                                                                                                     	          V       V/�      �      	               []5�_�                           ����                                                                                                                                                                                                                                                                                                                                     	          V       V/�!     �      	               []5�_�                           ����                                                                                                                                                                                                                                                                                                                                     	          V       V/�$     �      	                 .merge()5�_�                       "    ����                                                                                                                                                                                                                                                                                                                                     	          V       V/�+    �      	         $        .merge(UsersInRegion.find())5�_�                       3    ����                                                                                                                                                                                                                                                                                                                                     	          V       V/�7    �      	         8        .merge(UsersInRegion.find(region_id: region.id))5�_�                    	       ����                                                                                                                                                                                                                                                                                                                                     	          V       V/�:     �      	          3        # .where(leagues: { region_id: region_id })5�_�                           ����                                                                                                                                                                                                                                                                                                                                     	          V       V/�<   
 �                @    # TODO: Somehow fix duplication between these three classes,   $    # maybe use scopes on user class5�_�                           ����                                                                                                                                                                                                                                                                                                                                               V       V/�H     �                  class << self5�_�                           ����                                                                                                                                                                                                                                                                                                                                               V       V/�I     �                  end5�_�                            ����                                                                                                                                                                                                                                                                                                                                                 V        V/�J     �               &    def find(region_id:, agegroup_id:)         UsersInAgegroup   '        .find(agegroup_id: agegroup_id)   8        .merge(UsersInRegion.find(region_id: region_id))       end5�_�                           ����                                                                                                                                                                                                                                                                                                                                                 V        V/�L    �               $  def find(region_id:, agegroup_id:)5�_�                             ����                                                                                                                                                                                                                                                                                                                                                 V        V/�b    �                module UsersInRegionAndAgegroup5�_�              
   	          ����                                                                                                                                                                                                                                                                                                                                               V       V/<5     �               *    #       maybe use scopes on user class5��