Vim�UnDo� �U��gnQ�u��u�	BV��OG�O��0���   %   !  scope :in_region, ->(region) {       !      8       8   8   8    U�jd    _�                     
        ����                                                                                                                                                                                                                                                                                                                                                  V        U�f{     �   
      "        �   
      !    5�_�                           ����                                                                                                                                                                                                                                                                                                                                                  V        U�f�     �         #        def self.in_region()5�_�                           ����                                                                                                                                                                                                                                                                                                                                                  V        U�f�    �         $          �         #    5�_�                           ����                                                                                                                                                                                                                                                                                                                                                  V        U�f�    �         %          �         $    5�_�                           ����                                                                                                                                                                                                                                                                                                                                                  V        U�f�    �         %          all5�_�                           ����                                                                                                                                                                                                                                                                                                                                                  V        U�g    �         %          region.clubs5�_�                           ����                                                                                                                                                                                                                                                                                                                                                  V        U�g     �         %          region.teams5�_�      	                     ����                                                                                                                                                                                                                                                                                                                                                  V        U�g     �         %          region.teams.flat_map()5�_�      
           	      !    ����                                                                                                                                                                                                                                                                                                                                                  V        U�g   	 �         %      !    region.teams.flat_map(&:club)5�_�   	              
          ����                                                                                                                                                                                                                                                                                                                                                  V        U�h'     �         %      &    region.teams.flat_map(&:club).uniq5�_�   
                        ����                                                                                                                                                                                                                                                                                                                                                  V        U�h-   
 �         %          region.teams.joins()5�_�                           ����                                                                                                                                                                                                                                                                                                                                                  V        U�h7     �         %          region.teams.joins(:club)5�_�                           ����                                                                                                                                                                                                                                                                                                                                                  V        U�hG    �         %          region.teams.includes()5�_�                       !    ����                                                                                                                                                                                                                                                                                                                                                  V        U�hL     �         %      !    region.teams.includes(:clubs)5�_�                       +    ����                                                                                                                                                                                                                                                                                                                                                  V        U�hO    �         %      ,    region.teams.includes(:clubs).flat_map()5�_�                       3    ����                                                                                                                                                                                                                                                                                                                                                  V        U�hS    �         %      3    region.teams.includes(:clubs).flat_map(&:clubs)5�_�                           ����                                                                                                                                                                                                                                                                                                                                                  V        U�h[     �         %      3    region.teams.includes(:clubs).flat_map(&:clubs)5�_�                       1    ����                                                                                                                                                                                                                                                                                                                                                  V        U�h]    �         %      2    region.teams.includes(:club).flat_map(&:clubs)5�_�                       /    ����                                                                                                                                                                                                                                                                                                                                                  V        U�i/     �         &          �         %    5�_�                           ����                                                                                                                                                                                                                                                                                                                                                  V        U�i/    �         &          pry5�_�                           ����                                                                                                                                                                                                                                                                                                                                                  V        U�iE    �                    require "pry"; binding.pry5�_�                           ����                                                                                                                                                                                                                                                                                                                                                  V        U�i�     �                     # require "pry"; binding.pry5�_�                       1    ����                                                                                                                                                                                                                                                                                                                                                  V        U�i�    �         %      1    region.teams.includes(:club).flat_map(&:club)5�_�                       5    ����                                                                                                                                                                                                                                                                                                                                                  V        U�i�     �         &          �         %    5�_�                           ����                                                                                                                                                                                                                                                                                                                                                  V        U�i�     �         &          Club.where()5�_�                          ����                                                                                                                                                                                                                                                                                                                                                  V        U�i�    �                    Club.where(id:)5�_�                           ����                                                                                                                                                                                                                                                                                                                                                  V        U�i�    �                    # Club.where(id:)5�_�                           ����                                                                                                                                                                                                                                                                                                                                                  V        U�i�     �         &          Club.where(id:)5�_�                            ����                                                                                                                                                                                                                                                                                                                                                  V        U�i�     �         '          Club.where(id:5�_�      !                      ����                                                                                                                                                                                                                                                                                                                                                  V        U�i�     �                              )5�_�       "           !          ����                                                                                                                                                                                                                                                                                                                                                  V        U�i�     �                6    region.teams.includes(:club).flat_map(&:club).uniq5�_�   !   #           "          ����                                                                                                                                                                                                                                                                                                                                                  V        U�i�     �         '    �         '    5�_�   "   %           #          ����                                                                                                                                                                                                                                                                                                                                                  V        U�i�     �         (      	      id:   6    region.teams.includes(:club).flat_map(&:club).uniq5�_�   #   &   $       %          ����                                                                                                                                                                                                                                                                                                                                                  V        U�i�     �         '    �         '    5�_�   %   '           &          ����                                                                                                                                                                                                                                                                                                                                                  V        U�i�     �                <      id: region.teams.includes(:club).flat_map(&:club).uniq5�_�   &   (           '          ����                                                                                                                                                                                                                                                                                                                                                  V        U�i�     �         (      <      id: region.teams.includes(:club).flat_map(&:club).uniq5�_�   '   )           (          ����                                                                                                                                                                                                                                                                                                                                                  V        U�i�     �         (            id: region.teams.pluc()5�_�   (   *           )          ����                                                                                                                                                                                                                                                                                                                                                  V        U�i�    �         (            id: region.teams.pluck()5�_�   )   +           *          ����                                                                                                                                                                                                                                                                                                                                                  V        U�i�     �                >      # id: region.teams.includes(:club).flat_map(&:club).uniq5�_�   *   ,           +          ����                                                                                                                                                                                                                                                                                                                                                  V        U�j      �         '          Club.where(   &      id: region.teams.pluck(:club_id)5�_�   +   -           ,          ����                                                                                                                                                                                                                                                                                                                                                  V        U�j      �         &      0    Club.where( id: region.teams.pluck(:club_id)5�_�   ,   .           -          ����                                                                                                                                                                                                                                                                                                                                                  V        U�j    �         &      /    Club.where(id: region.teams.pluck(:club_id)       )5�_�   -   /           .          ����                                                                                                                                                                                                                                                                                                                                                  V        U�jG     �      	   &        �         %    5�_�   .   0           /          ����                                                                                                                                                                                                                                                                                                                                                  V        U�jL     �      	   '        scope :in_region, ->()5�_�   /   1           0          ����                                                                                                                                                                                                                                                                                                                                                  V        U�jM     �      	   '        scope :in_region, ->(region)5�_�   0   2           1           ����                                                                                                                                                                                                                                                                                                                                                  V        U�jM     �      
   '      !  scope :in_region, ->(region) {}5�_�   1   3           2          ����                                                                                                                                                                                                                                                                                                                                                  V        U�jO     �                0    Club.where(id: region.teams.pluck(:club_id))5�_�   2   4           3          ����                                                                                                                                                                                                                                                                                                                                                  V        U�jP     �      
   '    �      	   '    5�_�   3   5           4   	       ����                                                                                                                                                                                                                                                                                                                                                  V        U�jQ     �      
   (      0    Club.where(id: region.teams.pluck(:club_id))5�_�   4   6           5           ����                                                                                                                                                                                                                                                                                                                                                 V       U�jT    �                      def self.in_region(region)     end5�_�   5   7           6   	       ����                                                                                                                                                                                                                                                                                                                                                 V       U�jX    �      
   %      ,    .where(id: region.teams.pluck(:club_id))5�_�   6   8           7      !    ����                                                                                                                                                                                                                                                                                                                                                 V       U�j`     �      	   %      !  scope :in_region, ->(region) { 5�_�   7               8      !    ����                                                                                                                                                                                                                                                                                                                                                 V       U�jc    �      	   %      !  scope :in_region, ->(region) {}5�_�   #           %   $          ����                                                                                                                                                                                                                                                                                                                                                  V        U�i�     �         '            id: region.teams.5�_�                      6    ����                                                                                                                                                                                                                                                                                                                                                  V        U�i�     �         &      1    region.teams.includes(:club).flat_map(&:club)5�_�                            ����                                                                                                                                                                                                                                                                                                                                                  V        U�i�     �         &          region.teams.pluck5��