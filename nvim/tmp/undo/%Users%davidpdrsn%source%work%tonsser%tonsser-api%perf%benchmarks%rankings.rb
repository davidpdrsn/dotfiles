Vim�UnDo� @�sق���Cz�������Ւ%!;4ԉ-W��M      `        config.url = @http.get "/rankings/goals", league_id: league.id, agegroup_id: agegroup.id      `      (       (   (   (    V0�y    _�                             ����                                                                                                                                                                                                                                                                                                                                                             V0�H     �               5�_�                            ����                                                                                                                                                                                                                                                                                                                                                             V0�I     �                  5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             V0�J    �                 class StaticData5�_�                            ����                                                                                                                                                                                                                                                                                                                                                V       V0�Q     �                +      5.times { @factory.create :position }   ,      5.times { @factory.create :formation }   '      5.times { @factory.create :role }   *      5.times { @factory.create :country }   C      5.times { @factory.create :agegroup, country: Country.first }   A      5.times { @factory.create :region, country: Country.first }5�_�                       !    ����                                                                                                                                                                                                                                                                                                                                                V       V0�S    �               -        config.url = @http.get "/static_data"5�_�                       )    ����                                                                                                                                                                                                                                                                                                                                                V       V0�g    �               *        config.url = @http.get "/rankings"5�_�                       *    ����                                                                                                                                                                                                                                                                                                                                                  V       V0��    �                       �             5�_�      	                 $    ����                                                                                                                                                                                                                                                                                                                                                  V       V0��    �               '        config.number_of_requests = 1005�_�      
           	      +    ����                                                                                                                                                                                                                                                                                                                                                  V       V0��     �               0        config.url = @http.get "/rankings/:type"5�_�   	              
      
    ����                                                                                                                                                                                                                                                                                                                                                  V       V0��     �             �                     �             5�_�   
                        ����                                                                                                                                                                                                                                                                                                                                                  V       V0��     �                     10.times do 5�_�                            ����                                                                                                                                                                                                                                                                                                                                                  V       V0��     �                5�_�                       %    ����                                                                                                                                                                                                                                                                                                                                                  V       V0��    �                       �             5�_�                       0    ����                                                                                                                                                                                                                                                                                                                               %          *       V   *    V0��     �               0        config.url = @http.get "/rankings/goals"5�_�                       
    ����                                                                                                                                                                                                                                                                                                                               %          *       V   *    V0��     �                     �             5�_�                       %    ����                                                                                                                                                                                                                                                                                                                               %          *       V   *    V0��     �             �             5�_�                           ����                                                                                                                                                                                                                                                                                                                               %          *       V   *    V0��     �             5�_�                            ����                                                                                                                                                                                                                                                                                                                                          %       v   %    V0��    �               &      region = @factory.create :region5�_�                       >    ����                                                                                                                                                                                                                                                                                                                                                v       V0�     �               N        config.url = @http.get "/rankings/goals", region_id: 1, agegroup_id: 15�_�                       )    ����                                                                                                                                                                                                                                                                                                                                                v       V0�     �             �             5�_�                       "    ����                                                                                                                                                                                                                                                                                                                               "          )       v   )    V0�     �               *      agegroup = @factory.create :agegroup5�_�                           ����                                                                                                                                                                                                                                                                                                                                                v       V0�     �                       �             5�_�                       &    ����                                                                                                                                                                                                                                                                                                                                                v       V0�     �               &      league = @factory.create :league5�_�                       =    ����                                                                                                                                                                                                                                                                                                                                                v       V0�     �               \        config.url = @http.get "/rankings/goals", region_id: 1, league_id: 1, agegroup_id: 15�_�                       R    ����                                                                                                                                                                                                                                                                                                                                                v       V0�!     �               d        config.url = @http.get "/rankings/goals", region_id: region.id, league_id: 1, agegroup_id: 15�_�                       R    ����                                                                                                                                                                                                                                                                                                                                                v       V0�"     �               c        config.url = @http.get "/rankings/goals", region_id: region.id, league_id:1, agegroup_id: 15�_�                       l    ����                                                                                                                                                                                                                                                                                                                                                v       V0�'   	 �               l        config.url = @http.get "/rankings/goals", region_id: region.id, league_id: league.id, agegroup_id: 15�_�                       2    ����                                                                                                                                                                                                                                                                                                                               2          G       v   G    V0��   
 �               v        config.url = @http.get "/rankings/goals", region_id: region.id, league_id: league.id, agegroup_id: agegroup.id5�_�                       4    ����                                                                                                                                                                                                                                                                                                                               2          G       v   G    V0��     �               4        home = @factory.create :team, league: league5�_�                           ����                                                                                                                                                                                                                                                                                                                               2          G       v   G    V0��     �                &        match = @factory.create :match5�_�                            ����                                                                                                                                                                                                                                                                                                                               2          G       v   G    V0��     �             �             5�_�      !                  &    ����                                                                                                                                                                                                                                                                                                                               2          G       v   G    V0��    �               &        match = @factory.create :match5�_�       #           !          ����                                                                                                                                                                                                                                                                                                                               2          G       v   G    V0�    �                +        @factory.create :goal, match: match5�_�   !   $   "       #          ����                                                                                                                                                                                                                                                                                                                               2          G       v   G    V0�     �             �             5�_�   #   %           $          ����                                                                                                                                                                                                                                                                                                                               2          G       v   G    V0�     �               4        home = @factory.create :team, league: league5�_�   $   &           %      8    ����                                                                                                                                                                                                                                                                                                                               2          G       v   G    V0�    �               _        match = @factory.create :match, home_team: home, home_team_score: 3, away_team_score: 15�_�   %   '           &      D    ����                                                                                                                                                                                                                                                                                                                               2          G       v   G    V0�+    �               u        match = @factory.create :match, home_team: home, away_team: away_team, home_team_score: 3, away_team_score: 15�_�   &   (           '      G    ����                                                                                                                                                                                                                                                                                                                               2          G       v   G    V0�m     �                     �             5�_�   '               (      `    ����                                                                                                                                                                                                                                                                                                                               2          G       v   G    V0�x    �               `        config.url = @http.get "/rankings/goals", league_id: league.id, agegroup_id: agegroup.id5�_�   !           #   "          ����                                                                                                                                                                                                                                                                                                                               2          G       v   G    V0�     �              5��