Vim�UnDo� ��	�y{9X�e�L;�M&��<�	�B����Z�   .                                  UKk^    _�                             ����                                                                                                                                                                                                                                                                                                                                                             UB-     �                   �               5�_�                            ����                                                                                                                                                                                                                                                                                                                                                             UB-    �                  5�_�                    )        ����    (                                                                                                                                                                                                                                                                                                                       )           k           V        UB-H     �   (   )       C       class Norway   *      def initialize(notification, helper)   $        @notification = notification           @helper = helper   	      end             def new_follower   (        "#{name} begynte å følge deg."   	      end             def mvp_award   >        "Gratulerer! Du har blitt valgt til Team of the Week!"   	      end             def match_input   0        firstname = @notification.user.firstname   �        "#{firstname}, du kan nå oppdatere lagoppstilling, mål og målgivende pasninger. #{result.home_team} - #{result.away_team}. Resultat: #{result.score}."   	      end             def feed_story_like   7        case @notification.meta_data["feed_story_type"]           when "achievement"   )          "#{name} likte din prestasjon."           when "joined-team"   )          "#{name} likte overgangen din."           when "mvp-team-player"   ?          "#{name} likte at du ble valgt til Team of the Week."           end   	      end       %      delegate :result, to: "@helper"   #      delegate :name, to: "@helper"       end           class Swedish   *      def initialize(notification, helper)   $        @notification = notification           @helper = helper   	      end             def new_follower   &        "#{name} började följa dig."   	      end             def mvp_award   ?        "Grattis! Du har blivit uttagen till Team of the Week!"   	      end             def match_input   0        firstname = @notification.user.firstname   �        "#{firstname}, du kan nu uppdatera startformationen, mål och assists. #{result.home_team} - #{result.away_team}. Resultat: #{result.score}."   	      end             def feed_story_like   7        case @notification.meta_data["feed_story_type"]           when "achievement"   +          "#{name} gillade din prestation."           when "joined-team"   +          "#{name} gillade din övergång."           when "mvp-team-player"   F          "#{name} gillade att du blev uttagen till Team of the Week."           end   	      end       %      delegate :result, to: "@helper"   #      delegate :name, to: "@helper"       end5�_�                    (        ����    (                                                                                                                                                                                                                                                                                                                       )           )           V        UB-I    �   '   (           5�_�                    $        ����                                                                                                                                                                                                                                                                                                                                                             UKjv    �   $   *   )    �   $   %   )    5�_�                    )       ����                                                                                                                                                                                                                                                                                                                                                             UKj~    �   (   *   .      %      delegate :result, to: "@helper"5�_�                    )        ����                                                                                                                                                                                                                                                                                                                            )          +          V       UKj�    �   (   ,   .      (      delegate :firstname, to: "@helper"   %      delegate :result, to: "@helper"   #      delegate :name, to: "@helper"5�_�      	              &       ����                                                                                                                                                                                                                                                                                                                            )          +          V       UKkQ     �   &   (   .    �   &   '   .    5�_�      
           	   '       ����                                                                                                                                                                                                                                                                                                                            '          '   "       v   "    UKkT     �   &   (   /      H        let(:profile_views_text) { "Morten, folk holder øje med dig." }5�_�   	              
   '   -    ����                                                                                                                                                                                                                                                                                                                            '          '   "       v   "    UKkV     �   &   (   /      -        "Morten, folk holder øje med dig." }5�_�   
                 '   	    ����                                                                                                                                                                                                                                                                                                                            '          '   "       v   "    UKk[     �   &   (   /      +        "Morten, folk holder øje med dig."5�_�                    '       ����                                                                                                                                                                                                                                                                                                                            '          '   "       v   "    UKk\     �   &   (   /      (        "#{}, folk holder øje med dig."5�_�                     &       ����                                                                                                                                                                                                                                                                                                                            '          '   "       v   "    UKk]    �   %   &          1        "#{firstname}, folk holder oeje med dig."5��