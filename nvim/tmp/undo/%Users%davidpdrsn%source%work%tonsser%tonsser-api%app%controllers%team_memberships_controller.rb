Vim�UnDo� 3/�t�x.�?��+J�hu����5>�*              1                       VI�r    _�                           ����                                                                                                                                                                                                                                                                                                                                                             VI�    �             �      	       5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             VI�     �                1    expose current_user, serializer: MeSerializer5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             VI�     �             �             5�_�                       1    ����                                                                                                                                                                                                                                                                                                                                                             VI�    �               1    expose current_user, serializer: MeSerializer5�_�                       %    ����                                                                                                                                                                                                                                                                                                                                                             VI�     �               /    expose current_user, serializer: serializer5�_�                       @    ����                                                                                                                                                                                                                                                                                                                                                             VI�     �               A    expose current_user, serializer: MeSerializerForVersion.new()5�_�      	                 G    ����                                                                                                                                                                                                                                                                                                                                                             VI�      �               I    expose current_user, serializer: MeSerializerForVersion.new(params[])5�_�      
           	      Q    ����                                                                                                                                                                                                                                                                                                                                                             VI�!     �               Q    expose current_user, serializer: MeSerializerForVersion.new(params[:version])5�_�   	              
      J    ����                                                                                                                                                                                                                                                                                                                                         J       V   [    VI�'    �                                 end�   
                                MeSerializer�   	                              else�      
          #                   V2::MeSerializer�      	          J    serializer = if VersionNumber.new(2).and_up.matches?(params[:version])5�_�   
                         ����                                                                                                                                                                                                                                                                                                                               J                 V   J    VI�M    �                L    # serializer = if VersionNumber.new(2).and_up.matches?(params[:version])   %    #                V2::MeSerializer       #              else   !    #                MeSerializer       #              end5�_�                           ����                                                                                                                                                                                                                                                                                                                               J                 V   J    VI�q     �             �      	       5�_�                            ����                                                                                                                                                                                                                                                                                                                               J                 V   J    VI�q    �                \    expose current_user, serializer: MeSerializerForVersion.new(params[:version]).serializer5�_�                            ����                                                                                                                                                                                                                                                                                                                                                             VI�@     �      	        5��