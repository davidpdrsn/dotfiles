Vim�UnDo� �fZ_��ߧ2��A�'n�ʴ�R��
�        def appearance            *       *   *   *    U,z�    _�                            ����                                                                                                                                                                                                                                                                                                                                                             U,XY    �                  5�_�                            ����                                                                                                                                                                                                                                                                                                                                                  V        U,[�    �                 �             5�_�                           ����                                                                                                                                                                                                                                                                                                                                                  V        U,[�     �               !  validates :name, presence: true5�_�                           ����                                                                                                                                                                                                                                                                                                                                                  V        U,[�    �               (  validates :name, :name, presence: true5�_�                           ����                                                                                                                                                                                                                                                                                                                                                  V        U,[�    �               +  validates :name, :user_id, presence: true5�_�                       !    ����                                                                                                                                                                                                                                                                                                                                                  V        U,[�    �               1  validates :name, :user_id, :url, presence: true5�_�                           ����                                                                                                                                                                                                                                                                                                                                                  V        U,\�     �                 �             5�_�      	                     ����                                                                                                                                                                                                                                                                                                                                                  V        U,\�    �             5�_�      
           	           ����                                                                                                                                                                                                                                                                                                                                                  V        U,]�     �             �             5�_�   	              
          ����                                                                                                                                                                                                                                                                                                                                                  V        U,]�   	 �                 belongs_to :video_type5�_�   
                        ����                                                                                                                                                                                                                                                                                                                                                  V        U,^7     �                 �             5�_�                           ����                                                                                                                                                                                                                                                                                                                            
                      V        U,^9   
 �      	             �      	       5�_�                           ����                                                                                                                                                                                                                                                                                                                                                  V        U,^<     �      	             �      	       5�_�                           ����                                                                                                                                                                                                                                                                                                                                                  V        U,^<    �      	             ""5�_�                           ����                                                                                                                                                                                                                                                                                                                                                  V        U,^A     �      	             "Tutorial"5�_�                           ����                                                                                                                                                                                                                                                                                                                                                  V        U,^H    �      	             video_type.name5�_�                    	       ����                                                                                                                                                                                                                                                                                                                                                             U,q    �             �   	              �   	          5�_�                            ����                                                                                                                                                                                                                                                                                                                                                             U,q!     �                5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             U,q"    �                   []5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             U,q=     �                   Move.all5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             U,qE     �                   Appearances.where()5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             U,qI     �                   Appearances.where(vid)5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             U,qO     �                   self.app5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             U,qS    �                   Appearance.where()5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             U,qX     �               '    Appearance.where(video_id: self.id)5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             U,qY    �               "    Appearance.where(video_id:.id)5�_�                       "    ����                                                                                                                                                                                                                                                                                                                                                             U,q^     �             �               "    Appearance.where(video_id: id)5�_�                       +    ����                                                                                                                                                                                                                                                                                                                                                             U,qh     �               6    Appearance.where(video_id: id).map do |Appearance|5�_�                            ����                                                                                                                                                                                                                                                                                                                                                             U,qi     �                5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             U,qk     �                      appearance       end5�_�                        6    ����                                                                                                                                                                                                                                                                                                                                                             U,qk     �               6    Appearance.where(video_id: id).map do |appearance|5�_�      !                  '    ����                                                                                                                                                                                                                                                                                                                                                             U,qm    �               (    Appearance.where(video_id: id).map()5�_�       "           !      
    ����                                                                                                                                                                                                                                                                                                                                                             U,qs     �                   �             5�_�   !   #           "          ����                                                                                                                                                                                                                                                                                                                                                             U,qv     �                
    # TODO5�_�   "   $           #      "    ����                                                                                                                                                                                                                                                                                                                                                             U,qx     �               .    Appearance.where(video_id: id).map(&:move)5�_�   #   %           $      +    ����                                                                                                                                                                                                                                                                                                                                                             U,q{    �               8    Appearance.where(video_id: id).include().map(&:move)5�_�   $   &           %      *    ����                                                                                                                                                                                                                                                                                                                                                             U,q�    �               =    Appearance.where(video_id: id).include(:move).map(&:move)5�_�   %   '           &          ����                                                                                                                                                                                                                                                                                                                               
                 V   *    U,q�    �                   �             5�_�   &   (           '          ����                                                                                                                                                                                                                                                                                                                               (          (       V   (    U,z�     �   
              def moves5�_�   '   )           (          ����                                                                                                                                                                                                                                                                                                                               (          (       V   (    U,z�     �                )    # TODO: Test that eager loading works5�_�   (   *           )      "    ����                                                                                                                                                                                                                                                                                                                               (          (       V   (    U,z�    �               >    Appearance.where(video_id: id).includes(:move).map(&:move)5�_�   )               *          ����                                                                                                                                                                                                                                                                                                                               (          (       V   (    U,z�    �   
              def appearance5��