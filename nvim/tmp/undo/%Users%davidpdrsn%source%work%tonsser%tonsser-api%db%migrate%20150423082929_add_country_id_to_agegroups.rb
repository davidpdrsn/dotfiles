Vim�UnDo� `B��t�O���0+�ʸ6_r�H!C�q: �      0    add_column :agegroups, :country_id, :integer      0      
       
   
   
    U8�    _�                        0    ����                                                                                                                                                                                                                                                                                                                                                             U8�n    �               0    add_column :agegroups, :country_id, :integer5�_�                            ����                                                                                                                                                                                                                                                                                                                                                             U8��     �             �                   �             5�_�                            ����                                                                                                                                                                                                                                                                                                                                                             U8��     �         	       5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             U8��     �         	            agegroup.update()5�_�                            ����                                                                                                                                                                                                                                                                                                                                                             U8��     �         
          �         	    5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             U8��     �         
           denmark = Country.find_by!()5�_�                       &    ����                                                                                                                                                                                                                                                                                                                                                             U8��    �         
      (    denmark = Country.find_by!(code: "")5�_�      	                 0    ����                                                                                                                                                                                                                                                                                                                                                             U8��    �         
      =    add_column :agegroups, :country_id, :integer, null: false5�_�      
           	           ����                                                                                                                                                                                                                                                                                                                                                  V        U8�E   
 �                    *    denmark = Country.find_by!(code: "DK")   #    Agegroup.all.each do |agegroup|   -      agegroup.update(country_id: denmark.id)       end5�_�   	               
      0    ����                                                                                                                                                                                                                                                                                                                                                             U8�    �               0    add_column :agegroups, :country_id, :integer5��