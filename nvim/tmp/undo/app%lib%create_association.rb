Vim�UnDo� ��l��v:+�C)c�ĮԔl<�n����73e�:      class                               T]�    _�                             ����                                                                                                                                                                                                                                                                                                                                                             T]�i     �                   �               5�_�                            ����                                                                                                                                                                                                                                                                                                                                                             T]�j     �                  5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             T]�k     �               
   :  def associate(records_with_ids:, of_type:, with_record:)   6    association_name = of_type.to_s.pluralize.downcase       7    with_record.public_send("#{association_name}=", [])       !    records_with_ids.each do |id|   @      associated_record = of_type.find_or_create_by(id: id.to_i)   D      with_record.public_send(association_name) << associated_record       end     end5�_�                            ����                                                                                                                                                                                                                                                                                                                                                             T]�m     �   	             end�      
            end�      	          B    with_record.public_send(association_name) << associated_record�                >    associated_record = of_type.find_or_create_by(id: id.to_i)�                  records_with_ids.each do |id|�                 �                5  with_record.public_send("#{association_name}=", [])�                 �                4  association_name = of_type.to_s.pluralize.downcase�                 8def associate(records_with_ids:, of_type:, with_record:)5�_�                     
        ����                                                                                                                                                                                                                                                                                                                                                             T]�    �               1class `!p snip.rv = class_name() + inheritance()`�                 $0�                 class�   
              # �   
            5��