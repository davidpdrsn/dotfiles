Vim�UnDo� �'���I��U��6
vr7!�$�A�ە_Sa�v��        factory :post do |f|            ;       ;   ;   ;    T_Ý    _�                             ����                                                                                                                                                                                                                                                                                                                                                             TS�     �                  �               �               5�_�                            ����                                                                                                                                                                                                                                                                                                                                                             TS�    �                  5�_�                            ����                                                                                                                                                                                                                                                                                                                                                             TS�     �                end�                  admin      true�   
               last_name  "User"�   	               first_name "Admin"�      
          factory :admin, class: User do�      	          ># This will use the User class (Admin would have been guessed)�                end�                  admin false�                  last_name  "Doe"�                  first_name "John"�                factory :user do5�_�                            ����                                                                                                                                                                                                                                                                                                                                                  V        TS�     �                    @  # This will use the User class (Admin would have been guessed)      factory :admin, class: User do       first_name "Admin"       last_name  "User"       admin      true     end5�_�                           ����                                                                                                                                                                                                                                                                                                                                                  V        TS�     �                 factory :user do5�_�                           ����                                                                                                                                                                                                                                                                                                                                                V       TS�     �                    last_name  "Doe"       admin false5�_�                           ����                                                                                                                                                                                                                                                                                                                                                V       TS�     �                   first_name "John"5�_�      	                     ����                                                                                                                                                                                                                                                                                                                                                V       TS�     �                   fi5�_�      
           	          ����                                                                                                                                                                                                                                                                                                                                                V       TS�     �                   title ''5�_�   	              
          ����                                                                                                                                                                                                                                                                                                                                                V       TS�     �                   �             5�_�   
                    
    ����                                                                                                                                                                                                                                                                                                                                                V       TS�     �                   bodt ''5�_�                           ����                                                                                                                                                                                                                                                                                                                                                V       TS��     �                   markdown ''5�_�                       7    ����                                                                                                                                                                                                                                                                                                                                                V       TS�    �               =    markdown 'its **fun** to learn ios, but also quite harfd'5�_�                           ����                                                                                                                                                                                                                                                                                                                                                  V        TX�     �      
       �             5�_�                           ����                                                                                                                                                                                                                                                                                                                                                  V        TX�     �         
    5�_�                           ����                                                                                                                                                                                                                                                                                                                                                  V        TX�     �                 factory :post do5�_�                    	       ����                                                                                                                                                                                                                                                                                                                                                  V        TX�     �      	          <    markdown 'its **fun** to learn ios, but also quite hard'5�_�                           ����                                                                                                                                                                                                                                                                                                                                                  V        TX�     �      	   
          title 'Learning iOS'5�_�                       	    ����                                                                                                                                                                                                                                                                                                                                                  V        TX�     �      	   
          name[] 'Learning iOS'5�_�                       
    ����                                                                                                                                                                                                                                                                                                                                                  V        TX�    �      	   
          name 'Learning iOS'5�_�                            ����                                                                                                                                                                                                                                                                                                                                                             TY��     �                �             �         
    5�_�                            ����                                                                                                                                                                                                                                                                                                                                                             TY��     �               %sequence(:username) { |n| "foo#{n}" }5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             TY��     �               '  sequence(:username) { |n| "foo#{n}" }5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             TY��     �               )    sequence(:username) { |n| "foo#{n}" }5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             TY��     �               &    sequence(:title) { |n| "foo#{n}" }5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             TY��    �                    title 'Learning iOS'5�_�                       <    ����                                                                                                                                                                                                                                                                                                                                                             TY��    �         
      <    markdown 'its **fun** to learn ios, but also quite hard'5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             T[R�    �                   �             5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             T[SH    �                    draft false5�_�                             ����                                                                                                                                                                                                                                                                                                                                                             T_��   	 �                   �             5�_�      !                  	    ����                                                                                                                                                                                                                                                                                                                                                             T_�    
 �                   slug title5�_�       "           !          ����                                                                                                                                                                                                                                                                                                                                                             T_�C     �                   slug :title5�_�   !   #           "          ����                                                                                                                                                                                                                                                                                                                                                             T_�C     �                   f.after_create {}5�_�   "   $           #          ����                                                                                                                                                                                                                                                                                                                                                             T_�G     �      	               �      	       �               4      caredate.user_id = caredate.provider.user_id }�               N    f.after_create { |caredate| caredate.user_id = caredate.provider.user_id }5�_�   #   %           $          ����                                                                                                                                                                                                                                                                                                                                                             T_�J     �                    f.after_create do |caredate|5�_�   $   &           %          ����                                                                                                                                                                                                                                                                                                                                                             T_�L    �      	               �      	       5�_�   %   '           &          ����                                                                                                                                                                                                                                                                                                                                                             T_�Y    �                2      caredate.user_id = caredate.provider.user_id5�_�   &   (           '          ����                                                                                                                                                                                                                                                                                                                                                             T_�b    �                 factory :post do5�_�   '   )           (      (    ����                                                                                                                                                                                                                                                                                                                                                             T_�n    �               (      post.slug = post.title.paramterize5�_�   (   *           )          ����                                                                                                                                                                                                       (                                                                                                                                                    T_��     �             �             5�_�   )   +           *           ����                                                                                                                                                                                                       (                                                                                                                             	          V       T_��     �                    f.after_create do |post|   )      post.slug = post.title.parameterize       end5�_�   *   ,           +          ����                                                                                                                                                                                                        (                                                                                                                                       V       T_��     �               0    sequence(:title) { |n| "Learning iOS #{n}" }5�_�   +   -           ,      -    ����                                                                                                                                                                                                        (                                                                                                                                '       v   '    T_��    �               /    sequence(:slug) { |n| "Learning iOS #{n}" }5�_�   ,   .           -           ����                                                                                                                                                                                                        (                                                                                                                                '       v   '    T_��    �   	                 name 'JavaScript'�                <    markdown 'its **fun** to learn ios, but also quite hard'5�_�   -   /           .          ����                                                                                                                                                                                                        (                                                                                                                                '       v   '    T_�W     �             �             5�_�   .   0           /          ����                                                                                                                                                                                                        (                                                                                                                                '       v   '    T_�X     �               <    markdown "its **fun** to learn ios, but also quite hard"5�_�   /   1           0          ����                                                                                                                                                                                                        (                                                                                                                                       v       T_�\    �               8    html "its **fun** to learn ios, but also quite hard"5�_�   0   2           1           ����                                                                                                                                                                                                        (                                                                                                                                       v       T_��    �                   �             5�_�   1   3           2          ����                                                                                                                                                                                                        (                                                                                                                                       v       T_��    �                   require "pry"; binding.pry5�_�   2   4           3          ����                                                                                                                                                                                                        (                                                                                                                                       v       T_�    �                    binding.pry5�_�   3   6           4          ����                                                                                                                                                                                                        (                                                                                                                                       v       T_É     �               <    sequence(:slug) { |n| "Learning iOS #{n}".parameterize }5�_�   4   7   5       6          ����                                                                                                                                                                                                        (                                                                                                                                       v       T_Ê     �               4    (:slug) { |n| "Learning iOS #{n}".parameterize }5�_�   6   8           7          ����                                                                                                                                                                                                        (                                                                                                                                       v       T_Ë     �               3    :slug) { |n| "Learning iOS #{n}".parameterize }5�_�   7   9           8          ����                                                                                                                                                                                                        (                                                                                                                                       v       T_Í     �               2    slug) { |n| "Learning iOS #{n}".parameterize }5�_�   8   :           9          ����                                                                                                                                                                                                        (                                                                                                                                       v       T_Î     �               1    slug { |n| "Learning iOS #{n}".parameterize }5�_�   9   ;           :          ����                                                                                                                                                                                                        (                                                                                                                                       v       T_Ð    �               -    slug { "Learning iOS #{n}".parameterize }5�_�   :               ;          ����                                                                                                                                                                                                        (                                                                                                                                       v       T_Ü    �                 factory :post do |f|5�_�   4           6   5          ����                                                                                                                                                                                                        (                                                                                                                                       v       T_É     �               3    (slug) { |n| "Learning iOS #{n}".parameterize }5�_�                            ����                                                                                                                                                                                                                                                                                                                                                  V        TX~     �             �               FactoryGirl.define do     factory :post do       title 'Learning iOS'   <    markdown 'its **fun** to learn ios, but also quite hard'     end   end5��