Vim�UnDo� �Ҷ*g�8V��(4��&�����Ƀ�Z!O      1  fill_in "Markdown", with: options[:markdown] ||      1      G   .   G   G   -    Tg��    _�                             ����                                                                                                                                                                                                                                                                                                                                                             T[f�     �                   �               5�_�                            ����                                                                                                                                                                                                                                                                                                                                                             T[f�    �                  5�_�                            ����                                                                                                                                                                                                                                                                                                                                                             T[f�     �               �               5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             T[f�    �                9  alias_method :create_post_but_before_save, :create_post�                  end�   
                 visit root_path�   	                 click_button "Create post"�      
              end�      	                yield�                    if block_given?�                &    fill_in "Markdown", with: "**hi**"�                <    fill_in "Title", with: options[:title] || "Learning iOS"�                     click_link "Create new post"�                    visit admin_path�                    authenticate�                   def create_post(options = {})5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             T[f�     �                  �               5�_�                            ����                                                                                                                                                                                                                                                                                                                                                             T[f�     �                 !def create_post_but_before_save()5�_�      	                 $    ����                                                                                                                                                                                                                                                                                                                                                             T[f�     �                   �               5�_�      
           	          ����                                                                                                                                                                                                                                                                                                                                                             T[f�     �                 �             5�_�   	              
          ����                                                                                                                                                                                                                                                                                                                                                             T[f�     �                 create_post()5�_�   
                    %    ����                                                                                                                                                                                                                                                                                                                                                             T[f�     �               &def create_post_but_before_save(*args)5�_�                       (    ����                                                                                                                                                                                                                                                                                                                                                             T[f�     �               .def create_post_but_before_save(*args, &block)5�_�                       %    ����                                                                                                                                                                                                                                                                                                                                                             T[f�     �               )def create_post_but_before_save(*args, &)5�_�                       %    ����                                                                                                                                                                                                                                                                                                                                                             T[f�     �               (def create_post_but_before_save(*args &)5�_�                       %    ����                                                                                                                                                                                                                                                                                                                                                             T[f�     �               'def create_post_but_before_save(*args&)5�_�                            ����                                                                                                                                                                                                                                                                                                                                                             T[g    �                7alias_method :create_post_but_before_save, :create_post5�_�                       %    ����                                                                                                                                                                                                                                                                                                                                                             T[g     �               &def create_post_but_before_save(*args)5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             T[g    �                   �             �                 create_post(*args)5�_�                            ����                                                                                                                                                                                                                                                                                                                                                             T[g4     �                  �               5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             T[g;     �                 def create_draft()5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             T[g=     �                 def create_draft(options = [])5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             T[g>    �             �                 def create_draft(options = {})5�_�                            ����                                                                                                                                                                                                                                                                                                                                                             T[gC     �             �             5�_�                            ����                                                                                                                                                                                                                                                                                                                                                             T[gD     �                 5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             T[gE     �                    end�                      check "Draft"�                0    create_post_but_before_save(title: title) do5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             T[gF    �               .  create_post_but_before_save(title: title) do5�_�      !                     ����                                                                                                                                                                                                                                                                                                                                                             T[g�    �                 �             5�_�      "          !      :    ����                                                                                                                                                                                                                                                                                                                                                  V        T[i�     �               :  fill_in "Title", with: options[:title] || "Learning iOS"5�_�   !   #           "      ;    ����                                                                                                                                                                                                                                                                                                                                                  V        T[i�     �               <  fill_in "Title", with: options[:title] || attributes_for()5�_�   "   $           #      A    ����                                                                                                                                                                                                                                                                                                                                                  V        T[i�     �               A  fill_in "Title", with: options[:title] || attributes_for(:post)5�_�   #   %           $      B    ����                                                                                                                                                                                                                                                                                                                                                  V        T[i�    �               C  fill_in "Title", with: options[:title] || attributes_for(:post)[]5�_�   $   &           %          ����                                                                                                                                                                                                                                                                                                                                                             T_��     �               $  fill_in "Markdown", with: "**hi**"5�_�   %   '           &          ����                                                                                                                                                                                                                                                                                                                                                             T_��     �                 fill_in "Markdown", with: �             5�_�   &   (           '      F    ����                                                                                                                                                                                                                                                                                                                               F          J       v   J    T_��    �               L  fill_in "Markdown", with: options[:title] || attributes_for(:post)[:title]5�_�   '   )           (           ����                                                                                                                                                                                                                                                                                                                                                             Tg��     �                  if block_given?5�_�   (   *           )          ����                                                                                                                                                                                                                                                                                                                                                             Tg��     �      	       �             5�_�   )   +           *          ����                                                                                                                                                                                                                                                                                                                                                             Tg��     �               	    yield     if block_given?5�_�   *   ,           +          ����                                                                                                                                                                                                                                                                                                                                                             Tg��     �                  end5�_�   +   -           ,          ����                                                                                                                                                                                                                                                                                                                                                             Tg��    �                   yield if block_given?5�_�   ,   .           -      1    ����                                                                                                                                                                                                                                                                                                                                                             Tg��    �               R  fill_in "Markdown", with: options[:markdown] || attributes_for(:post)[:markdown]5�_�   -   :           .          ����                                                                                                                                                                                                                                                                                                                                                             Tg�i     �                �                   def fill_in_fields   end�                5�_�   .   ;   /       :           ����                                                                                                                                                                                                                                                                                                                                                V       Tg�     �              5�_�   :   <           ;           ����                                                                                                                                                                                                                                                                                                                                                V       Tg�     �              5�_�   ;   =           <           ����                                                                                                                                                                                                                                                                                                                                                V       Tg�     �               P  fill_in "Title", with: options.fetch():title] || attributes_for(:post)[:title]5�_�   <   >           =      '    ����                                                                                                                                                                                                                                                                                                                               -          0       v   0    Tg�     �               O  fill_in "Title", with: options.fetch(:title] || attributes_for(:post)[:title]5�_�   =   ?           >      -    ����                                                                                                                                                                                                                                                                                                                               -          0       v   0    Tg�     �               K  fill_in "Title", with: options.fetch(:title attributes_for(:post)[:title]5�_�   >   @           ?      -    ����                                                                                                                                                                                                                                                                                                                               -          0       v   0    Tg�     �               L  fill_in "Title", with: options.fetch(:title, attributes_for(:post)[:title]5�_�   ?   B           @      L    ����                                                                                                                                                                                                                                                                                                                               #          1       V   L    Tg�    �               M  fill_in "Title", with: options.fetch(:title, attributes_for(:post)[:title])5�_�   @   C   A       B      1    ����                                                                                                                                                                                                                                                                                                                               #          1       V   L    Tg�     �               +  fill_in "Markdown", with: options.fetch()5�_�   B   D           C      *    ����                                                                                                                                                                                                                                                                                                                               #          1       V   L    Tg��     �               4  fill_in "Markdown", with: options.fetch(:markdown)5�_�   C   E           D      4    ����                                                                                                                                                                                                                                                                                                                               #          1       V   L    Tg��     �               3  fill_in "Markdown", with: options.fetch(:markdown5�_�   D   F           E      2    ����                                                                                                                                                                                                                                                                                                                               S          1       V   L    Tg��     �               T  fill_in "Markdown", with: options.fetch(:markdown attributes_for(:post)[:markdown]5�_�   E   G           F      3    ����                                                                                                                                                                                                                                                                                                                               S          1       V   L    Tg��     �               U  fill_in "Markdown", with: options.fetch(:markdown, attributes_for(:post)[:markdown]5�_�   F               G      U    ����                                                                                                                                                                                                                                                                                                                               S          1       V   L    Tg��    �               V  fill_in "Markdown", with: options.fetch(:markdown, attributes_for(:post)[:markdown])5�_�   @           B   A      1    ����                                                                                                                                                                                                                                                                                                                               #          1       V   L    Tg�    �              5�_�   .   0       :   /          ����                                                                                                                                                                                                                                                                                                                                                V       Tg�v     �               def fill_in_fields()5�_�   /   1           0          ����                                                                                                                                                                                                                                                                                                                                                V       Tg�w     �                def fill_in_fields(options = {})5�_�   0   5           1          ����                                                                                                                                                                                                                                                                                                                                                V       Tg�x     �                def fill_in_fields(options = {})5�_�   1   6   2       5           ����                                                                                                                                                                                                                                                                                                                                                V       Tg�     �              5�_�   5   7           6           ����                                                                                                                                                                                                                                                                                                                                                V       Tg�     �             �               I  fill_in "Title", with: options[:title] || attributes_for(:post)[:title]   1  fill_in "Markdown", with: options[:markdown] ||   $    attributes_for(:post)[:markdown]5�_�   6   8           7           ����                                                                                                                                                                                                                                                                                                                                                V       Tg�     �              5�_�   7   9           8          ����                                                                                                                                                                                                                                                                                                                                                V       Tg�     �                 �                 fill_in_fields()5�_�   8               9          ����                                                                                                                                                                                                                                                                                                                                                V       Tg�    �                 fill_in_fields(options)5�_�   1   3       5   2          ����                                                                                                                                                                                                                                                                                                                                                V       Tg�}     �              5�_�   2   4           3           ����                                                                                                                                                                                                                                                                                                                                                V       Tg�     �             �               I  fill_in "Title", with: options[:title] || attributes_for(:post)[:title]   1  fill_in "Markdown", with: options[:markdown] ||   $    attributes_for(:post)[:markdown]5�_�   3               4           ����                                                                                                                                                                                                                                                                                                                                                V       Tg�    �              5�_�            !         9    ����                                                                                                                                                                                                                                                                                                                                                  V        T[i�     �               >  fill_in "Title", with: options[:title] || "Learning iOS #{}"5�_�                       <    ����                                                                                                                                                                                                                                                                                                                                                  V        T[i�     �               @  fill_in "Title", with: options[:title] || "Learning iOS #{()}"5�_�                        =    ����                                                                                                                                                                                                                                                                                                                                                  V        T[i�     �               G  fill_in "Title", with: options[:title] || "Learning iOS #{(1..1000)}"5�_�                         E    ����                                                                                                                                                                                                                                                                                                                                                  V        T[i�    �               S  fill_in "Title", with: options[:title] || "Learning iOS #{(1..1000).to_a.sample}"5�_�                            ����                                                                                                                                                                                                                                                                                                                                       
           V        T[h+   
 �              5�_�                            ����                                                                                                                                                                                                                                                                                                                                                             T[f�     �                create5��