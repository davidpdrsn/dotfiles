Vim�UnDo� ʖg��?3�ZYC�|�{ �M�T��^���>Y�              2      :       :   :   :    T_�$    _�                             ����                                                                                                                                                                                                                                                                                                                                                             T_��    �               &describe `!p snip.rv = spec_name()` do�                   $0�                 it "$2" do�                   5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             T_�     �               
  it "" do5�_�                           ����                                                                                                                                                                                                                                                                                                                                                V        T_�
     �                         end5�_�                       %    ����                                                                                                                                                                                                                                                                                                                                                V        T_�     �               %  it "parses the markdown on save" do5�_�                           ����                                                                                                                                                                                                                                                                                                                                                V        T_�     �                 it ""5�_�                       "    ����                                                                                                                                                                                                                                                                                                                                                V        T_�     �               "  it "parses the markdown on save"5�_�                            ����                                                                                                                                                                                                                                                                                                                               !                  V   !    T_�     �                    $  it "parses the markdown on update"5�_�      	                 !    ����                                                                                                                                                                                                                                                                                                                               !                  V   !    T_�    �               "  it "parses the markdown on save"5�_�      
           	      -    ����                                                                                                                                                                                                                                                                                                                               !                  V   !    T_�     �                   �             �               -  it "parses the markdown on save and update"5�_�   	              
          ����                                                                                                                                                                                                                                                                                                                               !                  V   !    T_�)     �                   expect(post).to 5�_�   
                    "    ����                                                                                                                                                                                                                                                                                                                               !                  V   !    T_�+     �               #    expect(post).to have_received()5�_�                       )    ����                                                                                                                                                                                                                                                                                                                               !                  V   !    T_�.     �               )    expect(post).to have_received(:html=)5�_�                       /    ����                                                                                                                                                                                                                                                                                                                               !                  V   !    T_�0     �               0    expect(post).to have_received(:html=).with()5�_�                       4    ����                                                                                                                                                                                                                                                                                                                               !                  V   !    T_�1    �               4    expect(post).to have_received(:html=).with(html)5�_�                       /    ����                                                                                                                                                                                                                                                                                                                               !                  V   !    T_�e     �                   �             5�_�                           ����                                                                                                                                                                                                                                                                                                                               !                  V   !    T_�h     �                   post = double()5�_�                           ����                                                                                                                                                                                                                                                                                                                               !                  V   !    T_�h     �                   post = double("")5�_�                           ����                                                                                                                                                                                                                                                                                                                               !                  V   !    T_�i     �             5�_�                           ����                                                                                                                                                                                                                                                                                                                            	   !       	           V   !    T_�l    �         	          post = double("post")5�_�                       1    ����                                                                                                                                                                                                                                                                                                                            	   !       	           V   !    T_�y     �         
          �         	    5�_�                       )    ����                                                                                                                                                                                                                                                                                                                               !                  V   !    T_��     �               *    observer = ParseMarkdownObserver.new()5�_�                       .    ����                                                                                                                                                                                                                                                                                                                               !                  V   !    T_��     �      	             �      	       5�_�                           ����                                                                                                                                                                                                                                                                                                                               !                  V   !    T_��     �      	             observer.saved()5�_�                           ����                                                                                                                                                                                                                                                                                                                               !                  V   !    T_��     �      
             �      
       5�_�                    	       ����                                                                                                                                                                                                                                                                                                                               !                  V   !    T_��    �      
             observer.updated()5�_�                           ����                                                                                                                                                                                                                                                                                                                               !                  V   !    T_��     �                   �             5�_�                           ����                                                                                                                                                                                                                                                                                                                               !                  V   !    T_��     �                   parser = double()5�_�                           ����                                                                                                                                                                                                                                                                                                                               !                  V   !    T_��     �                   parser = double("")5�_�                           ����                                                                                                                                                                                                                                                                                                                               !                  V   !    T_��     �                   �             5�_�                       
    ����                                                                                                                                                                                                                                                                                                                               !                  V   !    T_��     �                   allow()5�_�                            ����                                                                                                                                                                                                                                                                                                                               !                  V   !    T_��     �                   allow(parser)5�_�      !                      ����                                                                                                                                                                                                                                                                                                                               !                  V   !    T_��     �                   allow(parser).to receive()5�_�       "           !      %    ����                                                                                                                                                                                                                                                                                                                               !                  V   !    T_��     �               %    allow(parser).to receive(:parser)5�_�   !   #           "      +    ����                                                                                                                                                                                                                                                                                                                               !                  V   !    T_��     �               ,    allow(parser).to receive(:parser).with()5�_�   "   $           #      4    ����                                                                                                                                                                                                                                                                                                                               !                  V   !    T_��     �               4    allow(parser).to receive(:parser).with(markdown)5�_�   #   %           $      @    ����                                                                                                                                                                                                                                                                                                                               !                  V   !    T_��   	 �               A    allow(parser).to receive(:parser).with(markdown).and_return()5�_�   $   &           %      /    ����                                                                                                                                                                                                                                                                                                                               !                  V   !    T_��     �                   �             5�_�   %   '           &          ����                                                                                                                                                                                                                                                                                                                               !                  V   !    T_��     �                   markdown = ""5�_�   &   (           '          ����                                                                                                                                                                                                                                                                                                                               !                  V   !    T_��     �                   �             5�_�   '   )           (          ����                                                                                                                                                                                                                                                                                                                               !                  V   !    T_��     �                   html = ""5�_�   (   *           )          ����                                                                                                                                                                                                                                                                                                                               !                  V   !    T_��   
 �                   html = "<strong></strong>"5�_�   )   +           *   	   $    ����                                                                                                                                                                                                                                                                                                                               !                  V   !    T_��    �      
         E    allow(parser).to receive(:parser).with(markdown).and_return(html)5�_�   *   ,           +      "    ����                                                                                                                                                                                                                                                                                                                               !                  V   !    T_�      �      	             �      	       5�_�   +   -           ,      
    ����                                                                                                                                                                                                                                                                                                                               !                  V   !    T_�     �      	             allow()5�_�   ,   .           -          ����                                                                                                                                                                                                                                                                                                                               !                  V   !    T_�    �      	             allow(post).to receive()5�_�   -   /           .           ����                                                                                                                                                                                                                                                                                                                               !                  V   !    T_�+     �      
             �      
       5�_�   .   0           /   	       ����                                                                                                                                                                                                                                                                                                                               !                  V   !    T_�-     �      	          	    allow5�_�   /   1           0          ����                                                                                                                                                                                                                                                                                                                               !                  V   !    T_�/    �               3    post = double("post", save: true, update: true)5�_�   0   2           1          ����                                                                                                                                                                                                                                                                                                                               !                  V   !    T_��     �      
       �      	       5�_�   1   3           2   	       ����                                                                                                                                                                                                                                                                                                                               !                  V   !    T_��     �      	          "    allow(post).to receive(:html=)5�_�   2   4           3      2    ����                                                                                                                                                                                                                                                                                                                               !                  V   !    T_��     �               G    post = double("post", markdown: markdown, save: true, update: true)5�_�   3   5           4      9    ����                                                                                                                                                                                                                                                                                                                               !                  V   !    T_��    �               H    post = double("post", markdown: markdown, save!: true, update: true)5�_�   4   6           5          ����                                                                                                                                                                                                                                                                                                                               !                  V   !    T_�%     �             �             5�_�   5   7           6      #    ����                                                                                                                                                                                                                                                                                                                               !                  V   !    T_�(     �               :    expect(post).to have_received(:html=).with(html).twice5�_�   6   8           7      #    ����                                                                                                                                                                                                                                                                                                                               !                  V   !    T_�*     �               6    expect(post).to have_received(:=).with(html).twice5�_�   7   9           8      (    ����                                                                                                                                                                                                                                                                                                                               (          2       v   2    T_�-    �               9    expect(post).to have_received(:save).with(html).twice5�_�   8   :           9      2    ����                                                                                                                                                                                                                                                                                                                               (          2       v   2    T_�2    �               :    post = double("post", markdown: markdown, save!: true)5�_�   9               :          ����                                                                                                                                                                                                                                                                                                                                                             T_�#    �                .    expect(post).to have_received(:save).twice5��