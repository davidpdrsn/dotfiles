Vim�UnDo� V` u�>Hh�n��2�I;��������|�                    S       S   S   S    T_��   0 _�                             ����                                                                                                                                                                                                                                                                                                                                                             TT(     �         #    5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             TT(      �         '        scenario '$1' do�         '        end�         '          $2�         %        �         $    5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             TT(&     �         (          e�         '          5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             TT(,     �         (          expect(page).to 5�_�                       "    ����                                                                                                                                                                                                                                                                                                                                                             TT(/    �         (      #    expect(page).to have_content ""5�_�                    $        ����                                                                                                                                                                                                                                                                                                                            '           $           V        TU0     �   #   $                def make_date(format)   3    ActiveSupport::TimeZone["Sydney"].parse(format)     end5�_�                       %    ����                                                                                                                                                                                                                                                                                                                            $           $           V        TU3    �         $      ?    post = create :post, created_at: make_date('2001-12-12 20')5�_�      	              #       ����                                                                                                                                                                                                                                                                                                                            $           $           V        TU<     �   $   &   (        scenario '$1' do�   &   (   (        end�   %   '   (          $2�   $   (   &        sce�   #   &   %        �   #   %   $    5�_�      
           	   &       ����                                                                                                                                                                                                                                                                                                                            (           (           V        TUD     �   %   '   (          5�_�   	              
   &   ?    ����                                                                                                                                                                                                                                                                                                                            (           (           V        TUI     �   &   (   )          �   &   (   (    5�_�   
                 '       ����                                                                                                                                                                                                                                                                                                                            )           )           V        TUK     �   &   (   )          visit ""5�_�                    &   @    ����                                                                                                                                                                                                                                                                                                                            )           )           V        TUT     �   %   '   )      @    post = create :post, created_at: Time.parse('2001-12-12 20')5�_�                    &   J    ����                                                                                                                                                                                                                                                                                                                            )           )           V        TUV     �   %   '   )      K    post = create :post, created_at: Time.parse('2001-12-12 20'), title: ""5�_�                    &   L    ����                                                                                                                                                                                                                                                                                                                            )           )           V        TUX     �   %   '   )      Q    post = create :post, created_at: Time.parse('2001-12-12 20'), title: "A psot"5�_�                    '       ����                                                                                                                                                                                                                                                                                                                            )           )           V        TUY     �   (   *   +          e�   '   *   *          �   '   )   )    5�_�                    )       ����                                                                                                                                                                                                                                                                                                                            +           +           V        TU\     �   (   *   +          expect(page).to 5�_�                    )   "    ����                                                                                                                                                                                                                                                                                                                            +           +           V        TU]    �   (   *   +      #    expect(page).to have_content ""5�_�                    '       ����                                                                                                                                                                                                                                                                                                                            +           +           V        TU�     �   &   (   +          visit "/2001/12/12/a-post"5�_�                    '       ����                                                                                                                                                                                                                                                                                                                            +           +           V        TU�     �   &   (   +          visit "/2001/1212/a-post"5�_�                    '       ����                                                                                                                                                                                                                                                                                                                            +           +           V        TU�    �   &   (   +          visit "/2001/122/a-post"5�_�                    &       ����                                                                                                                                                                                                                                                                                                                            +           +           V        TU�    �   &   (   ,          �   &   (   +    5�_�                    '       ����                                                                                                                                                                                                                                                                                                                            ,           ,           V        TU�    �   &   '              require 'pry'; binding.pry5�_�                    '       ����                                                                                                                                                                                                                                                                                                                            +           +           V        TU{   	 �   &   (   +          visit "/2001/12/a-post"5�_�                    '       ����                                                                                                                                                                                                                                                                                                                            +           +           V        TU}   
 �   &   (   +           visit "posts/2001/12/a-post"5�_�                    $        ����                                                                                                                                                                                                                                                                                                                            *          $           V       TU�    �   #   $              0  scenario 'viewing a post by the pretty url' do   Q    post = create :post, created_at: Time.parse('2001-12-12 20'), title: "A post"   !    visit "/posts/2001/12/a-post"       )    expect(page).to have_content "A post"     end5�_�                            ����                                                                                                                                                                                                                                                                                                                                                             TY��    �       "          /      expect(page).to have_content '12/12/2001'�      !              within 'article.post' do�                @    post = create :post, created_at: Time.parse('2001-12-12 20')�                2  scenario 'sees the date the post was created' do�                '      expect(page).to have_content 'hi'�                #    within 'article.post strong' do�                +    post = create :post, markdown: '**hi**'�                0  scenario 'sees the parsed markdown as html' do�                    within '.posts' do�      
            scenario 'sees the posts' do�                ,  scenario 'sees when there are no posts' do�                feature 'viewing posts' do�                 require 'rails_helper'5�_�                       )    ����                                                                                                                                                                                                                                                                                                                                                             TY��    �         $      ,  scenario "sees when there are no posts" do5�_�                       )    ����                                                                                                                                                                                                                                                                                                                               5          )       v   )    TY��     �         $      9  scenario "sees when there are no posts", focus: true do5�_�                   	       ����                                                                                                                                                                                                                                                                                                                               5          )       v   )    TY��    �      
   $        scenario "sees the posts" do�   	   
   $    5�_�                            ����                                                                                                                                                                                                                                                                                                                               5          )       v   )    TY��     �                    within ".posts" do5�_�      !                      ����                                                                                                                                                                                                                                                                                                                               5          )       v   )    TY��     �                    end5�_�       $           !          ����                                                                                                                                                                                                                                                                                                                               5          )       v   )    TY��    �         "      -      expect(page).to have_content post.title5�_�   !   %   "       $          ����                                                                                                                                                                                                                                                                                                                               5          )       v   )    TY�;     �         "      +    post = create :post, markdown: "**hi**"5�_�   $   &           %          ����                                                                                                                                                                                                                                                                                                                               5          )       v   )    TY�<     �         "      &    = create :post, markdown: "**hi**"5�_�   %   '           &          ����                                                                                                                                                                                                                                                                                                                               5          )       v   )    TY�<    �         "      %     create :post, markdown: "**hi**"5�_�   &   (           '          ����                                                                                                                                                                                                                                                                                                                                                  V        TY�I     �         "      @    post = create :post, created_at: Time.parse("2001-12-12 20")5�_�   '   )           (          ����                                                                                                                                                                                                                                                                                                                                                  V        TY�I     �         "      ;    = create :post, created_at: Time.parse("2001-12-12 20")5�_�   (   *           )          ����                                                                                                                                                                                                                                                                                                                                                  V        TY�I    �         "      :     create :post, created_at: Time.parse("2001-12-12 20")5�_�   )   +           *   !       ����                                                                                                                                                                                                                                                                                                                                                             TZ�     �   !   $   #        �   !   #   "    5�_�   *   ,           +   "        ����                                                                                                                                                                                                                                                                                                                            "          #           V       TZ�    �   !   "            s    5�_�   +   -           ,   !        ����                                                                                                                                                                                                                                                                                                                            "          "           V       TZ�     �   $   &   &        end�   #   %   &          $2�   "   $   &        scenario "$1" do�   "   &   $        sce�   !   $   #        �   !   #   "    5�_�   ,   .           -   "        ����                                                                                                                                                                                                                                                                                                                            %          "           V       TZ�    �   !   "                scenario "" do            end5�_�   -   /           .           ����                                                                                                                                                                                                                                                                                                                                                             TZ�+     �   
              5�_�   .   0           /           ����                                                                                                                                                                                                                                                                                                                                                             TZ�,    �                 5�_�   /   1           0           ����                                                                                                                                                                                                                                                                                                                                                             TZ�.     �                 5�_�   0   2           1           ����                                                                                                                                                                                                                                                                                                                                                             TZ�/     �                s5�_�   1   3           2          ����                                                                                                                                                                                                                                                                                                                                                             TZ�1     �             5�_�   2   4           3          ����                                                                                                                                                                                                                                                                                                                                                             TZ�2    �              5�_�   3   5           4           ����                                                                                                                                                                                                                                                                                                                                                             TZ�D     �       #   "        �       "   !    5�_�   4   6           5   "       ����                                                                                                                                                                                                                                                                                                                                                             TZ�K     �   !   #   #        def create_and_visit_post()5�_�   5   7           6   "   '    ����                                                                                                                                                                                                                                                                                                                                                             TZ�O     �   !   #   #      )  def create_and_visit_post(options = {})5�_�   6   8           7   "   '    ����                                                                                                                                                                                                                                                                                                                                                             TZ�O     �   "   $   $          �   "   $   #    5�_�   7   9           8   "       ����                                                                                                                                                                                                                                                                                                                                                             TZ�P     �   "   $   %          �   "   $   $    5�_�   8   :           9   #       ����                                                                                                                                                                                                                                                                                                                                                             TZ�W     �   #   %   &          �   #   %   %    5�_�   9   ;           :   $       ����                                                                                                                                                                                                                                                                                                                                                             TZ�Z     �   #   %   &          bis5�_�   :   <           ;   "       ����                                                                                                                                                                                                                                                                                                                                                             TZ�^    �   !   #   &      )  def create_and_visit_post(options = {})5�_�   ;   =           <          ����                                                                                                                                                                                                                                                                                                                            "          "          v       TZ�k     �         '          �         &    5�_�   <   >           =   
        ����                                                                                                                                                                                                                                                                                                                            
                    V       TZ�n    �   	   
              post = create :post       visit root_path5�_�   =   ?           >   
       ����                                                                                                                                                                                                                                                                                                                            
          
          V       TZ�~     �   	      %          create_post_and_visit_root5�_�   >   @           ?   
       ����                                                                                                                                                                                                                                                                                                                            
          
          V       TZ�     �   	      %           create_post_and_visit_root()5�_�   ?   A           @   
   '    ����                                                                                                                                                                                                                                                                                                                            
          
          V       TZ��     �   	      %      )    create_post_and_visit_root(title: "")5�_�   @   B           A      +    ����                                                                                                                                                                                                                                                                                                                            
          
          V       TZ��     �         %      +    expect(page).to have_content post.title5�_�   A   C           B      "    ����                                                                                                                                                                                                                                                                                                                            
          
          V       TZ��    �         %      #    expect(page).to have_content ""5�_�   B   D           C          ����                                                                                                                                                                                                                                                                                                                            
          
          V       TZ��     �         &          �         %    5�_�   C   E           D          ����                                                                                                                                                                                                                                                                                                                            
          
          V       TZ��     �         &           create_post_and_visit_root()5�_�   D   F           E      *    ����                                                                                                                                                                                                                                                                                                                            
          
          V       TZ��     �         &      ,    create_post_and_visit_root(markdown: "")5�_�   E   G           F      *    ����                                                                                                                                                                                                                                                                                                                            
          
          V       TZ��     �         &      ,    create_post_and_visit_root(markdown: "")5�_�   F   H           G      $    ����                                                                                                                                                                                                                                                                                                                                         $       V   /    TZ��   " �                $    create :post, markdown: "**hi**"       visit root_path5�_�   G   I           H   	       ����                                                                                                                                                                                                                                                                                                                            	   '       	          v       TZ��   $ �      
   $      +  scenario "sees the posts", focus: true do5�_�   H   J           I          ����                                                                                                                                                                                                                                                                                                                                                v       TZ�b     �         $      9    create :post, created_at: Time.parse("2001-12-12 20")5�_�   I   K           J          ����                                                                                                                                                                                                                                                                                                                                                v       TZ�e   ) �                    visit root_path5�_�   J   L           K           ����                                                                                                                                                                                                                                                                                                                                                  V        T_��     �      !   #          create :post, options5�_�   K   M           L            ����                                                                                                                                                                                                                                                                                                                                                  V        T_��     �      !   #      !    create :post, options.merge{}5�_�   L   N           M            ����                                                                                                                                                                                                                                                                                                                                                  V        T_��     �      !   #      !    create :post, options.merge()5�_�   M   O           N       !    ����                                                                                                                                                                                                                                                                                                                                                  V        T_��     �      !   #      #    create :post, options.merge({})5�_�   N   P           O       '    ����                                                                                                                                                                                                                                                                                                                                                  V        T_��     �      !   #      *    create :post, options.merge({ html: })5�_�   O   Q           P       A    ����                                                                                                                                                                                                                                                                                                                                                  V        T_��     �      !   #      E    create :post, options.merge({ html: MarkdownParser.new.parse() })5�_�   P   R           Q       I    ����                                                                                                                                                                                                                                                                                                                                                  V        T_��   + �      !   #      N    create :post, options.merge({ html: MarkdownParser.new.parse(options[]) })5�_�   Q   S           R           ����                                                                                                                                                                                                                                                                                                                                                  V        T_��   , �      !   #      W    create :post, options.merge({ html: MarkdownParser.new.parse(options[:markdown]) })5�_�   R               S          ����                                                                                                                                                                                                                                                                                                                                                 V       T_��   0 �                0  scenario "sees the parsed markdown as html" do   2    create_post_and_visit_root(markdown: "**hi**")       #    within "article.post strong" do   '      expect(page).to have_content "hi"       end     end    5�_�   !   #       $   "           ����                                                                                                                                                                                                                                                                                                                               5          )       v   )    TY��     �              5�_�   "               #           ����                                                                                                                                                                                                                                                                                                                               5          )       v   )    TY��     �   
           5�_�                    	       ����                                                                                                                                                                                                                                                                                                                               5          )       v   )    TY��     �   	   
   $    �      
   $      +  scenario "sees the posts" , focus: truedo5��