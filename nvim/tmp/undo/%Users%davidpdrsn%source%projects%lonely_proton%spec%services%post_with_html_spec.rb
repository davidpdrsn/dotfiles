Vim�UnDo� 5�e�{`ĢAs0R��q��"D��92:�-mB��                                       T��!    _�                        	    ����                                                                                                                                                                                                                                                                                                                                                             T���    �               !describe ParseMarkdownObserver do5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             T���     �               0    observer = ParseMarkdownObserver.new(parser)5�_�                            ����                                                                                                                                                                                                                                                                                                                                                             T���    �               '    observer = PostWithHtml.new(parser)5�_�                            ����                                                                                                                                                                                                                                                                                                                                                V   %    T���     �                   observer.saved(post)�                    observer.updated(post)5�_�                           ����                                                                                                                                                                                                                                                                                                                                                V   %    T���     �               -    observer = PostWithHtml.new(post, parser)5�_�                           ����                                                                                                                                                                                                                                                                                                                                                V   %    T���     �               )    post = PostWithHtml.new(post, parser)5�_�                            ����                                                                                                                                                                                                                                                                                                                                                V   %    T���    �                5�_�      	                     ����                                                                                                                                                                                                                                                                                                                                                V       T��    �               :    expect(post).to have_received(:html=).with(html).twice5�_�      
           	      >    ����                                                                                                                                                                                                                                                                                                                                                V       T��   
 �               >    expect(raw_post).to have_received(:html=).with(html).twice5�_�   	              
      
    ����                                                                                                                                                                                                                                                                                                                               
                 v       T��    �               9    post = double("post", markdown: markdown, save: true)�      	         "    allow(post).to receive(:html=)5�_�   
                    4    ����                                                                                                                                                                                                                                                                                                                                                v       T��     �               A    raw_post = double("raw_post", markdown: markdown, save: true)5�_�                        4    ����                                                                                                                                                                                                                                                                                                                                                v       T��     �      	             �      	       5��