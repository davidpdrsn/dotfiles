Vim�UnDo� ��e�-��f���}|��x8�>�1֗�        include WithCacheVersion                             Uz�   	 _�                             ����                                                                                                                                                                                                                                                                                                                                                             Uz�     �             5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             Uz�     �                  �             5�_�                            ����                                                                                                                                                                                                                                                                                                                                                             Uz�     �              5�_�                            ����                                                                                                                                                                                                                                                                                                                                                             Uz�     �         !       5�_�                    
   	    ����                                                                                                                                                                                                                                                                                                                                                             Uz�     �   	      $        POSITIONS =       {5�_�                           ����    
   	                                                                                                                                                                                                                                                                                                                                         V        Uz     �   
      #      D      "4-4-2" => { 1 => :st, 2 => :st, 3 => :lm, 4 => :cm, 5 => :cm,   R                   6 => :rm, 7 => :lb, 8 => :cb, 9 => :cb, 10 => :rb, 11 => :gk },       D      "4-3-3" => { 1 => :lw, 2 => :st, 3 => :rw, 4 => :cm, 5 => :cm,   R                   6 => :cm, 7 => :lb, 8 => :cb, 9 => :cb, 10 => :rb, 11 => :gk },       D      "3-5-2" => { 1 => :st, 2 => :st, 3 => :lm, 4 => :cm, 5 => :cm,   R                   6 => :cm, 7 => :rm, 8 => :lb, 9 => :cb, 10 => :rb, 11 => :gk },       D      "4-5-1" => { 1 => :st, 2 => :lm, 3 => :cm, 4 => :am, 5 => :cm,   R                   6 => :rm, 7 => :lb, 8 => :cb, 9 => :cb, 10 => :rb, 11 => :gk },       }           private       
    def in5�_�      	                     ����    
   	                                                                                                                                                                                                                                                                                                                                         V        Uz
     �         %          �         $    �         #        def in5�_�      
           	          ����    
   	                                                                                                                                                                                                                                                                                                                                         V        Uz    �         %          CacheVersion.increment()5�_�   	              
           ����    
   	                                                                                                                                                                                                                                                                                                                                         V        Uz    �                '  after_create :increment_cache_version5�_�   
                         ����    	   	                                                                                                                                                                                                                                                                                                                   
                      V        Uz     �                 5�_�                            ����       	                                                                                                                                                                                                                                                                                                                                        V       Uz!    �                      def increment_cache_version        CacheVersion.increment(self)     end5�_�                            ����       	                                                                                                                                                                                                                                                                                                                                        V       Uz%    �                    	  private5�_�                            ����       	                                                                                                                                                                                                                                                                                                                                        V       Uz*     �                 �             5�_�                           ����    	   	                                                                                                                                                                                                                                                                                                                                        V       Uz/    �             5�_�                          ����    
   	                                                                                                                                                                                                                                                                                                                                        V       UzJ    �                 include WithCacheVersion5�_�                            ����    
   	                                                                                                                                                                                                                                                                                                                                        V       Uz�   	 �                 extend WithCacheVersion5�_�                           ����    
   	                                                                                                                                                                                                                                                                                                                                        V       Uz6    �                 extend WithCacheVersion5�_�                            ����    
   	                                                                                                                                                                                                                                                                                                                                                    Uz      �   
             B    "4-4-2" => { 1 => :st, 2 => :st, 3 => :lm, 4 => :cm, 5 => :cm,�                P                 6 => :rm, 7 => :lb, 8 => :cb, 9 => :cb, 10 => :rb, 11 => :gk },�                O                 "4-3-3" => { 1 => :lw, 2 => :st, 3 => :rw, 4 => :cm, 5 => :cm,�                ]                              6 => :cm, 7 => :lb, 8 => :cb, 9 => :cb, 10 => :rb, 11 => :gk },�                \                              "3-5-2" => { 1 => :st, 2 => :st, 3 => :lm, 4 => :cm, 5 => :cm,�                j                                           6 => :cm, 7 => :rm, 8 => :lb, 9 => :cb, 10 => :rb, 11 => :gk },�                i                                           "4-5-1" => { 1 => :st, 2 => :lm, 3 => :cm, 4 => :am, 5 => :cm,�                w                                                        6 => :rm, 7 => :lb, 8 => :cb, 9 => :cb, 10 => :rb, 11 => :gk },�                  }�                	  private�                  def in�                  end�                  # == Schema Information�                  #�                   # Table name: formations�      !            #�       "          1  #  id   :integer          not null, primary key�   !   #          $  #  name :string(255)      not null�   "   $            #5��