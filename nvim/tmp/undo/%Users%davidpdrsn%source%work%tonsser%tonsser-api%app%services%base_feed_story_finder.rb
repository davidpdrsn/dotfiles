Vim�UnDo� 3>��Ř��� V�_�՝P�/�AԲ$qN��                                      U�~    _�                             ����                                                                                                                                                                                                                                                                                                                                                             U]�/     �                   5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             U]�0    �                1class `!p snip.rv = class_name() + inheritance()`�                 $0�                  class5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             U]�6     �             �             5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             U]�6     �                  5�_�                            ����                                                                                                                                                                                                                                                                                                                                                             U]�8    �                 5�_�                            ����                                                                                                                                                                                                                                                                                                                                                             U]�\     �      
       �             5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             U]�\     �         
    5�_�      	              
        ����                                                                                                                                                                                                                                                                                                                                                             U]�^    �   	   
           5�_�      
           	           ����                                                                                                                                                                                                                                                                                                                                                             U]�v     �      
   
    �         
    5�_�   	              
          ����                                                                                                                                                                                                                                                                                                                                                             U]�w     �             5�_�   
                 
        ����                                                                                                                                                                                                                                                                                                                                                             U]�x     �   	   
           5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             U]�z    �      	         P    FeedStory.where(query_for_finding_relevant_stories).order(created_at: :desc)5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             U]�~    �      	         3    FeedStory.where(query).order(created_at: :desc)5�_�                            ����                                                                                                                                                                                                                                                                                                                                                             U]Ɩ   
 �             �             5�_�                            ����                                                                                                                                                                                                                                                                                                                                                             U]Ɲ     �             �             5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             U]ƞ     �             5�_�                            ����                                                                                                                                                                                                                                                                                                                                                             U]Ɵ    �                 5�_�                       @    ����                                                                                                                                                                                                                                                                                                                                                             U�x�    �      	         @    FeedStory.where(find_stories_query).order(created_at: :desc)5�_�                      @    ����                                                                                                                                                                                                                                                                                                                                                             U�x�    �      	         H    FeedStory.where(find_stories_query).order(created_at: :desc).take(3)5�_�                      @    ����                                                                                                                                                                                                                                                                                                                                                             U�{F    �      	         @    FeedStory.where(find_stories_query).order(created_at: :desc)5�_�                       I    ����                                                                                                                                                                                                                                                                                                                                                             U�{P     �      	         K    FeedStory.where(find_stories_query).order(created_at: :desc).limit(nil)5�_�                       G    ����                                                                                                                                                                                                                                                                                                                                                             U�{Q    �      	         K    FeedStory.where(find_stories_query).order(created_at: :desc).limit(niw)5�_�                       (    ����                                                                                                                                                                                                                                                                                                                                                             U�{j     �             �                 �             5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             U�{l    �                 def liimt5�_�                            ����                                                                                                                                                                                                                                                                                                                                                             U�{�    �                5�_�                       M    ����                                                                                                                                                                                                                                                                                                                                                             U�|�     �                  class BaseFeedStoryFinder     def initialize(user)       @user = user   !    @table = FeedStory.arel_table     end         def find_stories   M    FeedStory.where(find_stories_query).order(created_at: :desc).limit(limit)     end       	  private       )  attr_reader :user, :base_finder, :table         def limit   N    # Intentionally left blank to provide a configuration point for subclasses     end         def type_is(type)   $    table[:feed_story_type].eq(type)     end         def query_has_results?(query)   %    FeedStory.where(query).count == 0     end   end5�_�                             ����                                                                                                                                                                                                                                                                                                                                                             U�~     �                  class BaseFeedStoryFinder     def initialize(user)       @user = user   !    @table = FeedStory.arel_table     end         def find_stories   M    FeedStory.where(find_stories_query).order(created_at: :desc).limit(limit)     end       	  private       )  attr_reader :user, :base_finder, :table         def limit   N    # Intentionally left blank to provide a configuration point for subclasses     end         def type_is(type)   $    table[:feed_story_type].eq(type)     end         def query_has_results?(query)   %    FeedStory.where(query).count == 0     end   end5�_�                       &    ����                                                                                                                                                                                                                                                                                                                                                             U�x�    �      	         H    FeedStory.where(find_stories_query.take(2)).order(created_at: :desc)5�_�                       F    ����                                                                                                                                                                                                                                                                                                                                                             U�x�    �      	         I    FeedStory.where(find_stories_query).order(created_at: :desc).take(-1)5��