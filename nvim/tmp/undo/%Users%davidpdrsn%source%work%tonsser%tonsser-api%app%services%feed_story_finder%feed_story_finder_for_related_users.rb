Vim�UnDo� Ȃ��Ły�B���>���(  �q��5�8�   +   class FeedStoryFinder                              U��g    _�                            ����                                                                                                                                                                                                                                                                                                                                                             U��\     �          )      $class FeedStoryFinderForRelatedUsers5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             U��^     �         *    5�_�                            ����                                                                                                                                                                                                                                                                                                                                                             U��`     �                 5�_�                    *        ����                                                                                                                                                                                                                                                                                                                                                             U��a     �   *                �   *            5�_�                            ����    +                                                                                                                                                                                                                                                                                                                                                       U��a     �   )   +          end�   (   *            end�   '   )              end�   &   (                  .pluck(:id)�   %   '                  .limit(20)�   $   &          J        .where("(meta_data -> 'user' ->> 'slug') IN (#{slugs_for_query})")�   #   %          .        .where(feed_story_type: "achievement")�   "   $                FeedStory�   !   #          E    Rails.cache.fetch([self.class.name, slugs], expires_in: 1.day) do�      !          A    slugs_for_query = slugs.map { |slug| "'#{slug}'" }.join(", ")�                 #  def find_stories_for_slugs(slugs)�                  end�                    end�                5      find_stories_for_users(range + RANGE_INCREMENT)�                    else�                7      FeedStory.where(id: ids).order(created_at: :desc)�                    if ids.present?�                '    ids = find_stories_for_slugs(slugs)�                    return [] if slugs.nil?�                %    slugs = users.pluck(:slug)[range]�                #  def find_stories_for_users(range)�                  RANGE_INCREMENT = 20�                %  attr_reader :users, :finder_factory�                	  private�   
               end�   	             F    find_stories_for_users(IncrementableRange.new(0..RANGE_INCREMENT))�      
          !    return [] if users.count == 0�      	            def find_stories�                  end�                $    @finder_factory = finder_factory�                    @users = users�                '  def initialize(users, finder_factory)5�_�                            ����    +                                                                                                                                                                                                                                                                                                                                                       U��e     �          +      class FeedStoryFinder5�_�                             ����    +                                                                                                                                                                                                                                                                                                                                                       U��f    �          +      class FeedStoryFinder5��