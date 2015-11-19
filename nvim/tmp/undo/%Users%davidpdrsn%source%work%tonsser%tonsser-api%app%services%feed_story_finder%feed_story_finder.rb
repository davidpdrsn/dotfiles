Vim�UnDo� �@�ĝ��t]7��.�1�I��Ǣҋn��a��   w   -  class FeedStoryFinder < BaseFeedStoryFinder                             U�>    _�                            ����                                                                                                                                                                                                                                                                                                                                                             U�5     �          v       �          u    5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             U�6     �          x      2module `!p snip.rv = class_name() + inheritance()`�         x        $0�          v      module5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             U�8     �                  5�_�                            ����                                                                                                                                                                                                                                                                                                                                                             U�8     �             u   +class FeedStoryFinder < BaseFeedStoryFinder   #  def find_achievements_for_profile   F    FeedStory.where(is_achievement_from_self).order(created_at: :desc)     end         def find_awards_for_profile   J    FeedStory.where(is_mvp_team_player_from_self).order(created_at: :desc)     end       7  def find_achievements_for_match_and_team(match, team)   %    join_query = <<-SQL.strip_heredoc   ,      JOIN achievements ON achievements.id =   2        feed_stories.triggering_entity_id::INTEGER       SQL           FeedStory         .joins(join_query)   =      .where(is_achievement_from_match_and_team(match, team))         .order(created_at: :desc)     end       %  def find_latest_achievements(limit)       FeedStory   $      .where(type_is("achievement"))         .order(created_at: :desc)         .limit(limit)     end       	  private         def find_stories_query        query = user_follows_subject   $      .or(type_is("joined-tonsser"))   +      .or(is_match_where_user_participated)   (      .or(is_achievement_from_following)   #      .or(is_achievement_from_self)   '      .or(is_achievement_from_teammate)   '      .or(is_mvp_team_player_from_self)   +      .or(is_mvp_team_player_from_teammate)       "    # REF: Remove this conditional   O    #      Can be done by introducing new null objects so the ened for ensuring   3    #      that the user has a team isn't necessary   =    # NOTE: Refactoring this will require adding tests for it       if @user.primary_team?   A      query = query.or(is_mvp_team_from_same_region_and_agegroup)       end       	    query     end       &  def is_match_where_user_participated       type_is("match")   M      .and(table[:triggering_entity_id].in(@user.match_ids_for_primary_team))     end         def user_follows_subject       table[:subject_slug]         .in(following_slugs)     end       "  def is_achievement_from_teammate       type_is("achievement")   3      .and(table[:subject_slug].in(teammate_slugs))     end         def is_achievement_from_self       type_is("achievement")   /      .and(table[:subject_slug].eq(@user.slug))     end       #  def is_achievement_from_following       type_is("achievement")   4      .and(table[:subject_slug].in(following_slugs))     end       "  def is_mvp_team_player_from_self       type_is("mvp-team-player")   /      .and(table[:subject_slug].eq(@user.slug))     end       &  def is_mvp_team_player_from_teammate       type_is("mvp-team-player")   3      .and(table[:subject_slug].in(teammate_slugs))     end       /  def is_mvp_team_from_same_region_and_agegroup   +    user_league = @user.primary_team.league   (    agegroup = user_league.agegroup.name       5    agegroup_condition = Arel::Nodes::SqlLiteral.new(         <<-SQL   I        "feed_stories".meta_data->'mvp_team'->>'agegroup' = '#{agegroup}'   	      SQL       )           type_is("mvp-team")   G      .and(table[:triggering_entity_id].eq(user_league.region.id.to_s))         .and(agegroup_condition)     end       5  def is_achievement_from_match_and_team(match, team)   1    achievements = Arel::Table.new(:achievements)           type_is("achievement")   =      .and(table[:subject_slug].in(team.players.map(&:slug)))   0      .and(achievements[:match_id].eq(match.id))     end         def following_slugs   #    @user.all_following.map(&:slug)     end         def teammate_slugs   (    @user.primary_team.users.map(&:slug)     end   end5�_�                            ����                                                                                                                                                                                                                                                                                                                                                             U�9     �      w       �             5�_�                            ����                                                                                                                                                                                                                                                                                                                                                            U�9     �   u   w          end�   t   v            end�   s   u          (    @user.primary_team.users.map(&:slug)�   r   t            def teammate_slugs�   p   r            end�   o   q          #    @user.all_following.map(&:slug)�   n   p            def following_slugs�   l   n            end�   k   m          0      .and(achievements[:match_id].eq(match.id))�   j   l          =      .and(table[:subject_slug].in(team.players.map(&:slug)))�   i   k              type_is("achievement")�   g   i          1    achievements = Arel::Table.new(:achievements)�   f   h          5  def is_achievement_from_match_and_team(match, team)�   d   f            end�   c   e                .and(agegroup_condition)�   b   d          G      .and(table[:triggering_entity_id].eq(user_league.region.id.to_s))�   a   c              type_is("mvp-team")�   _   a              )�   ^   `          	      SQL�   \   ^                <<-SQL�   [   ]          5    agegroup_condition = Arel::Nodes::SqlLiteral.new(�   Y   [          (    agegroup = user_league.agegroup.name�   X   Z          +    user_league = @user.primary_team.league�   W   Y          /  def is_mvp_team_from_same_region_and_agegroup�   U   W            end�   T   V          3      .and(table[:subject_slug].in(teammate_slugs))�   S   U              type_is("mvp-team-player")�   R   T          &  def is_mvp_team_player_from_teammate�   P   R            end�   O   Q          /      .and(table[:subject_slug].eq(@user.slug))�   N   P              type_is("mvp-team-player")�   M   O          "  def is_mvp_team_player_from_self�   K   M            end�   J   L          4      .and(table[:subject_slug].in(following_slugs))�   I   K              type_is("achievement")�   H   J          #  def is_achievement_from_following�   F   H            end�   E   G          /      .and(table[:subject_slug].eq(@user.slug))�   D   F              type_is("achievement")�   C   E            def is_achievement_from_self�   A   C            end�   @   B          3      .and(table[:subject_slug].in(teammate_slugs))�   ?   A              type_is("achievement")�   >   @          "  def is_achievement_from_teammate�   <   >            end�   ;   =                .in(following_slugs)�   :   <              table[:subject_slug]�   9   ;            def user_follows_subject�   7   9            end�   6   8          M      .and(table[:triggering_entity_id].in(@user.match_ids_for_primary_team))�   5   7              type_is("match")�   4   6          &  def is_match_where_user_participated�   2   4            end�   1   3          	    query�   /   1              end�   .   0          A      query = query.or(is_mvp_team_from_same_region_and_agegroup)�   -   /              if @user.primary_team?�   ,   .          =    # NOTE: Refactoring this will require adding tests for it�   +   -          3    #      that the user has a team isn't necessary�   *   ,          O    #      Can be done by introducing new null objects so the ened for ensuring�   )   +          "    # REF: Remove this conditional�   '   )          +      .or(is_mvp_team_player_from_teammate)�   &   (          '      .or(is_mvp_team_player_from_self)�   %   '          '      .or(is_achievement_from_teammate)�   $   &          #      .or(is_achievement_from_self)�   #   %          (      .or(is_achievement_from_following)�   "   $          +      .or(is_match_where_user_participated)�   !   #          $      .or(type_is("joined-tonsser"))�       "               query = user_follows_subject�      !            def find_stories_query�                	  private�                  end�                      .limit(limit)�                      .order(created_at: :desc)�                $      .where(type_is("achievement"))�                    FeedStory�                %  def find_latest_achievements(limit)�                  end�                      .order(created_at: :desc)�                =      .where(is_achievement_from_match_and_team(match, team))�                      .joins(join_query)�                    FeedStory�                    SQL�                %    join_query = <<-SQL.strip_heredoc�   
             7  def find_achievements_for_match_and_team(match, team)�      
            end�      	          J    FeedStory.where(is_mvp_team_player_from_self).order(created_at: :desc)�                  def find_awards_for_profile�                  end�                F    FeedStory.where(is_achievement_from_self).order(created_at: :desc)�                #  def find_achievements_for_profile�                +class FeedStoryFinder < BaseFeedStoryFinder5�_�                           ����                                                                                                                                                                                                                                                                                                                                                            U�;     �         w      -  class FeedStoryFinder < BaseFeedStoryFinder5�_�                        %    ����                                                                                                                                                                                                                                                                                                                                                            U�=    �         w      %  class Default < BaseFeedStoryFinder5��