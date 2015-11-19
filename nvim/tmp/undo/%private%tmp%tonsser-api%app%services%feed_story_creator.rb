Vim�UnDo� ��m��O�e��y����y���=�n�Z���   �                                   VC.�    _�                             ����                                                                                                                                                                                                                                                                                                                                                  V        VB>�     �                 A# This class is responsible for taking in an array of new objects   9# and figuring out which feed stories need to be created.   F# This could also be done in the models, but would lead to duplication   (# and lots of conditionals in callbacks.5�_�                            ����                                                                                                                                                                                                                                                                                                                                                  V        VB>�    �                A        # The triggering_entity_id is set to the id of the league   K        # to make it easier to find the relevant stories in FeedStoryFinder5�_�                            ����                                                                                                                                                                                                                                                                                                                                                  V        VB?+     �               �   class FeedStoryCreator     class << self   $    def create_relevant_story(*args)   :      types = args.map { |arg| arg.class.name.underscore }         @new_stories = []   +      send("for_#{types.join('_')}", *args)         @new_stories       end       %    def destroy_relevant_story(*args)   O      stories = FeedStory.where(triggering_entity_id: args.map(&:id).join("-"))       &      if args.first.is_a?(Achievement)   I        update_feed_stories_of_destroyed_achievement(args.first, stories)   
      else            destroy_stories(stories)   	      end       end           private           def for_mvp_team(team)         build_story do |builder|   ,        builder.feed_story_type = "mvp-team"   (        builder.subject_slug = team.slug   :        add_serialized_attributes(team, builder.meta_data)       >        builder.triggering_entity_id = team.find_group.id.to_s   	      end       .      team.mvp_team_players.each do |mvp_user|           user = mvp_user.user                build_story do |builder|   5          builder.feed_story_type = "mvp-team-player"   *          builder.subject_slug = user.slug   E          add_serialized_attributes_for_user(user, builder.meta_data)   <          add_serialized_attributes(team, builder.meta_data)           end   	      end       end           def for_match(match)         build_story do |builder|   )        builder.subject_slug = match.slug   (        builder.feed_story_type = :match   /        builder.triggering_entity_id = match.id       ;        add_serialized_attributes(match, builder.meta_data)   	      end       end           def for_user(user)   3      if should_trigger_joined_tonsser_story?(user)            build_story do |builder|   *          builder.subject_slug = user.slug   4          builder.feed_story_type = "joined-tonsser"       E          add_serialized_attributes_for_user(user, builder.meta_data)           end   	      end       end       !    def for_user_team(user, team)         build_story do |builder|   /        builder.feed_story_type = "joined-team"   (        builder.subject_slug = user.slug       C        add_serialized_attributes_for_user(user, builder.meta_data)   :        add_serialized_attributes(team, builder.meta_data)   	      end       end       $    def for_achievement(achievement)   B      if similar_achievement = achievement_similar_to(achievement)   &        story_to_join_into = FeedStory   @          .find_by(triggering_entity_id: similar_achievement.id)       C        join_achievement_and_story(achievement, story_to_join_into)   
      else   4        build_new_story_for_achievement(achievement)   	      end       end       4    def build_new_story_for_achievement(achievement)         build_story do |builder|   4        builder.subject_slug = achievement.user.slug   .        builder.feed_story_type = :achievement   :        builder.triggering_entity_id = achievement.id.to_s       O        add_serialized_attributes_for_user(achievement.user, builder.meta_data)                if achievement.match_id?   I          add_serialized_attributes(achievement.match, builder.meta_data)   :          update_user_team(achievement, builder.meta_data)           end   O        builder.meta_data.achievements = meta_data_for_achievement(achievement)   	      end       end       0    def update_user_team(achievement, meta_data)   L      return if match_played_for_primary_team?(achievement.match, meta_data)       P      user_match_team = (achievement.user.teams & achievement.match.teams).first   N      team_json = MiniTeamSerializer.new(user_match_team, root: false).as_json   %      meta_data.user.team = team_json       end       8    def match_played_for_primary_team?(match, meta_data)   1      user_team_slug = meta_data.user.team[:slug]   5      match_team_slugs = match.teams.flat_map(&:slug)       /      match_team_slugs.include?(user_team_slug)       end       6    def join_achievement_and_story(achievement, story)   =      new_meta_data = AchievementFeedStoryMetaDataMerger.new(           story.meta_data,   A        "achievements" => meta_data_for_achievement(achievement),         ).merge   D      story.update!(meta_data: story.meta_data.merge(new_meta_data))       end       +    def achievement_similar_to(achievement)         Achievement   &        .where.not(id: achievement.id)           .where.not(match: nil)   @        .where(user: achievement.user, match: achievement.match)           .first       end       5    def update_feed_stories_of_destroyed_achievement(         destroyed_achievement,         stories       )   I      related_achievement = achievement_similar_to(destroyed_achievement)       M      if destroyed_achievement_that_triggered_stories?(destroyed_achievement,   K                                                       related_achievement,   ?                                                       stories)   O        stories.first.update(triggering_entity_id: related_achievement.id.to_s)   
      else            destroy_stories(stories)   	      end       D      update_related_achievement_feed_stories(destroyed_achievement)       end       6    def destroyed_achievement_that_triggered_stories?(         destroyed_achievement,         achievement,         stories       )         stories.present? &&           achievement.present? &&   K        destroyed_achievement.id.to_s == stories.first.triggering_entity_id       end            def destroy_stories(stories)         stories.each do |story|           story.destroy               Notification   A          .where("meta_data->>'feed_story_slug' = ?", story.slug)             .destroy_all   	      end       end       .    def meta_data_for_achievement(achievement)   *      json = HashBuilder.new.tap do |data|   O        achievement.meta_data.each { |key, value| data.send("#{key}=", value) }   5        data.achievement_type = achievement.type_name         end.as_json             [json]       end       <    def update_related_achievement_feed_stories(achievement)   ?      related_achievement = achievement_similar_to(achievement)   (      return if related_achievement.nil?   P      new_achievement_meta_data = meta_data_for_achievement(related_achievement)   M      story = FeedStory.find_by(triggering_entity_id: related_achievement.id)             new_meta_data = story           .meta_data   ;        .merge("achievements" => new_achievement_meta_data)       -      story.update!(meta_data: new_meta_data)       end       1    def add_serialized_attributes(obj, meta_data)         type = obj.class.name   >      json = serializer_for(obj).new(obj, root: false).as_json         json.each do |key, value|   >        meta_data.send(type.underscore).send("#{key}=", value)   	      end       end           def serializer_for(obj)   .      serializer = obj.active_model_serializer   G      serializer = TeamMatchSerializer if serializer == MatchSerializer   E      serializer = MiniUserSerializer if serializer == UserSerializer   E      serializer = MiniTeamSerializer if serializer == TeamSerializer         serializer       end       ;    def add_serialized_attributes_for_user(user, meta_data)   0      add_serialized_attributes(user, meta_data)   *      meta_data.user.role = user.role_name       end       2    def should_trigger_joined_tonsser_story?(user)   A      (user.role.name == "scout" || user.role.name == "agent") &&   *        has_no_joined_tonsser_story?(user)       end       *    def has_no_joined_tonsser_story?(user)         FeedStory.where(            subject_slug: user.slug,   *        feed_story_type: "joined-tonsser",         ).blank?       end           def build_story         builder = HashBuilder.new         yield builder         options = builder.as_json   0      @new_stories << FeedStory.create!(options)       end     end   end5�_�                             ����                                                                                                                                                                                                                                                                                                                                                 V       VC.�    �             
   %    def destroy_relevant_story(*args)   O      stories = FeedStory.where(triggering_entity_id: args.map(&:id).join("-"))       &      if args.first.is_a?(Achievement)   I        update_feed_stories_of_destroyed_achievement(args.first, stories)   
      else            destroy_stories(stories)   	      end       end    5��