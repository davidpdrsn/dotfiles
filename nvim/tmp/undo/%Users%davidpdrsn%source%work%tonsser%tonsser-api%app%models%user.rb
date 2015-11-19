Vim�UnDo� y�%���wR�{'T�Q^u�O$m���n�7 S   �   "  has_many :likes, ->() { active }                             VL1�   	 _�                     �        ����                                                                                                                                                                                                                                                                                                                                                  V        VJ
�    �               �   class User < ActiveRecord::Base     include FriendlyId     friendly_id :name         acts_as_followable     acts_as_follower       $  enum state: [:normal, :new_season]         belongs_to :primary_position,   +             foreign_key: "primary_pos_id",   #             class_name: "Position"     belongs_to :country     belongs_to :role     has_one :profile     has_many :performance_points     has_many :team_memberships   K  has_many :teams, through: :team_memberships, after_add: ->(user, team) do   6    FeedStoryCreator.create_relevant_story(user, team)     end     has_many :achievements     has_many :notifications   "  has_many :likes, ->() { active }       )  delegate :name, to: :role, prefix: true   B  delegate :name, to: :primary_team, prefix: true, allow_nil: true   B  delegate :slug, to: :primary_team, prefix: true, allow_nil: true       =  after_save { FeedStoryCreator.create_relevant_story(self) }       
  def name       "#{firstname} #{lastname}"     end         def member?(team)       teams.exists?(team.id)     end       #  def shares_team_with?(other_user)   A    teams.includes(:users).flat_map(&:users).include?(other_user)     end         def primary_team   %    find_primary_team || NullTeam.new     end         def primary_team?       find_primary_team.present?     end       
  def role       super || NullRole.new     end       *  # Define goalkeeper?, defensive?, etc...   E  [:goalkeeper, :defensive, :midfield, :attacking].each do |position|       name = "#{position}?"           define_method(name) do   +      return false if primary_position.nil?   !      primary_position.send(name)       end     end          def match_ids_for_primary_team       primary_team.match_ids     end         def profile_views_count       if profile         profile.views.count       else         0       end     end         def followings_count       all_following.count     end         def active_now!   $    update(last_active_at: Time.now)     end         def converted_now!       update(phantom: false)   "    update(converted_at: Time.now)     end       "  def follow(another_user, &block)       super(another_user, &block)   	    touch       another_user.touch     end       *  def stop_following(another_user, &block)       super(another_user, &block)   	    touch       another_user.touch     end       #  def follow_and_maybe_notify(user)       follow(user) do   6      Notifier.new(user).new_follower(user_slug: slug)       end     end         def users_in_same_league       User   %      .joins(team_memberships: :team)   ;      .where(teams: { league_id: teams.pluck(:league_id) })         .where.not(id: id)     end         def admin?       role.name == "admin"     end         def not_member_of_any_teams?       !primary_team?     end       '  def users_in_same_region_and_agegroup   "    UsersInRegionAndAgegroup.find(   /      region_id: primary_team.league.region.id,   3      agegroup_id: primary_team.league.agegroup.id,       ).where.not(id: id)     end         def current_season_start_date   ,    team_in_current_season.season_start_date     end         def current_season_end_date   *    team_in_current_season.season_end_date     end         def team_in_current_season   '    return NullTeam.new if teams.empty?           if primary_team?         primary_team       else   1      teams.order(season_start_date: :desc).first       end     end         def terms_url       UserTermsUrl.new(self).get     end         def agegroup       primary_team.agegroup     end         def region       primary_team.region     end         def team_mates   5    primary_team.users.reject { |mate| mate == self }     end       	  private         def find_primary_team       team_memberships          .where(primary_team: true)         .order(created_at: :desc)         .first         .try(:team)     end   end       # == Schema Information   #   # Table name: users   #   <#  id                :integer          not null, primary key   /#  slug              :string(255)      not null   /#  facebook_id       :string(255)      not null   !#  email             :string(255)   /#  firstname         :string(255)      not null   /#  lastname          :string(255)      not null   #  primary_pos_id    :integer   /#  phantom           :boolean          not null   #  converted_at      :datetime   /#  country_id        :integer          not null   /#  role_id           :integer          not null   #  last_active_at    :datetime   #  created_at        :datetime   #  updated_at        :datetime   #  device_token      :string   ;#  state             :integer          default(0), not null   #  terms_accepted_at :date   #   	# Indexes   #   -#  index_users_on_email        (email) UNIQUE   3#  index_users_on_facebook_id  (facebook_id) UNIQUE   ,#  index_users_on_slug         (slug) UNIQUE   #5�_�                   �        ����                                                                                                                                                                                                                                                                                                                                                  V        VJr     �               �   class User < ActiveRecord::Base     include FriendlyId     friendly_id :name         acts_as_followable     acts_as_follower       $  enum state: [:normal, :new_season]         belongs_to :primary_position,   +             foreign_key: "primary_pos_id",   #             class_name: "Position"     belongs_to :country     belongs_to :role     has_one :profile     has_many :performance_points     has_many :team_memberships   K  has_many :teams, through: :team_memberships, after_add: ->(user, team) do   6    FeedStoryCreator.create_relevant_story(user, team)     end     has_many :achievements     has_many :notifications   "  has_many :likes, ->() { active }       )  delegate :name, to: :role, prefix: true   B  delegate :name, to: :primary_team, prefix: true, allow_nil: true   B  delegate :slug, to: :primary_team, prefix: true, allow_nil: true       =  after_save { FeedStoryCreator.create_relevant_story(self) }       
  def name       "#{firstname} #{lastname}"     end         def member?(team)       teams.exists?(team.id)     end       #  def shares_team_with?(other_user)   A    teams.includes(:users).flat_map(&:users).include?(other_user)     end         def primary_team   %    find_primary_team || NullTeam.new     end         def primary_team?       find_primary_team.present?     end       
  def role       super || NullRole.new     end       *  # Define goalkeeper?, defensive?, etc...   E  [:goalkeeper, :defensive, :midfield, :attacking].each do |position|       name = "#{position}?"           define_method(name) do   +      return false if primary_position.nil?   !      primary_position.send(name)       end     end          def match_ids_for_primary_team       primary_team.match_ids     end         def profile_views_count       if profile         profile.views.count       else         0       end     end         def followings_count       all_following.count     end         def active_now!   $    update(last_active_at: Time.now)     end         def converted_now!       update(phantom: false)   "    update(converted_at: Time.now)     end       "  def follow(another_user, &block)       super(another_user, &block)   	    touch       another_user.touch     end       *  def stop_following(another_user, &block)       super(another_user, &block)   	    touch       another_user.touch     end       #  def follow_and_maybe_notify(user)       follow(user) do   6      Notifier.new(user).new_follower(user_slug: slug)       end     end         def users_in_same_league       User   %      .joins(team_memberships: :team)   ;      .where(teams: { league_id: teams.pluck(:league_id) })         .where.not(id: id)     end         def admin?       role.name == "admin"     end         def not_member_of_any_teams?       !primary_team?     end       '  def users_in_same_region_and_agegroup   "    UsersInRegionAndAgegroup.find(   /      region_id: primary_team.league.region.id,   3      agegroup_id: primary_team.league.agegroup.id,       ).where.not(id: id)     end         def current_season_start_date   ,    team_in_current_season.season_start_date     end         def current_season_end_date   *    team_in_current_season.season_end_date     end         def team_in_current_season   '    return NullTeam.new if teams.empty?           if primary_team?         primary_team       else   1      teams.order(season_start_date: :desc).first       end     end         def terms_url       UserTermsUrl.new(self).get     end         def agegroup       primary_team.agegroup     end         def region       primary_team.region     end         def team_mates   5    primary_team.users.reject { |mate| mate == self }     end       	  private         def find_primary_team   0    Rails.cache.fetch([self, "primary_team"]) do         teams   !        .joins(:team_memberships)   8        .where(team_memberships: { primary_team: true })   2        .order("team_memberships.created_at DESC")           .first       end     end   end       # == Schema Information   #   # Table name: users   #   <#  id                :integer          not null, primary key   /#  slug              :string(255)      not null   /#  facebook_id       :string(255)      not null   !#  email             :string(255)   /#  firstname         :string(255)      not null   /#  lastname          :string(255)      not null   #  primary_pos_id    :integer   /#  phantom           :boolean          not null   #  converted_at      :datetime   /#  country_id        :integer          not null   /#  role_id           :integer          not null   #  last_active_at    :datetime   #  created_at        :datetime   #  updated_at        :datetime   #  device_token      :string   ;#  state             :integer          default(0), not null   #  terms_accepted_at :date   #   	# Indexes   #   -#  index_users_on_email        (email) UNIQUE   3#  index_users_on_facebook_id  (facebook_id) UNIQUE   ,#  index_users_on_slug         (slug) UNIQUE   #5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             VJ&     �         �    �         �    5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             VJ&     �         �      =  after_save { FeedStoryCreator.create_relevant_story(self) }5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             VJ&    �         �      ?  after_create { FeedStoryCreator.create_relevant_story(self) }5�_�      	                     ����                                                                                                                                                                                                                                                                                                                               %          /       v   /    VJ(v    �                2  after_create { SearcherWithCaching.clear_cache }5�_�      
           	          ����                                                                                                                                                                                                                                                                                                                               %          /       v   /    VJ(z    �                4  # after_create { SearcherWithCaching.clear_cache }5�_�   	              
          ����                                                                                                                                                                                                                                                                                                                               %          /       v   /    VJ(�    �                2  after_create { SearcherWithCaching.clear_cache }5�_�   
                         ����                                                                                                                                                                                                                                                                                                                                                  V        VL1�     �         �    �         �    5�_�                           ����                                                                                                                                                                                                                                                                                                                                                  V        VL1�    �         �      "  has_many :likes, ->() { active }5�_�                            ����                                                                                                                                                                                                                                                                                                                                                  V        VL1�   	 �         �        has_many :warnings5�_�                   �        ����                                                                                                                                                                                                                                                                                                                                                  V        VJ
�     �   �   �        5�_�                     �       ����                                                                                                                                                                                                                                                                                                                                                  V        VJ
�    �   �   �        5��