Vim�UnDo� il�J%�uH�(��2���E�>��ԯ&YD	                                      U/�	    _�                              ����                                                                                                                                                                                                                                                                                                                                                             U/�    �                 # == Schema Information   #   # Table name: lineups   #   7#  id           :integer          not null, primary key   #  formation_id :integer   *#  match_id     :integer          not null   *#  team_id      :integer          not null   #  created_at   :datetime   #  updated_at   :datetime   #   	# Indexes   #   (#  index_lineups_on_match_id  (match_id)   '#  index_lineups_on_team_id   (team_id)   #    5��