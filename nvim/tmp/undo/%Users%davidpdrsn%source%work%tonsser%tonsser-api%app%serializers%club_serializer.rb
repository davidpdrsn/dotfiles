Vim�UnDo� V;�J�w�2�# 0��
�*�/g:�m�}�����~                                      U/�    _�                              ����                                                                                                                                                                                                                                                                                                                                                             U/�    �                 # == Schema Information   #   # Table name: clubs   #   6#  id          :integer          not null, primary key   )#  slug        :string           not null   )#  name        :string           not null   #  city        :string   #  zip_code    :string   #  website     :string   )#  country_id  :integer          not null   )#  external_id :string           not null   #  created_at  :datetime   #  updated_at  :datetime   #   	# Indexes   #   *#  index_clubs_on_country_id  (country_id)   +#  index_clubs_on_slug        (slug) UNIQUE   #    5��