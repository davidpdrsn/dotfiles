Vim�UnDo� �����E���x�ۚ�x��m����Ξ�B�               "                       Tg�t    _�                             ����                                                                                                                                                                                                                                                                                                                                                             Tg��    �                  # == Schema Information   #   # Table name: tags   #   /#  id   :integer          not null, primary key   #  name :string(255)   #       class Tag < ActiveRecord::Base   D  has_and_belongs_to_many :posts, -> { where.not published_at: nil }       !  validates :name, presence: true   #  validates :name, uniqueness: true       2  scope :find_for_ids, -> (ids) { where(id: ids) }         def to_param   %    [id, name.parameterize].join("-")     end         def ==(another_tag)        # TODO: Remove DIP violation   %    if another_tag.is_a? TagWithDomId         another_tag == self       else         super(another_tag)       end     end   end5�_�                           ����                                                                                                                                                                                                                                                                                                                                                v       Tg�I    �                     another_tag == self�               %    if another_tag.is_a? TagWithDomId�                 def ==(another_tag)�                     super(another_tag)5�_�                            ����                                                                                                                                                                                                                                                                                                                                                v       Tg�R    �      
          �      
       5�_�                       1    ����                                                                                                                                                                                                                                                                                                                                                v       Tgɛ     �   
            D  has_and_belongs_to_many :posts, -> { where.not published_at: nil }5�_�                       B    ����                                                                                                                                                                                                                                                                                                                                                v       Tgɟ     �   
            D  has_and_belongs_to_many :posts, -> { where.not(published_at: nil }5�_�                       C    ����                                                                                                                                                                                                                                                                                                                                                v       Tgɟ    �   
            F  has_and_belongs_to_many :posts, -> { where.not(published_at: nil() }5�_�                       D    ����                                                                                                                                                                                                                                                                                                                                                v       Tgɫ    �   
            F  has_and_belongs_to_many :posts, -> { where.not(published_at: nil). }5�_�      	                 D    ����                                                                                                                                                                                                                                                                                                                                                v       Tgɶ    �   
            ^  has_and_belongs_to_many :posts, -> { where.not(published_at: nil).recently_published_first }5�_�      
           	      "    ����                                                                                                                                                                                                                                                                                                                               "          #       v   #    Tg��   	 �   
            D  has_and_belongs_to_many :posts, -> { where.not(published_at: nil).5�_�   	              
      *    ����                                                                                                                                                                                                                                                                                                                               "          #       v   #    Tg��     �   
            H  has_and_belongs_to_many :posts, lambda { where.not(published_at: nil).5�_�   
                        ����                                                                                                                                                                                                                                                                                                                               "          #       v   #    Tg��     �                A                                       recently_published_first }5�_�                           ����                                                                                                                                                                                                                                                                                                                               "          #       v   #    Tg��   
 �                    recently_published_first }5�_�                            ����                                                                                                                                                                                                                                                                                                                               "          #       v   #    Tg��     �         !          recently_published_first5�_�                           ����                                                                                                                                                                                                                                                                                                                               "          #       v   #    Tg��     �         !            recently_published_first5�_�                       !    ����                                                                                                                                                                                                                                                                                                                               "          #       v   #    Tg��     �         !      !    where.not(published_at: nil).5�_�                           ����                                                                                                                                                                                                                                                                                                                               "          #       v   #    Tg��    �         !          .recently_published_first5�_�                       "    ����                                                                                                                                                                                                                                                                                                                               "          #       v   #    Tg�;    �   
      !      *  has_and_belongs_to_many :posts, lambda {5�_�                            ����                                                                                                                                                                                                                                                                                                                               "          #       v   #    Tg�?     �         !           where.not(published_at: nil)5�_�                           ����                                                                                                                                                                                                                                                                                                                               "          #       v   #    Tg�A    �         !            .recently_published_first5�_�                           ����                                                                                                                                                                                                                                                                                                                               "          #       v   #    Tg�     �         !            recently_published_first5�_�                           ����                                                                                                                                                                                                                                                                                                                               "          #       v   #    Tgπ     �         !          .recently_published_first5�_�                       !    ����                                                                                                                                                                                                                                                                                                                               "          #       v   #    Tgρ     �         !      !    where.not(published_at: nil).5�_�                       "    ����                                                                                                                                                                                                                                                                                                                               "          #       v   #    Tgσ    �   
      !      &  has_and_belongs_to_many :posts, -> {5�_�                     	        ����                                                                                                                                                                                                                                                                                                                               "          #       v   #    Tg�s    �      	          # Tag model5��