Vim�UnDo� ؕdJ���(G�	&����b���iK��   9                 %       %   %   %    T��    _�                             ����                                                                                                                                                                                                                                                                                                                                                             T�d     �                 �             �                   5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             T�d     �                     �             �                   def initialize()5�_�                       ?    ����                                                                                                                                                                                                                                                                                                                                                             T�d     �               @          @hash = ActiveSupport::HashWithIndifferentAccess.new()5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             T�d     �         
              �         	    �                     def method_missing()5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             T�d     �      	                       �      	       �      	   
                  if has_key()5�_�                       3    ����                                                                                                                                                                                                                                                                                                                                                             T�d     �               4                    fetch_possibly_decorated_value()5�_�                    
   3    ����                                                                                                                                                                                                                                                                                                                                                             T�d     �   	            4                                raise KeyError.new()5�_�      	              
   4    ����                                                                                                                                                                                                                                                                                                                                                             T�d     �   	            6                                raise KeyError.new("")5�_�      
           	   
   ;    ����                                                                                                                                                                                                                                                                                                                                                             T�d     �   	            >                                raise KeyError.new("key :#{}")5�_�   	              
          ����                                                                                                                                                                                                                                                                                                                                                             T�d     �                       def respond_to?()5�_�   
                        ����                                                                                                                                                                                                                                                                                                                                                             T�d     �                             has_key()5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             T�d     �                                @hash.keys.map()5�_�                       1    ����                                                                                                                                                                                                                                                                                                                                                             T�d     �                               �             �               2              def fetch_possibly_decorated_value()5�_�                       &    ����                                                                                                                                                                                                                                                                                                                                                             T�d     �               '                    obj = @hash.fetch()5�_�                       /    ����                                                                                                                                                                                                                                                                                                                                                             T�d     �         "                                �         !    �                0                        if should_be_decorated()5�_�                       )    ����                                                                                                                                                                                                                                                                                                                                                             T�d     �      #   "      *                                decorate()5�_�                    "   (    ����                                                                                                                                                                                                                                                                                                                                                             T�d     �   !   $   (      )                def should_be_decorated()5�_�                    #       ����                                                                                                                                                                                                                                                                                                                                                             T�d     �   "   $   )                            ()5�_�                    #   !    ����                                                                                                                                                                                                                                                                                                                                                             T�d     �   "   $   )      #                      (obj.is_a?())5�_�                    #   /    ����                                                                                                                                                                                                                                                                                                                                                             T�d     �   "   $   )      1                      (obj.is_a?(Array) && obj[])5�_�                    #   8    ����                                                                                                                                                                                                                                                                                                                                                             T�d     �   "   $   )      :                      (obj.is_a?(Array) && obj[0].is_a?())5�_�                    #   L    ����                                                                                                                                                                                                                                                                                                                                                             T�d     �   "   '   )      M                      (obj.is_a?(Array) && obj[0].is_a?(Hash)) || obj.is_a?()5�_�                    &       ����                                                                                                                                                                                                                                                                                                                                                             T�d     �   %   (   ,                         def decorate()5�_�                    '   %    ����                                                                                                                                                                                                                                                                                                                                                             T�d     �   &   )   -      &                        if obj.is_a?()5�_�                    (   )    ����                                                                                                                                                                                                                                                                                                                                                             T�d     �   '   )   .      *                                obj.map {}5�_�                    (   F    ����                                                                                                                                                                                                                                                                                                                                                             T�d     �   '   *   .      H                                obj.map { |o| HashWithQuickAccess.new()}5�_�                    )   4    ����                                                                                                                                                                                                                                                                                                                                                             T�d     �   (   +   /      5                                    elsif obj.is_a?()5�_�                    *   D    ����                                                                                                                                                                                                                                                                                                                                                             T�d     �   )   /   0      E                                            HashWithQuickAccess.new()5�_�                    .        ����                                                                                                                                                                                                                                                                                                                                                             T�d     �   -   0   4      !                    def has_key()5�_�                    /   ,    ����                                                                                                                                                                                                                                                                                                                                                             T�d     �   3   5   ;                                    �   3   5   :    �   .   5   5      -                          all_keys.include?()5�_�                     4   %    ����                                                                                                                                                                                                                                                                                                                                                             T�d     �   3   9   ;      &                                    []5�_�      !                       ����                                                                                                                                                                                                                                                                                                                                                             T�g     �              ?   class HashWithQuickAccess       def initialize(hash)   D          @hash = ActiveSupport::HashWithIndifferentAccess.new(hash)               end             def method_missing(key)               if has_key(key)   7                    fetch_possibly_decorated_value(key)                           else   O                                raise KeyError.new("key :#{key} was not found")   '                                    end                 end       =        def respond_to?(method_name, include_private = false)   +              has_key(method_name) || super                   end                 def keys   (                @hash.keys.map(&:to_sym)                     end                   private       5              def fetch_possibly_decorated_value(key)   *                    obj = @hash.fetch(key)       3                        if should_be_decorated(obj)   -                                decorate(obj)   (                                    else   /                                            obj   3                                                end                             end       ,                def should_be_decorated(obj)   Q                      (obj.is_a?(Array) && obj[0].is_a?(Hash)) || obj.is_a?(Hash)                           end       #                  def decorate(obj)   +                        if obj.is_a?(Array)   J                                obj.map { |o| HashWithQuickAccess.new(o) }   9                                    elsif obj.is_a?(Hash)   H                                            HashWithQuickAccess.new(obj)   3                                                end                             end       $                    def has_key(key)   0                          all_keys.include?(key)                               end       "                      def all_keys   8                            @hash.keys.flat_map do |key|   5                                    [key, key.to_sym]   +                                        end   !                              end   end                                   end                           end                 end               end   	      end       end   end5�_�       "           !           ����                                                                                                                                                                                                                                                                                                                                                             T�h     �               �               �               5�_�   !   #           "           ����                                                                                                                                                                                                                                                                                                                                                             T�i    �                      5�_�   "   $           #           ����                                                                                                                                                                                                                                                                                                                                                             T��     �          7    5�_�   #   %           $           ����                                                                                                                                                                                                                                                                                                                                                             T��     �          9       �          8    5�_�   $               %      	    ����                                                                                                                                                                                                                                                                                                                                                             T��    �          9      
require ""5��