Vim�UnDo� ��λ�Z;\@���Z5��;Dm�,*�=�R���z      +    decorated_obj = decorator.decorate(obj)   	         
       
   
   
    T��    _�                             ����                                                                                                                                                                                                                                                                                                                                                  V        Tr�    �                  require "delegate"   require "active_support/all"   0require_relative "../../lib/composite_decorator"       describe CompositeDecorator do   O  it "composes decorators and decorates and object with all of them at once" do       obj = double   @    decorator = CompositeDecorator.new([DecoratorA, DecoratorB])   +    decorated_obj = decorator.decorate(obj)       3    expect(decorated_obj.method_from_a).to eq "foo"   3    expect(decorated_obj.method_from_b).to eq "bar"     end   end       "class DecoratorA < SimpleDelegator     def method_from_a   	    "foo"     end   end       "class DecoratorB < SimpleDelegator     def method_from_b   	    "bar"     end   end5�_�                            ����                                                                                                                                                                                                                                                                                                                                                  V        Tr�     �                 �              5�_�                       	    ����                                                                                                                                                                                                                                                                                                                                                  V        Tr�     �                
require ""5�_�                            ����                                                                                                                                                                                                                                                                                                                                                  V        Tr�     �                "5�_�                           ����                                                                                                                                                                                                                                                                                                                                                  V        Tr�     �                require 5�_�                       
    ����                                                                                                                                                                                                                                                                                                                                                  V        Tr�     �                require  ""5�_�                       	    ����                                                                                                                                                                                                                                                                                                                                                  V        Tr�     �                
require ""5�_�      	                     ����                                                                                                                                                                                                                                                                                                                                                  V        Tr�    �                require "rails_helpers"5�_�      
           	           ����                                                                                                                                                                                                                                                                                                                                                  V        Tr�    �                 require "rails_helper"5�_�   	               
   	       ����                                                                                                                                                                                                                                                                                                                                                             T��~    �      
         +    decorated_obj = decorator.decorate(obj)5��