Vim�UnDo� ��[�F�lT��*j,7��a���q�o�O&~*�:   $   # hi                             T�	�    _�                             ����                                                                                                                                                                                                                                                                                                                                                             T���     �               �               �               5�_�                            ����                                                                                                                                                                                                                                                                                                                                                             T���     �                  5�_�                            ����                                                                                                                                                                                                                                                                                                                                                             T���    �                  5�_�                       +    ����                                                                                                                                                                                                                                                                                                                                                             T�	?     �                a          callback = options.fetch(:with) { fail "spy_on called without block or `with` option" }5�_�                       ?    ����                                                                                                                                                                                                                                                                                                                                                             T�	@    �         !      A            fail "spy_on called without block or `with` option" }5�_�                       +    ����                                                                                                                                                                                                                                                                                                                                                             T�	]     �         "      +          callback = options.fetch(:with) {5�_�                       
    ����                                                                                                                                                                                                                                                                                                                                                             T�	_    �         "                }5�_�      	                      ����                                                                                                                                                                                                                                                                                                                                                             T�	{     �               "   7require "active_support/core_ext/array/extract_options"       module MethodSpy     def spy_on(*args)   #    options = args.extract_options!           args.each do |name|   &      original = instance_method(name)             define_method(name) do   6        if block_given? && options.fetch(:with, false)   B          fail "spy_on called with both `with` option and block."\   4            " Its ambiguous what to do in that case"           end               if block_given?             yield name           else   ,          callback = options.fetch(:with) do   ?            fail "spy_on called without block or `with` option"             end             m = method(callback)             if m.arity == 1               m.call(name)             else               m.call             end           end                original.bind(self).call   	      end       end     end   end5�_�      
           	           ����                                                                                                                                                                                                                                                                                                                                                             T�	}     �                >>>>>>> Stashed changes5�_�   	              
      
    ����                                                                                                                                                                                                                                                                                                                                         
       V   
    T�	    �                <<<<<<< Updated upstream   +          callback = options.fetch(:with) {   ?            fail "spy_on called without block or `with` option"             }   =======5�_�   
                         ����                                                                                                                                                                                                                                                                                                                                                v       T�	�     �         #       �         "    5�_�                           ����                                                                                                                                                                                                                                                                                                                                                v       T�	�     �         #       ""5�_�                           ����                                                                                                                                                                                                                                                                                                                                                v       T�	�     �         $      ""5�_�                           ����                                                                                                                                                                                                                                                                                                                                                v       T�	�     �         $      " "5�_�                       
    ����                                                                                                                                                                                                                                                                                                                                                v       T�	�     �         $      
" hi there5�_�                           ����                                                                                                                                                                                                                                                                                                                                                v       T�	�     �         $      " hi there"5�_�                           ����                                                                                                                                                                                                                                                                                                                                                v       T�	�     �         $    5�_�                            ����                                                                                                                                                                                                                                                                                                                                                v       T�	�     �         %      
"hi there"5�_�                            ����                                                                                                                                                                                                                                                                                                                                                v       T�	�    �                 5�_�                           ����                                                                                                                                                                                                                                                                                                                                                v       T�	�   	 �         $      # hi5�_�                            ����                                                                                                                                                                                                                                                                                                                                                v       T�	�    �         $      # hi ho5��