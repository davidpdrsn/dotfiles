Vim�UnDo� ���kz�c���ӞcK��[��1I�,U�.�                                      T�p�    _�                             ����                                                                                                                                                                                                                                                                                                                                                             T�o�     �                   �               5�_�                            ����                                                                                                                                                                                                                                                                                                                                                             T�o�    �                  5�_�                            ����                                                                                                                                                                                                                                                                                                                                                             T�o�     �              �             5�_�                            ����                                                                                                                                                                                                                                                                                                                                                             T�o�    �             5�_�                    	        ����                                                                                                                                                                                                                                                                                                                                                             T�p:     �   	               �   
            �   	            5�_�                            ����                                                                                                                                                                                                                                                                                                                            	                      V        T�p<     �              	   2require 'rspec/core/formatters/progress_formatter'       >class MyFormatter < RSpec::Core::Formatters::ProgressFormatter   I  def dump_summary(duration, example_count, failure_count, pending_count)   	    super       output.print("Awesome\n")     end   end    5�_�                            ����                                                                                                                                                                                                                                                                                                                                                  V        T�p<    �                end�   
             <  @output << "example: " << notification.example.description�   	             !def example_started(notification)�      	          end�                  @output = output�                def initialize(output)�                7RSpec::Core::Formatters.register self, :example_started�                ?# us that this was written against the RSpec 3.x formatter API.�                E# This registers the notifications this formatter supports, and tells5�_�                             ����                                                                                                                                                                                                                                                                                                                                                  V        T�p�    �   
             >    @output << "example: " << notification.example.description5��