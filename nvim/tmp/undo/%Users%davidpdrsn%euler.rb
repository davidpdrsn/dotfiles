Vim�UnDo� ���@��ܚ��W�Ĵ�P-|�b��R�����                                     Tζz   
 _�                             ����                                                                                                                                                                                                                                                                                                                                                             Tε�     �                   5�_�                       	    ����                                                                                                                                                                                                                                                                                                                                                             Tε�     �                  
cache = []5�_�                       	    ����                                                                                                                                                                                                                                                                                                                                                             Tε�     �                  
cache = {}5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             Tε�     �                   �             �                 �             �                 	def fib()5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             Tε�     �         	      	    fib()5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             Tζ      �         	          fib(n-1)5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             Tζ     �         	          fib(n-1) + fib()5�_�      	              	       ����                                                                                                                                                                                                                                                                                                                                                             Tζ     �   	               �   	            5�_�      
           	      	    ����                                                                                                                                                                                                                                                                                                                                                             Tζ    �   
              
puts fib()5�_�   	              
          ����                                                                                                                                                                                                                                                                                                                               	                  V        Tζ     �                   fib(n-1) + fib(n-2)5�_�   
                        ����                                                                                                                                                                                                                                                                                                                               	                  V        Tζ     �                   onefib(n-1) + fib(n-2)5�_�                           ����                                                                                                                                                                                                                                                                                                                               	                  V        Tζ     �      	             one = fib(n-1) + fib(n-2)5�_�                       	    ����                                                                                                                                                                                                                                                                                                                               	                  V        Tζ     �                   one = fib(n-1)�                    two = fib(n-2)5�_�                           ����                                                                                                                                                                                                                                                                                                                               	                  V        Tζ)     �                   one = cache[]5�_�                           ����                                                                                                                                                                                                                                                                                                                               	                  V        Tζ*     �                   one = cache[n]5�_�                           ����                                                                                                                                                                                                                                                                                                                               	                  V        Tζ,     �                   one = cache[n] ||= fib()5�_�                           ����                                                                                                                                                                                                                                                                                                                               	                  V        Tζ.     �      	       �             5�_�                           ����                                                                                                                                                                                                                                                                                                                               	                  V        Tζ/     �      	             one = cache[n] ||= fib(n-1)5�_�                           ����                                                                                                                                                                                                                                                                                                                               	                  V        Tζ1    �      	             two = cache[n] ||= fib(n-1)5�_�                           ����                                                                                                                                                                                                                                                                                                                                                V       Tζ5    �      
             �      
       5�_�                            ����                                                                                                                                                                                                                                                                                                                                                V       Tζ=     �                
cache = {}5�_�                       
    ����                                                                                                                                                                                                                                                                                                                               
          
          
    TζB    �      	             two = cache[n] ||= fib(n-2)�                   one = cache[n] ||= fib(n-1)5�_�                            ����                                                                                                                                                                                                                                                                                                                               
          
          
    TζI     �                @@cache = {}5�_�                       
    ����                                                                                                                                                                                                                                                                                                                               
          
          
    TζL    �      	         !    one = @@cache[n] ||= fib(n-1)   !    two = @@cache[n] ||= fib(n-2)5�_�                           ����                                                                                                                                                                                                                                                                                                                               
          
          
    TζY     �                    one = @cache[n] ||= fib(n-1)5�_�                           ����                                                                                                                                                                                                                                                                                                                               
          
          
    TζZ     �               "    one = @cache[n-1] ||= fib(n-1)5�_�                           ����                                                                                                                                                                                                                                                                                                                               
          
          
    Tζ\    �      	              two = @cache[n] ||= fib(n-2)5�_�                       	    ����                                                                                                                                                                                                                                                                                                                               
          
          
    Tζc   	 �                 puts fib(10)5�_�                            ����                                                                                                                                                                                                                                                                                                                                                  V        Tζn     �                  �               5�_�                            ����                                                                                                                                                                                                                                                                                                                                                  V        Tζy   
 �                 numbers = []5��