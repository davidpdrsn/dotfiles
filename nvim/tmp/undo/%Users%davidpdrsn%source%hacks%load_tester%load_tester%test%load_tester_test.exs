Vim�UnDo� 4��Z�=��G'���c�����u_g38�n��   	   <    time = LoadTester.load_test("http://localhost:3000", 10)      !      U       U   U   U    U N   S _�                            ����                                                                                                                                                                                                                                                                                                                                                             U��     �                 test "the truth" do5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             U��     �                 test "#food" do5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             U��     �                 test "#foo" do5�_�                          ����                                                                                                                                                                                                                                                                                                                                                             U��     �                   assert 1 + 1 == 25�_�                           ����                                                                                                                                                                                                                                                                                                                                                             U��    �                   assert LoadTester.foo == ""5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             U�     �                �             �             5�_�      	                      ����                                                                                                                                                                                                                                                                                                                                                             U�     �               *response = HTTPotion.get "httpbin.org/get"5�_�      
           	          ����                                                                                                                                                                                                                                                                                                                                                             U�     �               ,  response = HTTPotion.get "httpbin.org/get"5�_�   	              
          ����                                                                                                                                                                                                                                                                                                                                                             U�    �             5�_�   
                       ����                                                                                                                                                                                                                                                                                                                                                             U�    �         
          �         	    5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             U�     �                   IO.puts response5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             U�-   	 �               .    response = HTTPotion.get "httpbin.org/get"5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             U�:   
 �               /    response = HTTPotion.get "lonelyproton.com"5�_�                            ����                                                                                                                                                                                                                                                                                                                                                             U�B    �                    !    assert LoadTester.foo == "hi"5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             U�i    �         	      -    response = HTTPotion.get "localhost:4567"5�_�                       %    ����                                                                                                                                                                                                                                                                                                                                                             U�    �         	      4    response = HTTPotion.get "http://localhost:4567"5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             U�      �         
          �         	    5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             U�"     �         
      -    response = HTTPotion.get "localhost:4567"       tcp://localhost:45675�_�                       .    ����                                                                                                                                                                                                                                                                                                                                                             U�#     �         	      B    response = HTTPotion.get "localhost:4567" tcp://localhost:45675�_�                       2    ����                                                                                                                                                                                                                                                                                                                                                             U�%    �         	      2    response = HTTPotion.get "tcp://localhost:45675�_�                           ����                                                                                                                                                                                                                                                                                                                                                             U�5    �         	      3    response = HTTPotion.get "tcp://localhost:4567"5�_�                       "    ����                                                                                                                                                                                                                                                                                                                                                             U�8    �         	      4    response = HTTPotion.get "http://localhost:4567"5�_�                       "    ����                                                                                                                                                                                                                                                                                                                                                             U�;    �         	      5    response = HTTPotion.get "https://localhost:4567"5�_�                       /    ����                                                                                                                                                                                                                                                                                                                                                             U�Z     �         	      4    response = HTTPotion.get "http://localhost:4567"5�_�                       /    ����                                                                                                                                                                                                                                                                                                                                                             U�[    �         	      6    response = HTTPotion.get "http://localhost:cw4567"5�_�                            ����                                                                                                                                                                                                                                                                                                                                                             U�9     �                 5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             U�:     �                    IO.puts response.body�                4    response = HTTPotion.get "http://localhost:3000"5�_�                            ����                                                                                                                                                                                                                                                                                                                                                             U�:     �             5�_�      !                       ����                                                                                                                                                                                                                                                                                                                                                             U�;     �                 5�_�       "           !          ����                                                                                                                                                                                                                                                                                                                                                             U�;     �      
               �      
       �      
   	          �             5�_�   !   #           "          ����                                                                                                                                                                                                                                                                                                                                                             U�P     �      	             :timer.tc fn ->5�_�   "   $           #          ����                                                                                                                                                                                                                                                                                                                                                             U�P    �      	             {}:timer.tc fn ->5�_�   #   %           $   
       ����                                                                                                                                                                                                                                                                                                                                      
          V       U�W    �   
                �   
          5�_�   $   &           %          ����                                                                                                                                                                                                                                                                                                                                      
          V       U�a     �                    # IO.puts response.body�                6    # response = HTTPotion.get "http://localhost:3000"5�_�   %   '           &          ����                                                                                                                                                                                                                                                                                                                                      
          V       U�a     �                4    response = HTTPotion.get "http://localhost:3000"       IO.puts response.body5�_�   &   (           '           ����                                                                                                                                                                                                                                                                                                                                                V       U�b     �      
       �             5�_�   '   )           (          ����                                                                                                                                                                                                                                                                                                                                      
          V       U�b     �                      1 + 15�_�   (   *           )           ����                                                                                                                                                                                                                                                                                                                                                V       U�c     �      	         4    response = HTTPotion.get "http://localhost:3000"       IO.puts response.body5�_�   )   +           *          ����                                                                                                                                                                                                                                                                                                                                                V       U�d     �                      IO.puts response.body5�_�   *   ,           +          ����                                                                                                                                                                                                                                                                                                                                                V       U�e     �               6      response = HTTPotion.get "http://localhost:3000"5�_�   +   -           ,          ����                                                                                                                                                                                                                                                                                                                                                V       U�e     �               -      = HTTPotion.get "http://localhost:3000"5�_�   ,   .           -          ����                                                                                                                                                                                                                                                                                                                                                V       U�e    �               ,       HTTPotion.get "http://localhost:3000"5�_�   -   /           .   
       ����                                                                                                                                                                                                                                                                                                                                                V       U�     �   	                IO.puts time5�_�   .   1           /           ����                                                                                                                                                                                                                                                                                                                                                V       U�     �                $    {time, _value} = :timer.tc fn ->   +      HTTPotion.get "http://localhost:3000"       end    5�_�   /   2   0       1          ����                                                                                                                                                                                                                                                                                                                                                V       U�     �             �             5�_�   1   3           2          ����                                                                                                                                                                                                                                                                                                                                                V       U�     �               $    {time, _value} = :timer.tc fn ->5�_�   2   4           3          ����                                                                                                                                                                                                                                                                                                                                                v       U�     �               #    time, _value} = :timer.tc fn ->5�_�   3   5           4          ����                                                                                                                                                                                                                                                                                                                                                v       U�     �                   LoadTester.benchmark      fn ->5�_�   4   6           5   	        ����                                                                                                                                                                                                                                                                                                                                                V       U�     �      	           5�_�   5   7           6           ����                                                                                                                                                                                                                                                                                                                                                V       U�   ! �                 5�_�   6   8           7          ����                                                                                                                                                                                                                                                                                                                                                V       U�     �         	      +      HTTPotion.get "http://localhost:3000"5�_�   7   9           8          ����                                                                                                                                                                                                                                                                                                                                                V       U�   & �         
            �         	    5�_�   8   :           9          ����                                                                                                                                                                                                                                                                                                                                                V       U�     �         
          LoadTester.benchmark fn ->5�_�   9   ;           :          ����                                                                                                                                                                                                                                                                                                                                                V       U�     �                      IO.puts response.body5�_�   :   <           ;          ����                                                                                                                                                                                                                                                                                                                                                V       U�   ( �      
   
          �      	   	    5�_�   ;   =           <   	       ����                                                                                                                                                                                                                                                                                                                                                V       U�     �      
             IO.puts time5�_�   <   >           =   	   !    ����                                                                                                                                                                                                                                                                                                                                                V       U�   , �      
         "    time = LoadTester.load_test ""5�_�   =   ?           >      %    ����                                                                                                                                                                                                                                                                                                                                         $       V   $    U�   ) �                %    time = LoadTester.benchmark fn ->   6      response = HTTPotion.get "http://localhost:3000"       end    5�_�   >   @           ?          ����                                                                                                                                                                                                                                                                                                                                          %       V   5    U�   . �                   �             5�_�   ?   A           @          ����                                                                                                                                                                                                                                                                                                                                                V       U�     �               7    time = LoadTester.load_test "http://localhost:3000"5�_�   @   B           A      7    ����                                                                                                                                                                                                                                                                                                                                                V       U�     �               7    time = LoadTester.load_test("http://localhost:3000"5�_�   A   C           B      7    ����                                                                                                                                                                                                                                                                                                                                                V       U�   1 �               8    time = LoadTester.load_test("http://localhost:3000")5�_�   B   D           C      9    ����                                                                                                                                                                                                                                                                                                                                                V       U�U   2 �               ;    time = LoadTester.load_test("http://localhost:3000", 0)5�_�   C   E           D      9    ����                                                                                                                                                                                                                                                                                                                                                V       U�b   4 �               ;    time = LoadTester.load_test("http://localhost:3000", 1)5�_�   D   F           E      9    ����                                                                                                                                                                                                                                                                                                                                                V       U��   7 �               <    time = LoadTester.load_test("http://localhost:3000", 10)5�_�   E   G           F      9    ����                                                                                                                                                                                                                                                                                                                                                V       U��   : �               =    time = LoadTester.load_test("http://localhost:3000", 100)5�_�   F   H           G          ����                                                                                                                                                                                                                                                                                                                                                V       U�V     �             �             5�_�   G   I           H          ����                                                                                                                                                                                                                                                                                                                                                V       U�V     �                <    time = LoadTester.load_test("http://localhost:3000", 10)5�_�   H   J           I      !    ����                                                                                                                                                                                                                                                                                                                                                V       U�X   ; �         	      <    time = LoadTester.load_test("http://localhost:3000", 10)5�_�   I   K           J      !    ����                                                                                                                                                                                                                                                                                                                                                V       U�e   = �         	      7    time = LoadTester.load_test("lonelyproton.com", 10)5�_�   J   L           K      ;    ����                                                                                                                                                                                                                                                                                                                                                V       U�l   ? �         	      >    time = LoadTester.load_test("http://lonelyproton.com", 10)5�_�   K   M           L      ;    ����                                                                                                                                                                                                                                                                                                                                                V       U��   A �         	      <    time = LoadTester.load_test("http://lonelyproton.com", )5�_�   L   N           M      <    ����                                                                                                                                                                                                                                                                                                                                                V       U��   D �         	      >    time = LoadTester.load_test("http://lonelyproton.com", 10)5�_�   M   O           N      <    ����                                                                                                                                                                                                                                                                                                                               <          <       V   <    U��   F �         	      =    time = LoadTester.load_test("http://lonelyproton.com", 1)5�_�   N   P           O      =    ����                                                                                                                                                                                                                                                                                                                               <          <       V   <    U��   H �         	      >    time = LoadTester.load_test("http://lonelyproton.com", 10)5�_�   O   Q           P      =    ����                                                                                                                                                                                                                                                                                                                               <          <       V   <    U��     �         	      @    time = LoadTester.load_test("http://lonelyproton.com", 1000)5�_�   P   R           Q      =    ����                                                                                                                                                                                                                                                                                                                               <          <       V   <    U��     �         	      ?    time = LoadTester.load_test("http://lonelyproton.com", 100)5�_�   Q   S           R      =    ����                                                                                                                                                                                                                                                                                                                               <          <       V   <    U��   N �         	      >    time = LoadTester.load_test("http://lonelyproton.com", 10)5�_�   R   T           S      ;    ����                                                                                                                                                                                                                                                                                                                               =                 V   =    U 7   Q �         	      ?    time = LoadTester.load_test("http://lonelyproton.com", 100)5�_�   S   U           T      <    ����                                                                                                                                                                                                                                                                                                                               =                 V   =    U K     �         	      >    time = LoadTester.load_test("http://lonelyproton.com", 10)5�_�   T               U      (    ����                                                                                                                                                                                                                                                                                                                               =                 V   =    U M   S �         	      ?    time = LoadTester.load_test("http://lonelyproton.com", 100)5�_�   /           1   0          ����                                                                                                                                                                                                                                                                                                                                                V       U�     �             �               $    {time, _value} = :timer.tc fn ->   +      HTTPotion.get "http://localhost:3000"       end    5�_�                            ����                                                                                                                                                                                                                                                                                                                                                             U��    �              5�_�   
                        ����                                                                                                                                                                                                                                                                                                                                                             U�    �         	      /    response = HTTPotion.get "lonelyproton.com"5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             U��     �                5��