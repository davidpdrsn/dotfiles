Vim�UnDo� �X� H��V��LK�{cj�8�IǤ��2      P    assert response == "HTTP/1.1 200 OK\r\nServer: Some fake server\r\n\r\ntext"      !      "       "   "   "    U@��    _�                             ����                                                                                                                                                                                                                                                                                                                                                             U@��     �               5�_�                            ����                                                                                                                                                                                                                                                                                                                                                             U@��     �                  5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             U@��     �                defmodule HttpRequestTest do5�_�                            ����                                                                                                                                                                                                                                                                                                                                                 V        U@��     �                #  test "it parses the HTTP verb" do       request = create_request            assert "GET" == request.verb     end         test "it parses the path" do       request = create_request           assert "/" == request.path     end       !  test "it parses the headers" do       request = create_request       7    assert "localhost:1234" == request.headers[:"Host"]     end         test "it parses the data" do       request = create_request   ;    assert "{ \"data\": \"Hi there\" }\r\n" == request.data     end         def create_request do   >    {:ok, text} = File.read "test/fixtures/sample_request.txt"       HttpRequest.parse(text)     end5�_�                            ����                                                                                                                                                                                                                                                                                                                                                 V        U@��    �                 5�_�                            ����                                                                                                                                                                                                                                                                                                                                                 V        U@�     �                 �             5�_�                           ����                                                                                                                                                                                                                                                                                                                                                 V        U@�     �             �               	  test ""5�_�      	                     ����                                                                                                                                                                                                                                                                                                                                                 V        U@�     �      	           �             5�_�      
           	          ����                                                                                                                                                                                                                                                                                                                            	          	           V        U@�     �      	   	      	  test ""5�_�   	              
           ����                                                                                                                                                                                                                                                                                                                            	          	           V        U@�     �                      test "ok with"5�_�   
                         ����                                                                                                                                                                                                                                                                                                                                                 V        U@�     �                5�_�                           ����                                                                                                                                                                                                                                                                                                                                                 V        U@�     �                    response = HttpResponse.ok()5�_�                            ����                                                                                                                                                                                                                                                                                                                                                 V        U@�8    �               "    response = HttpResponse.ok("")5�_�                       #    ����                                                                                                                                                                                                                                                                                                                                                 V        U@�=     �                   �             5�_�                       	    ����                                                                                                                                                                                                                                                                                                                                                 V        U@�>     �             5�_�                       
    ����                                                                                                                                                                                                                                                                                                                            	          	           V        U@�?     �         	      
    assert5�_�                           ����                                                                                                                                                                                                                                                                                                                            	          	           V        U@�D     �                    assert response == 5�_�                           ����                                                                                                                                                                                                                                                                                                                            	          	           V        U@�D    �         
          �         	    5�_�                           ����                                                                                                                                                                                                                                                                                                                            
          
           V        U@�Y     �      	              # assert response == 5�_�                           ����                                                                                                                                                                                                                                                                                                                            
          
           V        U@�Z     �      	   
          assert response == 5�_�                           ����                                                                                                                                                                                                                                                                                                                            
          
           V        U@�Z   
 �      	   
          assert response == ""5�_�                           ����                                                                                                                                                                                                                                                                                                                            
          
           V        U@�`    �                "    response |> inspect |> IO.puts5�_�                       %    ����                                                                                                                                                                                                                                                                                                                            	          	           V        U@�b     �         	      &    response = HttpResponse.ok("text")5�_�                       )    ����                                                                                                                                                                                                                                                                                                                            	          	           V        U@�f     �         	      +    response = HttpResponse.ok("text", %{})5�_�                       +    ����                                                                                                                                                                                                                                                                                                                            	          	           V        U@�g     �         	      .    response = HttpResponse.ok("text", %{:""})5�_�                       2    ����                                                                                                                                                                                                                                                                                                                            	          	           V        U@�n     �         	      4    response = HttpResponse.ok("text", %{:"Server"})5�_�                       7    ����                                                                                                                                                                                                                                                                                                                            	          	           V        U@�p     �         	      :    response = HttpResponse.ok("text", %{:"Server" => ""})5�_�                       '    ����                                                                                                                                                                                                                                                                                                                            	          	           V        U@�    �         	      4    assert response == "HTTP/1.1 200 OK\r\n\r\ntext"5�_�                            ����                                                                                                                                                                                                                                                                                                                                                  V        U@��     �         	    �      	   	    5�_�                    	       ����                                                                                                                                                                                                                                                                                                                                                  V        U@��     �      
       5�_�                     
       ����                                                                                                                                                                                                                                                                                                                                                  V        U@��     �   	              test "ok" do5�_�      !                      ����                                                                                                                                                                                                                                                                                                                                                  V        U@��    �   
            J    response = HttpResponse.ok("text", %{:"Server" => "Some fake server"})5�_�       "           !      !    ����                                                                                                                                                                                                                                                                                                                                                  V        U@��     �               P    assert response == "HTTP/1.1 200 OK\r\nServer: Some fake server\r\n\r\ntext"5�_�   !               "      %    ����                                                                                                                                                                                                                                                                                                                                                  V        U@��    �               P    assert response == "HTTP/1.1 404 OK\r\nServer: Some fake server\r\n\r\ntext"5��