Vim�UnDo� ��B�����șo�$'�A�d��L���xR�U              	                       T��O    _�                             ����                                                                                                                                                                                                                                                                                                                                                             T��     �                   5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             T��     �                 �             �                  function! Pwd()5�_�                       
    ����                                                                                                                                                                                                                                                                                                                                                             T��    �                 return ""5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             T��     �                function! Pwd5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             T��    �                function! Pwd()5�_�                       	    ����                                                                                                                                                                                                                                                                                                                                                             T��.    �                 return "hi there"5�_�                       	    ����                                                                                                                                                                                                                                                                                                                                                 V        T��O     �                 return pwd5�_�      	                     ����                                                                                                                                                                                                                                                                                                                                                 V        T��R    �                 return getcwd()5�_�      
           	      	    ����                                                                                                                                                                                                                                                                                                                                                 V        T��]     �                 return getcwd()5�_�   	              
          ����                                                                                                                                                                                                                                                                                                                                                 V        T��_    �                 return split(getcwd()5�_�   
                        ����                                                                                                                                                                                                                                                                                                                                                 V        T��s     �                 return split(getcwd())5�_�                           ����                                                                                                                                                                                                                                                                                                                                                 V        T��u    �                 return split(getcwd(), "")5�_�                           ����                                                                                                                                                                                                                                                                                                                                                 V        T��    �                 return split(getcwd(), "/")5�_�                           ����                                                                                                                                                                                                                                                                                                                                                 V        T���   	 �                 �             5�_�                           ����                                                                                                                                                                                                                                                                                                                                                 V        T���     �                 return get()5�_�                           ����                                                                                                                                                                                                                                                                                                                                                 V        T���    �                 return get(path, len())5�_�                            ����                                                                                                                                                                                                                                                                                                                                                 V        T���     �              �             5�_�                            ����                                                                                                                                                                                                                                                                                                                                                 V        T���     �             5�_�                           ����                                                                                                                                                                                                                                                                                                                                      	           V        T���     �                !  let path = split(getcwd(), "/")   !  return get(path, len(path) - 1)5�_�                       
    ����                                                                                                                                                                                                                                                                                                                                                 V        T���    �               function! Pwd()5�_�                           ����                                                                                                                                                                                                                                                                                                                                                 V        T��_     �                 �             5�_�                       
    ����                                                                                                                                                                                                                                                                                                                                                 V        T��d     �                 return ""5�_�                           ����                                                                                                                                                                                                                                                                                                                                                 V        T��l     �                 return "~/notes"5�_�                           ����                                                                                                                                                                                                                                                                                                                                                 V        T��n     �                 return "~/notes/"5�_�                           ����                                                                                                                                                                                                                                                                                                                                                 V        T��s    �                 return "~/notes/" . Pwd()5�_�                           ����                                                                                                                                                                                                                                                                                                                                                 V        T���     �                 return "~/notes/" . Pwd()5�_�                           ����                                                                                                                                                                                                                                                                                                                                                 V        T���    �                  return "~/notes/" . Pwd() . ""5�_�                       
    ����                                                                                                                                                                                                                                                                                                                                                 V        T���     �         	       �             5�_�                           ����                                                                                                                                                                                                                                                                                                                                      
           V        T���    �         
      let g:notes_folder = ""5�_�                    	   	    ����                                                                                                                                                                                                                                                                                                                                      
           V        T���    �      
   
      $  return "~/notes/" . Pwd() . ".txt"5�_�                             ����                                                                                                                                                                                                                                                                                                                                      
           V        T��N    �              
   function! Pwd()   !  let path = split(getcwd(), "/")   !  return get(path, len(path) - 1)   endfunction       let g:notes_folder = "~/notes/"       function! PathToNotesFile()   (  return g:notes_folder . Pwd() . ".txt"   endfunction5��