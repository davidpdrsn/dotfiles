Vim�UnDo� Ɖl�Oy�	]�Ax�N�L�U���|9x�9   (   lock '3.2.1'            	       	   	   	    U���    _�                             ����                                                                                                                                                                                                                                                                                                                                       
           V        U���     �                    # Default branch is :master   D# ask :branch, proc { `git rev-parse --abbrev-ref HEAD`.chomp }.call       0# Default deploy_to directory is /var/www/my_app5�_�                            ����                                                                                                                                                                                                                                                                                                                                                  V        U���     �                     # Default value for :scm is :git   # set :scm, :git       &# Default value for :format is :pretty   # set :format, :pretty       (# Default value for :log_level is :debug   # set :log_level, :debug       !# Default value for :pty is false5�_�                            ����                                                                                                                                                                                                                                                                                                                                       	           V        U���     �                    '# Default value for :linked_files is []5�_�                    	        ����                                                                                                                                                                                                                                                                                                                            	           
           V        U���     �      	              %# Default value for linked_dirs is []5�_�                    
        ����                                                                                                                                                                                                                                                                                                                            
                      V        U���    �   	   
              %# Default value for default_env is {}   3# set :default_env, { path: "/opt/ruby/bin:$PATH" }       &# Default value for keep_releases is 5   # set :keep_releases, 55�_�                           ����                                                                                                                                                                                                                                                                                                                                                  V        U���    �         '      set :rbenv_ruby, '2.1.4'5�_�                       x    ����                                                                                                                                                                                                                                                                                                                                                  V        U��
     �         (    �         (    �         '    5�_�      	                      ����                                                                                                                                                                                                                                                                                                                                                  V        U��    �                set :rbenv_ruby, '2.2.2'   ~set :rbenv_prefix, "RBENV_ROOT=#{fetch(:rbenv_path)} RBENV_VERSION=#{fetch(:rbenv_ruby)} #{fetch(:rbenv_path)}/bin/rbenv exec"    5�_�                  	          ����                                                                                                                                                                                                                                                                                                                                                  V        U���    �         (      lock '3.2.1'5��