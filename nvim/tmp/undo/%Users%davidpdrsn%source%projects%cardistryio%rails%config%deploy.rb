Vim�UnDo� q�X"�0Iw?���>v�Pv�Εi�),�?�                    2       2   2   2    U�ce    _�                             ����                                                                                                                                                                                                                                                                                                                                                             U�b     �                  �               �               5�_�                            ����                                                                                                                                                                                                                                                                                                                                                             U�b      �         !    5�_�                            ����                                                                                                                                                                                                                                                                                                                                                             U�b"     �                 5�_�                           ����                                                                                                                                                                                                                                                                                                                                                            U�b&     �         !    5�_�                           ����                                                                                                                                                                                                                                                                                                                                      "           V        U�b,     �       "          end�      !            execute "some_command"�                 on "root@example.com" do5�_�      	                      ����                                                                                                                                                                                                                                                                                                                                      "           V        U�b/    �      !   "          execute "some_command"5�_�      
           	          ����                                                                                                                                                                                                                                                                                                                                               v       U�bW     �          "    �         "    5�_�   	              
           ����                                                                                                                                                                                                                                                                                                                                               v       U�bX     �          #      Hserver "deployer@178.62.163.33", user: "deployer", roles: %w(app web db)5�_�   
                       ����                                                                                                                                                                                                                                                                                                                                               v       U�b\     �          #      A"deployer@178.62.163.33", user: "deployer", roles: %w(app web db)5�_�                            ����                                                                                                                                                                                                                                                                                                                                               v       U�b]     �                "deployer@178.62.163.33"5�_�                           ����                                                                                                                                                                                                                                                                                                                                               v       U�b]     �      !   "    �          "    5�_�                           ����                                                                                                                                                                                                                                                                                                                                               v       U�b]     �          #        on "root@example.com" do5�_�                           ����                                                                                                                                                                                                                                                                                                                                               v       U�ba     �          #        on   "deployer@178.62.163.33"5�_�                           ����                                                                                                                                                                                                                                                                                                                                               v       U�ba    �          "        on "deployer@178.62.163.33"5�_�                            ����                                                                                                                                                                                                                                                                                                                                               v       U�bv    �      !   "          execute "ls"5�_�                    "        ����                                                                                                                                                                                                                                                                                                                                               v       U�b�     �   "               �   #            �   "            5�_�                            ����                                                                                                                                                                                                                                                                                                                                               v       U�b�     �                   on "deployer@178.62.163.33" do5�_�                    $       ����                                                                                                                                                                                                                                                                                                                                               v       U�b�     �   $   &   +    �   $   %   +    5�_�                    $       ����                                                                                                                                                                                                                                                                                                                                               v       U�b�     �   #   $          &  on primary fetch(:migration_role) do5�_�                           ����                                                                                                                                                                                                                                                                                                                           !                    V       U�b�     �                task :execute_on_server do   #    execute "make db:migrate:reset"     end   end5�_�                   #   "    ����                                                                                                                                                                                                                                                                                                                                               V       U�b�    �   "   $   '      #        execute :rake, "db:migrate"5�_�                    "       ����                                                                                                                                                                                                                                                                                                                                               V       U�b�     �   !   "          *      with rails_env: fetch(:rails_env) do5�_�                    #       ����                                                                                                                                                                                                                                                                                                                                               V       U�b�     �   "   #          	      end5�_�                    "       ����                                                                                                                                                                                                                                                                                                                                               V       U�b�     �   !   #   %      )        execute :rake, "db:migrate:reset"5�_�                    "       ����                                                                                                                                                                                                                                                                                                                                               V       U�b�     �   "   $   &       �   #   $   &    �   "   $   %    5�_�                     "       ����                                                                                                                                                                                                                                                                                                                                               V       U�b�     �   !   #          '      execute :rake, "db:migrate:reset"5�_�      !               #        ����                                                                                                                                                                                                                                                                                                                                               V       U�b�     �   "   $   &      Jrun "cd '#{current_path}' && #{rake} super_awesome RAILS_ENV=#{rails_env}"5�_�       .           !   #       ����                                                                                                                                                                                                                                                                                                                                               V       U�b�     �   "   $   &      L  run "cd '#{current_path}' && #{rake} super_awesome RAILS_ENV=#{rails_env}"5�_�   !   /   "       .   #       ����                                                                                                                                                                                                                                                                                                                                               V       U�cB     �   "   $          N    run "cd '#{current_path}' && #{rake} super_awesome RAILS_ENV=#{rails_env}"5�_�   .   0           /   "       ����                                                                                                                                                                                                                                                                                                                                               V       U�cB     �   !   #          )      # execute :rake, "db:migrate:reset"5�_�   /   1           0          ����                                                                                                                                                                                                                                                                                                                                               V       U�cD     �          &      $task :migrate => [:set_rails_env] do5�_�   0   2           1          ����                                                                                                                                                                                                                                                                                                                                               V       U�cE    �          &      task :migrate => do5�_�   1               2           ����                                                                                                                                                                                                                                                                                                                                               V       U�cd    �             
       1desc 'Runs rake db:migrate if migrations are set'   task :migrate do      on "deployer@178.62.163.33" do       within release_path do   '      execute :rake, "db:migrate:reset"   P    # run "cd '#{current_path}' && #{rake} super_awesome RAILS_ENV=#{rails_env}"       end     end   end5�_�   !   #       .   "   #       ����                                                                                                                                                                                                                                                                                                                           #          #          v       U�b�     �   "   $   &      P      run "cd '#{current_path}' && #{rake} super_awesome RAILS_ENV=#{rails_env}"5�_�   "   $           #   #       ����                                                                                                                                                                                                                                                                                                                           #          #          v       U�b�     �   #   $   &       �   $   %   '    �   #   %   '      rake = fetch(:rake, 'rake')   +rails_env = fetch(:rails_env, 'production')       Jrun "cd '#{current_path}' && #{rake} super_awesome RAILS_ENV=#{rails_env}"5�_�   #   %           $   #        ����                                                                                                                                                                                                                                                                                                                           %          (          V       U�b�     �   #   $   *       5�_�   $   &           %   %        ����                                                                                                                                                                                                                                                                                                                           %          (          V       U�b�     �   $   )   +        rake = fetch(:rake, 'rake')   -  rails_env = fetch(:rails_env, 'production')       L  run "cd '#{current_path}' && #{rake} super_awesome RAILS_ENV=#{rails_env}"5�_�   %   '           &   %       ����                                                                                                                                                                                                                                                                                                                           %          (          V       U�c      �   $   )   +          rake = fetch(:rake, 'rake')   /    rails_env = fetch(:rails_env, 'production')       N    run "cd '#{current_path}' && #{rake} super_awesome RAILS_ENV=#{rails_env}"5�_�   &   (           '   %       ����                                                                                                                                                                                                                                                                                                                           %          (          V       U�c      �   $   )   +      !      rake = fetch(:rake, 'rake')   1      rails_env = fetch(:rails_env, 'production')       P      run "cd '#{current_path}' && #{rake} super_awesome RAILS_ENV=#{rails_env}"5�_�   '   )           (   #       ����                                                                                                                                                                                                                                                                                                                           $          '          V       U�c     �   "   $        5�_�   (   *           )   '   +    ����                                                                                                                                                                                                                                                                                                                           $          '          V       U�c    �   &   (   *      S      run "cd '#{current_path}' && #{rake} db:migrate:reset RAILS_ENV=#{rails_env}"5�_�   )   +           *          ����                                                                                                                                                                                                                                                                                                                           $          '          V       U�c'     �          *      task :migrate => do5�_�   *   ,           +          ����                                                                                                                                                                                                                                                                                                                           $          '          V       U�c(     �          *      task :migrate > do5�_�   +   -           ,          ����                                                                                                                                                                                                                                                                                                                           $          '          V       U�c(     �          *      task :migrate  do5�_�   ,               -          ����                                                                                                                                                                                                                                                                                                                           $          '          V       U�c(    �          *      task :migrate do5�_�                            ����                                                                                                                                                                                                                                                                                                                                               V       U�b�     �                5  set :rbenv_map_bins, %w(rake gem bundle ruby rails)�                  set :rbenv_roles, :all�                )  set :passenger_restart_with_touch, true�                  namespace :deploy do�                     # desc "Restart application"�                    # task :restart do�                1    #   on roles(:app), in: :sequence, wait: 5 do�                4    #     execute "sudo /etc/init.d/apache2 restart"�                    #   end�                	    # end�                !    # after :publishing, :restart�                  end�                3  desc 'Runs rake db:migrate if migrations are set'�                 &  task :migrate => [:set_rails_env] do�      !          "    on "deployer@178.62.163.33" do�       "                within release_path do�   !   #          ,        with rails_env: fetch(:rails_env) do�   "   $          %          execute :rake, "db:migrate"�   #   %                  end�   $   &          	      end�   %   '              end�   &   (            end5�_�   
                       ����                                                                                                                                                                                                                                                                                                                                               v       U�bY     �          #      "deployer@178.62.163.33"5�_�                            ����                                                                                                                                                                                                                                                                                                                                               v       U�bZ     �          #      "deployer@178.62.163.33s5�_�                            ����                                                                                                                                                                                                                                                                                                                                                            U�b'     �                5  set :rbenv_map_bins, %w(rake gem bundle ruby rails)�                  set :rbenv_roles, :all�                )  set :passenger_restart_with_touch, true�                  namespace :deploy do�                     # desc "Restart application"�                    # task :restart do�                1    #   on roles(:app), in: :sequence, wait: 5 do�                4    #     execute "sudo /etc/init.d/apache2 restart"�                    #   end�                	    # end�                !    # after :publishing, :restart�                  end�                  task :execute_on_server do�                     on "root@example.com" do�      !                execute "some_command"�       "              end�   !   #            end5�_�                            ����                                                                                                                                                                                                                                                                                                                                                            U�b"     �                5  set :rbenv_map_bins, %w(rake gem bundle ruby rails)�                  set :rbenv_roles, :all�                )  set :passenger_restart_with_touch, true�                  namespace :deploy do�                     # desc "Restart application"�                    # task :restart do�                1    #   on roles(:app), in: :sequence, wait: 5 do�                4    #     execute "sudo /etc/init.d/apache2 restart"�                    #   end�                	    # end�                !    # after :publishing, :restart�                  end�                  task :execute_on_server do�                    on "root@example.com" do�                       execute "some_command"�      !              end�       "            end5��