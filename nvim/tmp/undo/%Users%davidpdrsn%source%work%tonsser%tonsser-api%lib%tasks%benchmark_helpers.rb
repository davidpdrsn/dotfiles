Vim�UnDo� ��t�"�>�P^� /@^KS����4�Z��ZUT�   �                                   T��=    _�                             ����                                                                                                                                                                                                                                                                                                                                                             T�s�    �   F   H          end�   E   G            end�   D   F              end�   C   E          	      end�   B   D                  end�   A   C                    token�   @   B                     File.write(path, json)�   ?   A          A          json = { created_at: Date.today, token: token }.to_json�   >   @          "          token = STDIN.gets.chomp�   =   ?          L          print "Enter facebook access token (will be cached for one day): "�   <   >                  else�   ;   =          .          JSON.parse(File.read(path))["token"]�   :   <          3        if file_exists_and_is_less_than_one_day_old�   9   ;           �   8   :          W          Date.parse(JSON.parse(File.read(path))["created_at"]).to_s == Date.today.to_s�   7   9          H        file_exists_and_is_less_than_one_day_old = File.exists?(path) &&�   6   8           �   5   7          (        path = "tmp/fb_access_token.txt"�   4   6          #      def get_facebook_access_token�   3   5           �   2   4          	      end�   1   3          0        yield token, HashWithQuickAccess.new(me)�   0   2           �   /   1          	        )�   .   0                    ).body�   -   /          1            "Authorization" => "Bearer #{token}",�   ,   .          '            "http://localhost:3000/me",�   +   -                    RestClient.get(�   *   ,                  me = JSON.parse(�   )   +           �   (   *          .        token = JSON.parse(auth.body)["token"]�   '   )           �   &   (          	        )�   %   '          *          facebook_access_token: fb_token,�   $   &                    facebook_id: fb_id,�   #   %          .          "http://localhost:3000/oauth/token",�   "   $                  auth = RestClient.post(�   !   #           �       "                  fb_id = response["id"]�      !          7        response = JSON.parse(RestClient.get(url).body)�                 F        url = "https://graph.facebook.com/me?access_token=#{fb_token}"�                 �                ,        fb_token = get_facebook_access_token�                      def authorize�                 �                      private�                 �                	      end�                        end�                          end�                )            x.report(name.to_s) { yield }�                           ::Benchmark.ips do |x|�                $        task name => :environment do�                      def test(name)�                 �                	      end�                        end�                          end�                            end�                5              x.report(name.to_s) { yield token, me }�   
             "            ::Benchmark.ips do |x|�   	             "          authorize do |token, me|�      
          $        task name => :environment do�      	                def test_endpoint(name)�                 �                      include Rake::DSL�                    class << self�                  class Benchmark�                module Tonsser�                 �                 require "rake"5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             T��8     �              G   # require "rake"       # module Tonsser   #   class Benchmark   #     class << self   #       include Rake::DSL       #       def test_endpoint(name)   &#         task name => :environment do   $#           authorize do |token, me|   $#             ::Benchmark.ips do |x|   7#               x.report(name.to_s) { yield token, me }   #             end   #           end   #         end   #       end       #       def test(name)   &#         task name => :environment do   "#           ::Benchmark.ips do |x|   +#             x.report(name.to_s) { yield }   #           end   #         end   #       end       #       private       #       def authorize   .#         fb_token = get_facebook_access_token       H#         url = "https://graph.facebook.com/me?access_token=#{fb_token}"   9#         response = JSON.parse(RestClient.get(url).body)    #         fb_id = response["id"]       !#         auth = RestClient.post(   0#           "http://localhost:3000/oauth/token",   #           facebook_id: fb_id,   ,#           facebook_access_token: fb_token,   #         )       0#         token = JSON.parse(auth.body)["token"]       #         me = JSON.parse(   #           RestClient.get(   )#             "http://localhost:3000/me",   3#             "Authorization" => "Bearer #{token}",   #           ).body   #         )       2#         yield token, HashWithQuickAccess.new(me)   #       end       %#       def get_facebook_access_token   *#         path = "tmp/fb_access_token.txt"       J#         file_exists_and_is_less_than_one_day_old = File.exists?(path) &&   Y#           Date.parse(JSON.parse(File.read(path))["created_at"]).to_s == Date.today.to_s       5#         if file_exists_and_is_less_than_one_day_old   0#           JSON.parse(File.read(path))["token"]   #         else   N#           print "Enter facebook access token (will be cached for one day): "   $#           token = STDIN.gets.chomp   C#           json = { created_at: Date.today, token: token }.to_json   "#           File.write(path, json)   #           token   #         end   #       end   	#     end   #   end   # end5�_�                            ����                                                                                                                                                                                                                                                                                                                                                             T��9     �               �               �               5�_�                            ����                                                                                                                                                                                                                                                                                                                                                             T��:     �                  5�_�                            ����                                                                                                                                                                                                                                                                                                                                                             T��:     �                  5�_�                             ����                                                                                                                                                                                                                                                                                                                                                             T��<    �   �   �          end�   �   �            end�   �   �              end�   �   �          	      end�   �   �                  end�   �   �                    x.compare!�   �   �          !        if @benchmarks.length > 1�   �   �           �   �   �                  end�   �   �                    end�   �   �                      end�   �   �          !              benchmark.last.call�   �   �                      rollback_after do�   �   �          +          x.report(benchmark.first.to_s) do�   �   �          '        @benchmarks.each do |benchmark|�   �   �                ::Benchmark.ips do |x|�   �   �              def benchmark_and_compare!�   �   �           �   �   �              end�   �   �          	      end�   �   �          %        send(verb, path, all_options)�   �   �          O        all_options = { "Authorization" => "Bearer #{token}", }.merge!(options)�   �   �          F      define_method("authorized_#{verb}") do |token, path, options={}|�      �           �   ~   �          #      delegate verb, to: RestClient�   }             7    [:get, :post, :patch, :put, :delete].each do |verb|�   |   ~          O    # Define methods `get`, `authorized_get`, `post`, `authorized_post`, etc...�   {   }           �   z   |              end�   y   {                @count += 1�   x   z          <      @benchmarks << [(desc || "#{@name}_#{@count}"), block]�   w   y          %    def benchmark(desc = nil, &block)�   v   x           �   u   w              end�   t   v                @count = 1�   s   u                @benchmarks = []�   r   t                @name = name�   q   s              def initialize(name)�   p   r           �   o   q          *    include AutoRollbackTransactionMethods�   n   p          (    include FactoryGirl::Syntax::Methods�   m   o            class Runner�   l   n           �   k   m            end�   j   l              end�   i   k          	      end�   h   j                  end�   g   i                    token�   f   h                     File.write(path, json)�   e   g          A          json = { created_at: Date.today, token: token }.to_json�   d   f          "          token = STDIN.gets.chomp�   c   e          L          print "Enter facebook access token (will be cached for one day): "�   b   d                  else�   a   c          .          JSON.parse(File.read(path))["token"]�   `   b          3        if file_exists_and_is_less_than_one_day_old�   _   a           �   ^   `          W          Date.parse(JSON.parse(File.read(path))["created_at"]).to_s == Date.today.to_s�   ]   _          H        file_exists_and_is_less_than_one_day_old = File.exists?(path) &&�   \   ^           �   [   ]          (        path = "tmp/fb_access_token.txt"�   Z   \          #      def get_facebook_access_token�   Y   [           �   X   Z          	      end�   W   Y          0        yield token, HashWithQuickAccess.new(me)�   V   X           �   U   W          	        )�   T   V                    ).body�   S   U          1            "Authorization" => "Bearer #{token}",�   R   T          '            "http://localhost:3000/me",�   Q   S                    RestClient.get(�   P   R                  me = JSON.parse(�   O   Q           �   N   P          .        token = JSON.parse(auth.body)["token"]�   M   O           �   L   N          	        )�   K   M          *          facebook_access_token: fb_token,�   J   L                    facebook_id: fb_id,�   I   K          .          "http://localhost:3000/oauth/token",�   H   J                  auth = RestClient.post(�   G   I           �   F   H                  end�   E   G                    retry�   D   F          4          FileUtils.rm_rf("tmp/fb_access_token.txt")�   C   E          %        rescue RestClient::BadRequest�   B   D                     fb_id = response["id"]�   A   C          9          response = JSON.parse(RestClient.get(url).body)�   @   B          H          url = "https://graph.facebook.com/me?access_token=#{fb_token}"�   ?   A          .          fb_token = get_facebook_access_token�   >   @                  begin�   =   ?                def authorize�   <   >           �   ;   =          	      end�   :   <                  end�   9   ;          T          fail "Benchmarks can only run in test environment, you're in #{Rails.env}"�   8   :                  unless Rails.env.test?�   7   9                def ensure_test_env!�   6   8           �   5   7                private�   4   6           �   3   5          	      end�   2   4                  end�   1   3                    end�   0   2          )            runner.benchmark_and_compare!�   /   1           �   .   0                      end�   -   /          5              runner.instance_exec(token, me, &block)�   ,   .                      rollback_after do�   +   -          "          authorize do |token, me|�   *   ,          $        task name => :environment do�   )   +          #        desc %{Benchmark "#{name}"}�   (   *           �   '   )          !        runner = Runner.new(name)�   &   (           �   %   '          "        WebMock.allow_net_connect!�   $   &                  ensure_test_env!�   #   %          %      def test_endpoint(name, &block)�   "   $           �   !   #          	      end�       "                  end�      !          '          runner.benchmark_and_compare!�                  �                          end�                (            runner.instance_eval(&block)�                          rollback_after do�                $        task name => :environment do�                #        desc %{Benchmark "#{name}"}�                 �                !        runner = Runner.new(name)�                 �                "        WebMock.allow_net_connect!�                        ensure_test_env!�                      def test(name, &block)�                 �                ,      include AutoRollbackTransactionMethods�                      include Rake::DSL�                    class << self�                  class Benchmark�                 �                  end�                    end�   
             	      end�   	             $        raise ActiveRecord::Rollback�      
                  yield�      	          '      ActiveRecord::Base.transaction do�                    def rollback_after�                '  module AutoRollbackTransactionMethods�                module Tonsser�                 �                require "benchmark/ips"�                require "factory_girl_rails"�                 require "rake"5�_�                            ����                                                                                                                                                                                                                                                                                                                                                             T��t    �                 require "rake"�                 �                module Tonsser�                  class Benchmark�                    class << self�                      include Rake::DSL�                 �      	                def test_endpoint(name)�      
          $        task name => :environment do�   	             "          authorize do |token, me|�   
             "            ::Benchmark.ips do |x|�                5              x.report(name.to_s) { yield token, me }�                            end�                          end�                        end�                	      end�                 �                      def test(name)�                $        task name => :environment do�                           ::Benchmark.ips do |x|�                )            x.report(name.to_s) { yield }�                          end�                        end�                	      end�                 �                      private�                 �                      def authorize�                ,        fb_token = get_facebook_access_token�                 �                 F        url = "https://graph.facebook.com/me?access_token=#{fb_token}"�      !          7        response = JSON.parse(RestClient.get(url).body)�       "                  fb_id = response["id"]�   !   #           �   "   $                  auth = RestClient.post(�   #   %          .          "http://localhost:3000/oauth/token",�   $   &                    facebook_id: fb_id,�   %   '          *          facebook_access_token: fb_token,�   &   (          	        )�   '   )           �   (   *          .        token = JSON.parse(auth.body)["token"]�   )   +           �   *   ,                  me = JSON.parse(�   +   -                    RestClient.get(�   ,   .          '            "http://localhost:3000/me",�   -   /          1            "Authorization" => "Bearer #{token}",�   .   0                    ).body�   /   1          	        )�   0   2           �   1   3          0        yield token, HashWithQuickAccess.new(me)�   2   4          	      end�   3   5           �   4   6          #      def get_facebook_access_token�   5   7          (        path = "tmp/fb_access_token.txt"�   6   8           �   7   9          H        file_exists_and_is_less_than_one_day_old = File.exists?(path) &&�   8   :          W          Date.parse(JSON.parse(File.read(path))["created_at"]).to_s == Date.today.to_s�   9   ;           �   :   <          3        if file_exists_and_is_less_than_one_day_old�   ;   =          .          JSON.parse(File.read(path))["token"]�   <   >                  else�   =   ?          L          print "Enter facebook access token (will be cached for one day): "�   >   @          "          token = STDIN.gets.chomp�   ?   A          A          json = { created_at: Date.today, token: token }.to_json�   @   B                     File.write(path, json)�   A   C                    token�   B   D                  end�   C   E          	      end�   D   F              end�   E   G            end�   F   H          end5��