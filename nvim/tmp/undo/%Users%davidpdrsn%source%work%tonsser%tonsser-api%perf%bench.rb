Vim�UnDo� <ז��Ҡ8ιqb�g��j�F���?�3��z��D   �                 +       +   +   +    VJ�    _�                     !       ����                                                                                                                                                                                                                                                                                                                                                             V2A2     �   !   $   �            �   !   #   �    5�_�                    #       ����                                                                                                                                                                                                                                                                                                                                                             V2A6     �   #   %   �    5�_�                    "       ����                                                                                                                                                                                                                                                                                                                            "          #          V       V2A9     �   !   #   �            puts url5�_�                    #       ����                                                                                                                                                                                                                                                                                                                            "          #          V       V2A:    �   "   $   �            puts headers5�_�                    "        ����                                                                                                                                                                                                                                                                                                                            "          #          V       V2Bf     �               �   $require File.expand_path(ARGV.first)   require "factory_girl_rails"   require "net/http"   require "uri"       module Tonsser     class ApacheBench       def initialize(token)   !      @config = Configuration.new         @token = token       end           def configure         yield config       end           def run         if show_sample_response?   '        execute_and_show_sample_request   	      end       $      `ab -n #{number_of_requests} \             -c #{concurrency} \             #{headers_to_args} \             #{url} \             2> /dev/null`       end           private       4    attr_reader :number_of_requests, :config, :token       '    def execute_and_show_sample_request         pp url         pp headers       -      response = RestClient::Request.execute(           method: :get,           url: url,           headers: headers         )   !      json = JSON.parse(response)         pp json       end           def show_sample_response?   !      config.show_sample_response       end           def headers   @      config.headers || { "Authorization" => "Bearer #{token}" }       end           def concurrency         config.concurrency || 1       end           def number_of_requests   &      config.number_of_requests || 100       end           def url         config.url       end           def headers_to_args   /      headers.reduce("") do |acc, (key, value)|   &        "#{acc} -H '#{key}: #{value}'"   	      end       end           class Configuration         def initialize           @config = {}   	      end             attr_reader :config             def headers           config[:headers]   	      end             def headers=(headers)   "        config[:headers] = headers   	      end             def concurrency           config[:concurrency]   	      end       #      def concurrency=(concurrency)   *        config[:concurrency] = concurrency   	      end             def number_of_requests   #        config[:number_of_requests]   	      end              def number_of_requests=(n)   '        config[:number_of_requests] = n   	      end             def url           config.fetch(:url)   	      end             def url=(url)           config[:url] = url.to_s   	      end             def show_sample_response   %        config[:show_sample_response]   	      end             def show_sample_response!   ,        config[:show_sample_response] = true   	      end       end     end         module Benchmark       module Auth         class << self   %        def get_facebook_access_token   *          path = "tmp/fb_access_token.txt"       4          file_exists_and_is_less_than_one_day_old =   @            File.exists?(path) && fb_access_token_not_old?(path)       5          if file_exists_and_is_less_than_one_day_old   0            JSON.parse(File.read(path))["token"]             else   N            print "Enter facebook access token (will be cached for one day): "   $            token = STDIN.gets.chomp   C            json = { created_at: Date.today, token: token }.to_json   "            File.write(path, json)               token             end           end               def authorize   .          fb_token = get_facebook_access_token   I          user = FactoryGirl.create :user, facebook_id: "797254413650117"             token = JSON.parse(               RestClient.post(   2              "http://localhost:6666/oauth/token",   ,              facebook_id: user.facebook_id,   .              facebook_access_token: fb_token,               ).body             ).fetch("token")             yield token, user           end               private       *        def fb_access_token_not_old?(path)   @          created_at = JSON.parse(File.read(path))["created_at"]   8          Date.parse(created_at).to_s == Date.today.to_s           end   	      end       end     end         class HttpRequestFactory        def get(path, options = nil)   6      url = "http://localhost:6666/#{clean_url(path)}"         if options.present?   -        url += "?#{to_query_params(options)}"   	      end   	      url       end           private            def to_query_params(options)   >      options.map { |key, value| "#{key}=#{value}" }.join("&")       end           def clean_url(url)         url.sub(/^\//, "")       end     end   end       Rails.cache.clear       3Tonsser::Benchmark::Auth.authorize do |token, user|   @  klass_name = File.basename(ARGV.first).sub(".rb", "").camelize   ;  benchmark_class = "Benchmarks::#{klass_name}".constantize   0  apache_bench = Tonsser::ApacheBench.new(token)   "  benchmark = benchmark_class.new(       user: user,       token: token,       benchmark: apache_bench,       factory: FactoryGirl,   *    http: Tonsser::HttpRequestFactory.new,     )     benchmark.run     output = apache_bench.run     puts output   C  time = ApacheBenchOutputParser.new(output).parse.time_per_request   %  puts "TIME PER REQUEST: #{time} ms"   end5�_�                    �        ����                                                                                                                                                                                                                                                                                                                                                             VDSI    �   �   �            puts output5�_�                    �        ����                                                                                                                                                                                                                                                                                                                                                             VDS{    �   �   �            # puts output5�_�      	              �        ����                                                                                                                                                                                                                                                                                                                                                             VI�>    �   �   �   �        �   �   �   �    5�_�      
           	   �        ����                                                                                                                                                                                                                                                                                                                            �          �          V       VI�P     �   �   �          %  puts "TIME PER REQUEST: #{time} ms"�   �   �          C  time = ApacheBenchOutputParser.new(output).parse.time_per_request5�_�   	              
   �       ����                                                                                                                                                                                                                                                                                                                            �          �          V       VI�Q    �   �   �   �        puts output5�_�   
                 �       ����                                                                                                                                                                                                                                                                                                                            �          �          V       VI��    �   �   �   �        pp output5�_�                    �       ����                                                                                                                                                                                                                                                                                                                            �          �          V       VI��     �   �   �            puts output5�_�                    �       ����                                                                                                                                                                                                                                                                                                                            �          �          V       VI��    �   �   �          '  # puts "TIME PER REQUEST: #{time} ms"�   �   �          E  # time = ApacheBenchOutputParser.new(output).parse.time_per_request5�_�                    0       ����                                                                                                                                                                                                                                                                                                                                                             VJ�L     �   /   1   �    �   0   1   �    5�_�                   0       ����                                                                                                                                                                                                                                                                                                                            0          0          v       VJ�V   	 �   /   1   �      @      config.headers || { "Authorization" => "Bearer #{token}" }5�_�                          ����                                                                                                                                                                                                                                                                                                                                                V       VJ�_     �          �    �         �    5�_�                           ����                                                                                                                                                                                                                                                                                                                                                V       VJ�_     �         �    5�_�                       
    ����                                                                                                                                                                                                                                                                                                                               
          
       V   
    VJ�a     �                          2> /dev/null`�                          #{url} \�                          #{headers_to_args} \�                          -c #{concurrency} \�                $      `ab -n #{number_of_requests} \5�_�                           ����                                                                                                                                                                                                                                                                                                                               
          
       V   
    VJ�c     �         �      $      `ab -n #{number_of_requests} \5�_�                            ����                                                                                                                                                                                                                                                                                                                               
          
       V   
    VJ�e     �      !   �                2> /dev/null`5�_�                            ����                                                                                                                                                                                                                                                                                                                               
          
       V   
    VJ�g     �      !   �                2> /dev/null5�_�                           ����                                                                                                                                                                                                                                                                                                                               
          
       V   
    VJ�h     �          �                #{url} \5�_�                           ����                                                                                                                                                                                                                                                                                                                               
          
       V   
    VJ�i     �         �                #{headers_to_args} \5�_�                           ����                                                                                                                                                                                                                                                                                                                               
          
       V   
    VJ�i     �         �                -c #{concurrency} \5�_�                       $    ����                                                                                                                                                                                                                                                                                                                               
          
       V   
    VJ�j     �         �      $      "ab -n #{number_of_requests} \5�_�                       !    ����                                                                                                                                                                                                                                                                                                                               
          
       V   
    VJ�j     �         �      "      "ab -n #{number_of_requests}             -c #{concurrency}5�_�                       "    ����                                                                                                                                                                                                                                                                                                                               
          
       V   
    VJ�j     �         �      4      "ab -n #{number_of_requests} -c #{concurrency}             #{headers_to_args}5�_�                       4    ����                                                                                                                                                                                                                                                                                                                               
          
       V   
    VJ�j     �         �      G      "ab -n #{number_of_requests} -c #{concurrency} #{headers_to_args}             #{url}5�_�      !                 G    ����                                                                                                                                                                                                                                                                                                                               
          
       V   
    VJ�k     �         �      N      "ab -n #{number_of_requests} -c #{concurrency} #{headers_to_args} #{url}             2> /dev/null"5�_�      "           !           ����                                                                                                                                                                                                                                                                                                                               
          
       V   
    VJ�n     �                 �                          2> /dev/null`�                          #{url} \�                          #{headers_to_args} \�                          -c #{concurrency} \�                $      `ab -n #{number_of_requests} \5�_�   !   #           "           ����                                                                                                                                                                                                                                                                                                                               
          
       V   
    VJ�p     �                 �                          2> /dev/null`�                          #{url} \�                          #{headers_to_args} \�                          -c #{concurrency} \�                $      `ab -n #{number_of_requests} \5�_�   "   $           #           ����                                                                                                                                                                                                                                                                                                                               
          
       V   
    VJ�q     �                 �                          2> /dev/null`�                          #{url} \�                          #{headers_to_args} \�                          -c #{concurrency} \�                $      `ab -n #{number_of_requests} \5�_�   #   %           $           ����                                                                                                                                                                                                                                                                                                                                                  V        VJ�s     �                          2> /dev/null`�                          #{url} \�                          #{headers_to_args} \�                          -c #{concurrency} \�                $      `ab -n #{number_of_requests} \5�_�   $   &           %           ����                                                                                                                                                                                                                                                                                                                                                  V        VJ�t     �         �    5�_�   %   '           &           ����                                                                                                                                                                                                                                                                                                                                                  V        VJ�v     �                 5�_�   &   )           '          ����                                                                                                                                                                                                                                                                                                                                                       VJ�x    �         �                -c #{concurrency} \             #{headers_to_args} \             #{url} \             2> /dev/null`�         �      $      `ab -n #{number_of_requests} \5�_�   '   *   (       )   2       ����                                                                                                                                                                                                                                                                                                                                                V       VJ��     �   1   3   �      1      pp { "Authorization" => "Bearer #{token}" }5�_�   )   +           *   2   1    ����                                                                                                                                                                                                                                                                                                                                                V       VJ��    �   1   3   �      1      pp({ "Authorization" => "Bearer #{token}" }5�_�   *               +           ����                                                                                                                                                                                                                                                                                                                                                V       VJ�~     �               �   $require File.expand_path(ARGV.first)   require "factory_girl_rails"   require "net/http"   require "uri"       module Tonsser     class ApacheBench       def initialize(token)   !      @config = Configuration.new         @token = token       end           def configure         yield config       end           def run         if show_sample_response?   '        execute_and_show_sample_request   	      end       &      # `ab -n #{number_of_requests} \         #     -c #{concurrency} \          #     #{headers_to_args} \         #     #{url} \         #     2> /dev/null`       \      "ab -n #{number_of_requests} -c #{concurrency} #{headers_to_args} #{url} 2> /dev/null"       end           private       4    attr_reader :number_of_requests, :config, :token       '    def execute_and_show_sample_request   -      response = RestClient::Request.execute(           method: :get,           url: url,           headers: headers         )   !      json = JSON.parse(response)         pp json       end           def show_sample_response?   !      config.show_sample_response       end           def headers   2      pp({ "Authorization" => "Bearer #{token}" })   @      config.headers || { "Authorization" => "Bearer #{token}" }       end           def concurrency         config.concurrency || 1       end           def number_of_requests   &      config.number_of_requests || 100       end           def url         config.url       end           def headers_to_args   /      headers.reduce("") do |acc, (key, value)|   &        "#{acc} -H '#{key}: #{value}'"   	      end       end           class Configuration         def initialize           @config = {}   	      end             attr_reader :config             def headers           config[:headers]   	      end             def headers=(headers)   "        config[:headers] = headers   	      end             def concurrency           config[:concurrency]   	      end       #      def concurrency=(concurrency)   *        config[:concurrency] = concurrency   	      end             def number_of_requests   #        config[:number_of_requests]   	      end              def number_of_requests=(n)   '        config[:number_of_requests] = n   	      end             def url           config.fetch(:url)   	      end             def url=(url)           config[:url] = url.to_s   	      end             def show_sample_response   %        config[:show_sample_response]   	      end             def show_sample_response!   ,        config[:show_sample_response] = true   	      end       end     end         module Benchmark       module Auth         class << self   %        def get_facebook_access_token   *          path = "tmp/fb_access_token.txt"       4          file_exists_and_is_less_than_one_day_old =   @            File.exists?(path) && fb_access_token_not_old?(path)       5          if file_exists_and_is_less_than_one_day_old   0            JSON.parse(File.read(path))["token"]             else   N            print "Enter facebook access token (will be cached for one day): "   $            token = STDIN.gets.chomp   C            json = { created_at: Date.today, token: token }.to_json   "            File.write(path, json)               token             end           end               def authorize   .          fb_token = get_facebook_access_token   I          user = FactoryGirl.create :user, facebook_id: "797254413650117"             token = JSON.parse(               RestClient.post(   2              "http://localhost:6666/oauth/token",   ,              facebook_id: user.facebook_id,   .              facebook_access_token: fb_token,               ).body             ).fetch("token")             yield token, user           end               private       *        def fb_access_token_not_old?(path)   @          created_at = JSON.parse(File.read(path))["created_at"]   8          Date.parse(created_at).to_s == Date.today.to_s           end   	      end       end     end         class HttpRequestFactory        def get(path, options = nil)   6      url = "http://localhost:6666/#{clean_url(path)}"         if options.present?   -        url += "?#{to_query_params(options)}"   	      end   	      url       end           private            def to_query_params(options)   >      options.map { |key, value| "#{key}=#{value}" }.join("&")       end           def clean_url(url)         url.sub(/^\//, "")       end     end   end       Rails.cache.clear       3Tonsser::Benchmark::Auth.authorize do |token, user|   @  klass_name = File.basename(ARGV.first).sub(".rb", "").camelize   ;  benchmark_class = "Benchmarks::#{klass_name}".constantize   0  apache_bench = Tonsser::ApacheBench.new(token)   "  benchmark = benchmark_class.new(       user: user,       token: token,       benchmark: apache_bench,       factory: FactoryGirl,   *    http: Tonsser::HttpRequestFactory.new,     )     benchmark.run     output = apache_bench.run   C  time = ApacheBenchOutputParser.new(output).parse.time_per_request   %  puts "TIME PER REQUEST: #{time} ms"   end5�_�   '           )   (          ����                                                                                                                                                                                                                                                                                                                                                V       VJ�z     �                       # if show_sample_response?�                )      #   execute_and_show_sample_request�                      # end5�_�              !              ����                                                                                                                                                                                                                                                                                                                               
          
       V   
    VJ�m     �         �    �         �      $      `ab -n #{number_of_requests} \             -c #{concurrency} \             #{headers_to_args} \             #{url} \             2> /dev/null`5�_�                           ����                                                                                                                                                                                                                                                                                                                                                V       VJ�^     �         �    �         �      $      `ab -n #{number_of_requests} \             -c #{concurrency} \             #{headers_to_args} \             #{url} \             2> /dev/null`5�_�                    0       ����                                                                                                                                                                                                                                                                                                                            0          0          v       VJ�P     �   0   1   �    �   /   1   �      Q      cconfig.headers ||onfig.headers || { "Authorization" => "Bearer #{token}" }5��