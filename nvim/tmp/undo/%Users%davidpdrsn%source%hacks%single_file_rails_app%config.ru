Vim�UnDo� 8��㣍�����:`氦���2�$^z�T�   )                 I       I   I   I    T��{    _�                             ����                                                                                                                                                                                                                                                                                                                                                             T���     �                  �               �               5�_�                            ����                                                                                                                                                                                                                                                                                                                                                             T���    �                  5�_�                    :       ����                                                                                                                                                                                                                                                                                                                                        :       v   :    T��t     �   9   ;   L      %  include ActionController::Rendering5�_�                    :       ����                                                                                                                                                                                                                                                                                                                                        :       v   :    T��u     �   9   :          $  inlude ActionController::Rendering5�_�                   9   /    ����                                                                                                                                                                                                                                                                                                                                        :       v   :    T��x    �   8   :   K      /class HelloController < ActionController::Metal5�_�                    :        ����                                                                                                                                                                                                                                                                                                                                        :       v   :    T���     �   9   :           5�_�      	              4   	    ����                                                                                                                                                                                                                                                                                                                                        :       v   :    T���    �   3   5   J      �  config.secret_token = "49837489qkuweoiuoqwehisuakshdjksadhaisdy78o34y138974xyqp9rmye8yrpiokeuioqwzyoiuxftoyqiuxrhm3iou1hrzmjk"5�_�      
           	   )        ����                                                                                                                                                                                                                                                                                                                           2           )           V       T���     �   (   )       
       7  # Here you could remove some middlewares, for example   =  # Rack::Lock, AD::Flash and AD::BestStandardsSupport below.   8  # The remaining stack is printed on rackup (for fun!).   4  # Rails API has config.middleware.api_only! to get   &  # rid of browser related middleware.   '  config.middleware.delete "Rack::Lock"   2  config.middleware.delete "ActionDispatch::Flash"   A  config.middleware.delete "ActionDispatch::BestStandardsSupport"    5�_�   	              
   )       ����                                                                                                                                                                                                                                                                                                                           )           )           V       T���     �   (   *   @    5�_�   
                 )       ����                                                                                                                                                                                                                                                                                                                           *           *           V       T���     �   (   )            #5�_�                    (       ����                                                                                                                                                                                                                                                                                                                           '          (          V       T���     �   '   )   @    5�_�                    (       ����                                                                                                                                                                                                                                                                                                                           '          )          V       T���     �   '   (            #5�_�                    (       ����                                                                                                                                                                                                                                                                                                                           '          (          V       T���     �   (   *   A        # �   (   *   @    5�_�                    )       ����                                                                                                                                                                                                                                                                                                                           '          (          V       T���     �   (   *   A        # c5�_�                    )       ����                                                                                                                                                                                                                                                                                                                           '          (          V       T���     �   (   *   A        k5�_�                    '       ����                                                                                                                                                                                                                                                                                                                           '          (          V       T���     �   &   '          %  # uncomment below to display errors5�_�                          ����                                                                                                                                                                                                                                                                                                                                                V       T���     �                 ># Port of https://gist.github.com/josevalim/1942658 to Rails 4   # Original author: Jose Valim   #   # Run this file with:   #5�_�                            ����                                                                                                                                                                                                                                                                                                                                                  V        T���     �                #   # And access:   #   %#   http://localhost:3000/hello/world   #   9# We are using Bundler in this example, but we could also   # have used rubygems:   #   #   require "rubygems"   #   #   gem "actionpack"   #   gem "railties"   #   #   require "rails"   #   require "rails/all"       ;# The following lines should come as no surprise. Except by   ;# ActionController::Metal, it follows the same structure of   <# config/application.rb, config/environment.rb and config.ru   :# existing in any Rails 4 app. Here they are simply in one    # file and without the comments.5�_�                           ����                                                                                                                                                                                                                                                                                                                                                  V        T���     �          &      ;#   bundle exec RAILS_ENV=production rackup -p 3000 -s thin5�_�                           ����                                                                                                                                                                                                                                                                                                                                                  V        T���     �          &      :#  bundle exec RAILS_ENV=production rackup -p 3000 -s thin5�_�                          ����                                                                                                                                                                                                                                                                                                                                                  V        T���    �         &      require "rails"5�_�                            ����                                                                                                                                                                                                                                                                                                                                                  V        T��     �                # Print the stack for fun!   *puts ">> Starting Rails lightweight stack"   3Rails.configuration.middleware.each do |middleware|   "  puts "use #{middleware.inspect}"   end   1puts "run #{Rails.application.class.name}.routes"    5�_�                            ����                                                                                                                                                                                                                                                                                                                                                  V        T��     �                "# Run it (originally in config.ru)5�_�                            ����                                                                                                                                                                                                                                                                                                                                                  V        T��     �                :# Initialize the app (originally in config/environment.rb)5�_�                            ����                                                                                                                                                                                                                                                                                                                                                  V        T��    �                 5�_�                            ����                                                                                                                                                                                                                                                                                                                                                  V        T��     �                F# This is a barebone controller. One good reference can be found here:   J# http://piotrsarnacki.com/2010/12/12/lightweight-controllers-with-rails3/5�_�                             ����                                                                                                                                                                                                                                                                                                                                                  V        T��     �                5  # We need a secret token for session, cookies, etc.5�_�      !                       ����                                                                                                                                                                                                                                                                                                                                                  V        T��      �                    -  # config.consider_all_requests_local = true5�_�       "           !   
        ����                                                                                                                                                                                                                                                                                                                                                  V        T��"   	 �   	   
          +  # Enable cache classes. Production style.5�_�   !   #           "      #    ����                                                                                                                                                                                                                                                                                                                                                  V        T��%   
 �               <require "action_controller/railtie" # require more if needed5�_�   "   $           #      "    ����                                                                                                                                                                                                                                                                                                                                                  V        T��K     �      	             �      	       5�_�   #   %           $          ����                                                                                                                                                                                                                                                                                                                                                  V        T��N     �      	             root to: ""5�_�   $   &           %          ����                                                                                                                                                                                                                                                                                                                                                  V        T��S    �                '    get "/hello/world" => "hello#world"5�_�   %   '           &          ����                                                                                                                                                                                                                                                                                                                                                  V        T��r     �             5�_�   &   (           '          ����                                                                                                                                                                                                                                                                                                                                                  V        T��r     �                 �             5�_�   '   )           (          ����                                                                                                                                                                                                                                                                                                                                                  V        T��|     �                   �             �                 �             5�_�   (   *           )      
    ����                                                                                                                                                                                                                                                                                                                                                  V        T���     �                   puts ""5�_�   )   +           *      !    ����                                                                                                                                                                                                                                                                                                                                                  V        T���    �               #    puts "---------- one ---------"5�_�   *   ,           +          ����                                                                                                                                                                                                                                                                                                                                                  V        T���     �             �             5�_�   +   -           ,          ����                                                                                                                                                                                                                                                                                                                                                  V        T���     �                 before_action :one5�_�   ,   .           -           ����                                                                                                                                                                                                                                                                                                                                                V       T���     �             �             5�_�   -   /           .          ����                                                                                                                                                                                                                                                                                                                                                V       T���     �         "    5�_�   .   0           /          ����                                                                                                                                                                                                                                                                                                                                                V       T���     �         #      %    puts "---------- one -----------"5�_�   /   1           0          ����                                                                                                                                                                                                                                                                                                                                                V       T���     �         #    �         #    5�_�   0   2           1          ����                                                                                                                                                                                                                                                                                                                                                V       T���     �         $        before_action :two5�_�   1   3           2           ����                                                                                                                                                                                                                                                                                                                                          	       V       T���     �       $   $    �       !   $    5�_�   2   4           3   !       ����                                                                                                                                                                                                                                                                                                                                          	       V       T���     �       "   '    5�_�   3   5           4   "   	    ����                                                                                                                                                                                                                                                                                                                                          	       V       T���     �   !   #   (      	  def one5�_�   4   6           5          ����                                                                                                                                                                                                                                                                                                                                          	       V       T���     �         (      	  def one5�_�   5   7           6   "       ����                                                                                                                                                                                                                                                                                                                                          	       V       T���     �   !   #   (      	  def two5�_�   6   8           7   #       ����                                                                                                                                                                                                                                                                                                                                          	       V       T���    �   "   $   (      %    puts "---------- two -----------"5�_�   7   9           8          ����                                                                                                                                                                                                                                                                                                                                          	       V       T���    �         (        before_action :none5�_�   8   :           9          ����                                                                                                                                                                                                                                                                                                                                          	       V       T���     �                %    puts "---------- one -----------"5�_�   9   ;           :          ����                                                                                                                                                                                                                                                                                                                                          	       V       T���    �         )          # �         (    5�_�   :   <           ;          ����                                                                                                                                                                                                                                                                                                                            !             	       V       T���     �                	    false5�_�   ;   =           <          ����                                                                                                                                                                                                                                                                                                                            !             	       V       T���     �         *          # �         )    5�_�   <   >           =          ����                                                                                                                                                                                                                                                                                                                            "              	       V       T���    �         *          render text: ""5�_�   =   B           >          ����                                                                                                                                                                                                                                                                                                                                                       T��Y    �         *        _action :one�         *        before_action :one     before_action :two     before_action :three5�_�   >   C   ?       B          ����                                                                                                                                                                                                                                                                                                                                                       T��k     �                !    render text: "Hi from filter"5�_�   B   D           C           ����                                                                                                                                                                                                                                                                                                                                                V       T��l    �                    # false�                '    # puts "---------- one -----------"5�_�   C   E           D           ����                                                                                                                                                                                                                                                                                                                                                V       T��u     �                  after_action :one5�_�   D   F           E          ����                                                                                                                                                                                                                                                                                                                                                V       T��v     �         (    �         (    5�_�   E   G           F          ����                                                                                                                                                                                                                                                                                                                                                V       T��w     �                  after_action :three5�_�   F   H           G          ����                                                                                                                                                                                                                                                                                                                                                V       T��w     �         (    �         (    5�_�   G   I           H          ����                                                                                                                                                                                                                                                                                                                                                V       T��z     �                  after_action :three5�_�   H               I           ����                                                                                                                                                                                                                                                                                                                                                V       T��z    �         (    �         (    5�_�   >   @       B   ?          ����                                                                                                                                                                                                                                                                                                                                                       T��\     �              5�_�   ?   A           @          ����                                                                                                                                                                                                                                                                                                                                                       T��\     �                %    puts "---------- one -----------"5�_�   @               A          ����                                                                                                                                                                                                                                                                                                                                                       T��^     �                '    # puts "---------- one -----------"5�_�                           ����                                                                                                                                                                                                                                                                                                                                                  V        T���     �              5�_�                            ����                                                                                                                                                                                                                                                                                                                                                  V        T���    �         %      require "rails/all"5�_�                           ����                                                                                                                                                                                                                                                                                                                                               V       T���     �              5�_�                    9       ����                                                                                                                                                                                                                                                                                                                                        :       v   :    T��v     �   8   :   K      .lass HelloController < ActionController::Metal�   7   :   K      x# http://piotrsarnacki.com/2010/12/12/lightweight-controllers-with-rails3/lass HelloController < ActionController::Metal5��