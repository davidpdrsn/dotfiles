Vim�UnDo� Lx�	�����-f��w�����W�࿺�PRK                                     UU�j    _�                    O        ����                                                                                                                                                                                                                                                                                                                                                  V        UU�4     �   N   O          K  # Run specs in random order to surface order dependencies. If you find an   M  # order dependency and want to debug it, you can fix the order by providing   .  # the seed, which is printed after each run.     #     --seed 1234     config.order = :random    5�_�                    T       ����                                                                                                                                                                                                                                                                                                                                                  V        UU�5     �   T   [   U    �   T   U   U    5�_�                    U       ����                                                                                                                                                                                                                                                                                                                                                  V        UU�6     �   T   V   [    5�_�                    U       ����                                                                                                                                                                                                                                                                                                                                                  V        UU�7     �   T   V   \        #5�_�                    [        ����                                                                                                                                                                                                                                                                                                                                                  V        UU�8    �   Z   [           5�_�                    *        ����                                                                                                                                                                                                                                                                                                                            Y           *           V       UU�`     �   )   *       0       G# The settings below are suggested to provide a good initial experience   A# with RSpec, but feel free to customize to your heart's content.   =begin   E  # These two settings work together to allow you to limit a spec run   H  # to individual examples or groups you care about by tagging them with   I  # `:focus` metadata. When nothing is tagged with `:focus`, all examples     # get run.     config.filter_run :focus   0  config.run_all_when_everything_filtered = true       H  # Limits the available syntax to the non-monkey patched syntax that is   '  # recommended. For more details, see:   L  #   - http://myronmars.to/n/dev-blog/2012/06/rspecs-new-expectation-syntax   S  #   - http://teaisaweso.me/blog/2013/05/27/rspecs-new-message-expectation-syntax/   �  #   - http://myronmars.to/n/dev-blog/2014/05/notable-changes-in-rspec-3#new__config_option_to_disable_rspeccore_monkey_patching   !  config.disable_monkey_patching!       J  # This setting enables warnings. It's recommended, but in some cases may   /  # be too noisy due to issues in dependencies.     config.warnings = true       J  # Many RSpec users commonly either run the entire suite or an individual   F  # file, and it's useful to allow more verbose output when running an     # individual spec file.     if config.files_to_run.one?   :    # Use the documentation formatter for detailed output,   4    # unless a formatter has already been configured   %    # (e.g. via a command-line flag).   $    config.default_formatter = 'doc'     end       ;  # Print the 10 slowest examples and example groups at the   @  # end of the spec run, to help surface which specs are running     # particularly slow.     config.profile_examples = 10       L  # Seed global randomization in this process using the `--seed` CLI option.   J  # Setting this allows you to use `--seed` to deterministically reproduce   M  # test failures related to randomization by passing the same `--seed` value   *  # as the one that triggered the failure.     Kernel.srand config.seed   =end       K  # Run specs in random order to surface order dependencies. If you find an   M  # order dependency and want to debug it, you can fix the order by providing   .  # the seed, which is printed after each run.     #     --seed 12345�_�                    )       ����                                                                                                                                                                                                                                                                                                                            *           *           V       UU�a     �   )   +   +    5�_�                    %       ����                                                                                                                                                                                                                                                                                                                            '          %          V       UU�c     �   $   %          K    # Prevents you from mocking or stubbing a method that does not exist on   G    # a real object. This is generally recommended, and will default to       # `true` in RSpec 4.5�_�                    "       ����                                                                                                                                                                                                                                                                                                                            %          %          V       UU�d     �   !   "          F  # rspec-mocks config goes here. You can use an alternate test double   M  # library (such as bogus or mocha) by changing the `mock_with` option here.5�_�                           ����                                                                                                                                                                                                                                                                                                                                                V       UU�f     �                O    # This option will default to `true` in RSpec 4. It makes the `description`   N    # and `failure_message` of custom matchers include text for helper methods   "    # defined using `chain`, e.g.:   ;    #     be_bigger_than(2).and_smaller_than(4).description   4    #     # => "be bigger than 2 and smaller than 4"       # ...rather than:   !    #     # => "be bigger than 2"5�_�                           ����                                                                                                                                                                                                                                                                                                                                                V       UU�f     �                A  # rspec-expectations config goes here. You can use an alternate   F  # assertion/expectation library such as wrong or the stdlib/minitest     # assertions if you prefer.5�_�                             ����                                                                                                                                                                                                                                                                                                                                                 V        UU�i    �                 L# This file was generated by the `rspec --init` command. Conventionally, all   L# specs live under a `spec` directory, which RSpec adds to the `$LOAD_PATH`.   O# The generated `.rspec` file contains `--require spec_helper` which will cause   O# this file to always be loaded, without a need to explicitly require it in any   # files.   #   I# Given that it is always loaded, you are encouraged to keep this file as   M# light-weight as possible. Requiring heavyweight dependencies from this file   M# will add to the boot time of your test suite on EVERY test run, even for an   P# individual file that may not need all of that loaded. Instead, consider making   O# a separate helper file that requires the additional dependencies and performs   M# the additional setup, and require it from the spec files that actually need   # it.   #   L# The `.rspec` file also contains a few flags that are not defaults but that   # users commonly want.   #   B# See http://rubydoc.info/gems/rspec-core/RSpec/Core/Configuration5�_�                            ����                                                                                                                                                                                                                                                                                                                                                  V        UU�     �               5�_�                            ����                                                                                                                                                                                                                                                                                                                                      	          V       UU�     �              5�_�                            ����                                                                                                                                                                                                                                                                                                                                                V       UU�     �      
        5�_�                           ����                                                                                                                                                                                                                                                                                                                                      	          V       UU�     �              5�_�                            ����                                                                                                                                                                                                                                                                                                                            9           3           V        UU�	     �      
        5�_�      	             3        ����                                                                                                                                                                                                                                                                                                                            1           	           V        UU�     �   2   :        5�_�      
           	   	        ����                                                                                                                                                                                                                                                                                                                            	           	           V        UU�     �      2        5�_�   	               
          ����                                                                                                                                                                                                                                                                                                                            
           
           V        UU�     �      	   
       5�_�                   
        ����                                                                                                                                                                                                                                                                                                                            
          
           V        UU�     �   	   :        5�_�                     	        ����                                                                                                                                                                                                                                                                                                                            	          	           V        UU�     �      
        5��