Vim�UnDo� T���iY:~�U �2�qt��Oi��Pt���                                      U��&    _�                             ����                                                                                                                                                                                                                                                                                                                                                             U��#     �              =   # server-based syntax   # ======================   G# Defines a single server with a list of roles and multiple properties.   =# You can define all roles on a single server, or split them:       U# server 'example.com', user: 'deploy', roles: %w{app db web}, my_property: :my_value   X# server 'example.com', user: 'deploy', roles: %w{app web}, other_property: :other_value   8# server 'db.example.com', user: 'deploy', roles: %w{db}               # role-based syntax   # ==================       I# Defines a role with one or multiple servers. The primary server in each   H# group is considered to be the first unless any  hosts have the primary   G# property set. Specify the username and a domain or IP for the server.   %# Don't use `:all`, it's a meta role.       ;# role :app, %w{deploy@example.com}, my_property: :my_value   U# role :web, %w{user1@primary.com user2@additional.com}, other_property: :other_value   ## role :db,  %w{deploy@example.com}               # Configuration   # =============   A# You can set any configuration variable like in config/deploy.rb   =# These variables are then only loaded and set in this stage.   N# For available Capistrano configuration variables see the documentation page.   F# http://capistranorb.com/documentation/getting-started/configuration/   9# Feel free to add new variables to customise your setup.               # Custom SSH Options   # ==================   E# You may pass any option but keep in mind that net/ssh understands a   =# limited set of options, consult the Net::SSH documentation.   F# http://net-ssh.github.io/net-ssh/classes/Net/SSH.html#method-c-start   #   # Global options   # --------------   #  set :ssh_options, {   +#    keys: %w(/home/rlisowski/.ssh/id_rsa),   #    forward_agent: false,   #    auth_methods: %w(password)   #  }   #   :# The server-based syntax can be used to override options:   &# ------------------------------------   # server 'example.com',   #   user: 'user_name',   #   roles: %w{web app},   #   ssh_options: {   7#     user: 'user_name', # overrides user setting above   ,#     keys: %w(/home/user_name/.ssh/id_rsa),   #     forward_agent: false,   *#     auth_methods: %w(publickey password)   ##     # password: 'please use keys'   #   }5�_�                            ����                                                                                                                                                                                                                                                                                                                                                             U��$     �               �               �               5�_�                            ����                                                                                                                                                                                                                                                                                                                                                             U��%     �                  5�_�                             ����                                                                                                                                                                                                                                                                                                                                                             U��%    �                  5��