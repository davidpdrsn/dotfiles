Vim�UnDo� A�	���F��'@kJl'�T ����ev��E�   O   ,    get "search/moves", to: "searches#moves"   D   &      D       D   D   D    U���    _�                        '    ����                                                                                                                                                                                                                                                                                                                                                             U�S`    �         @      (    resources :comments, only: [:create]5�_�                       .    ����                                                                                                                                                                                                                                                                                                                                                             U�Tx    �         @      /    resources :comments, only: [:create, :edit]5�_�                    !        ����                                                                                                                                                                                                                                                                                                                                                             U�[�     �   !   #   @    �   !   "   @    5�_�                    !       ����                                                                                                                                                                                                                                                                                                                                                             U�[�    �       !          (    resources :comments, only: [:create]5�_�                            ����                                                                                                                                                                                                                                                                                                                                                  V        U�\�     �                 5�_�                           ����                                                                                                                                                                                                                                                                                                                                                  V        U�\�     �                5  post "/rating", to: "ratings#create", as: "ratings"5�_�                            ����                                                                                                                                                                                                                                                                                                                                                  V        U�\�     �         >    �         >    5�_�      	                     ����                                                                                                                                                                                                                                                                                                                                                  V        U�\�     �         ?      5  post "/rating", to: "ratings#create", as: "ratings"5�_�      
           	          ����                                                                                                                                                                                                                                                                                                                                                  V        U�\�     �         @          �         ?    5�_�   	              
          ����                                                                                                                                                                                                                                                                                                                                                  V        U�\�    �                7    post "/rating", to: "ratings#create", as: "ratings"5�_�   
                         ����                                                                                                                                                                                                                                                                                                                                                  V        U�],     �                9    # post "/rating", to: "ratings#create", as: "ratings"5�_�                            ����                                                                                                                                                                                                                                                                                                                                                  V        U�]/    �       "   ?    �       !   ?    5�_�                          ����                                                                                                                                                                                                                                                                                                                                                             U�_      �         C    �         A        �         @    5�_�                            ����                                                                                                                                                                                                                                                                                                                                                             U�_
     �                 5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             U�_     �         C        concern :comments_concern do5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             U�_     �                8    resources :comments, only: [:create, :edit, :update]5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             U�_     �         B    �         B    5�_�                            ����                                                                                                                                                                                                                                                                                                                                                V       U�_     �         C    �         C    5�_�                           ����                                                                                                                                                                                                                                                                                                                                                V       U�_     �         F    5�_�                           ����                                                                                                                                                                                                                                                                                                                                                V       U�_     �         G        concern :comments do5�_�                           ����                                                                                                                                                                                                                                                                                                                                                V       U�_     �                8    resources :comments, only: [:create, :edit, :update]5�_�                    !       ����                                                                                                                                                                                                                                                                                                                                                V       U�_     �       !          '    resources :ratings, only: [:create]5�_�                           ����                                                                                                                                                                                                                                                                                                                                                V       U�_     �         E    �         E    5�_�                    "       ����                                                                                                                                                                                                                                                                                                                                                V       U�_     �   !   "            end5�_�                    !   U    ����                                                                                                                                                                                                                                                                                                                                                V       U�_     �       "   E      U  resources :moves, only: [:index, :show, :new, :create, :destroy, :edit, :update] do5�_�                    %        ����                                                                                                                                                                                                                                                                                                                            %   7       &   '       V   Q    U�_#     �   $   %          8    resources :comments, only: [:create, :edit, :update]   '    resources :ratings, only: [:create]5�_�                    $       ����                                                                                                                                                                                                                                                                                                                            %   7       %   '       V   Q    U�_$    �   #   %   C        resources :videos do5�_�                    !   R    ����                                                                                                                                                                                                                                                                                                                            %   7       %   '       V   Q    U�_-    �       "   C      R  resources :moves, only: [:index, :show, :new, :create, :destroy, :edit, :update]5�_�                    1       ����                                                                                                                                                                                                                                                                                                                            1   *       2   I       V   I    U�`Q     �   0   1          +  get "all_videos", to: "videos#all_videos"5�_�                     )       ����                                                                                                                                                                                                                                                                                                                            1   *       1   I       V   I    U�`S     �   )   +   B    �   )   *   B    5�_�      !               *       ����                                                                                                                                                                                                                                                                                                                            2   *       2   I       V   I    U�`T     �   )   +   C      +  get "all_videos", to: "videos#all_videos"5�_�       "           !   )       ����                                                                                                                                                                                                                                                                                                                            2   *       2   I       V   I    U�`V   	 �   )   +   C    5�_�   !   #           "   +       ����                                                                                                                                                                                                                                                                                                                            +   	       +          v       U�`\   
 �   *   ,   D      -    get "all_videos", to: "videos#all_videos"5�_�   "   $           #   +       ����                                                                                                                                                                                                                                                                                                                            +   	       +          v       U�`�    �   *   ,   D          get "all_videos"5�_�   #   %           $   *        ����                                                                                                                                                                                                                                                                                                                                                             U�a     �   +   -   F    �   *   -   E          �   *   ,   D    5�_�   $   &           %   .       ����                                                                                                                                                                                                                                                                                                                                                             U�a     �   -   .              get "all"5�_�   %   '           &   ,        ����                                                                                                                                                                                                                                                                                                                                                             U�a     �   ,   .   F    �   ,   -   F    5�_�   &   (           '   ,        ����                                                                                                                                                                                                                                                                                                                                                             U�a     �   +   ,           5�_�   '   )           (   ,       ����                                                                                                                                                                                                                                                                                                                                                             U�a    �   +   -   F          get "all"5�_�   (   *           )   !   s    ����                                                                                                                                                                                                                                                                                                                            +          -           V        U�b     �   !   #   G    �       #   F      s  resources :moves, only: [:index, :show, :new, :create, :destroy, :edit, :update], concerns: [:comments, :ratings]5�_�   )   +           *   "        ����                                                                                                                                                                                                                                                                                                                            -          /           V        U�b     �   "   $   I    �   !   $   H       5�_�   *   ,           +   &       ����                                                                                                                                                                                                                                                                                                                            /          1           V        U�b     �   %   &          (  get "all_moves", to: "moves#all_moves"5�_�   +   -           ,   #        ����                                                                                                                                                                                                                                                                                                                            .          0           V        U�b     �   #   %   I    �   #   $   I    5�_�   ,   .           -   #        ����                                                                                                                                                                                                                                                                                                                            /          1           V        U�b     �   "   #           5�_�   -   /           .   #       ����                                                                                                                                                                                                                                                                                                                            .          0           V        U�b     �   "   $   I      (  get "all_moves", to: "moves#all_moves"5�_�   .   0           /   #       ����                                                                                                                                                                                                                                                                                                                            .          0           V        U�b     �   "   $   I      *    get "all_moves", to: "moves#all_moves"5�_�   /   1           0   #       ����                                                                                                                                                                                                                                                                                                                            #          #          v       U�b     �   "   $   I      ,      get "all_moves", to: "moves#all_moves"5�_�   0   2           1   #       ����                                                                                                                                                                                                                                                                                                                            #          #          v       U�b    �   "   $   I      &      get "all", to: "moves#all_moves"5�_�   1   3           2           ����                                                                                                                                                                                                                                                                                                                                                             U�{W    �         K      &    resources :relationships, only: []�         J          �         I    5�_�   2   4           3           ����                                                                                                                                                                                                                                                                                                                                                             U�}�     �                    -    resources :relationships, only: [:create]5�_�   3   5           4          ����                                                                                                                                                                                                                                                                                                                                                             U�}�    �         J            �         I    5�_�   4   6           5          ����                                                                                                                                                                                                                                                                                                                                                v       U�~     �         J            post :follow5�_�   5   7           6          ����                                                                                                                                                                                                                                                                                                                                                v       U�~#    �         J      1      post :follow, to: "RelationshipsController"5�_�   6   8           7      ,    ����                                                                                                                                                                                                                                                                                                                                                             U��    �         K            �         J    5�_�   7   9           8           ����                                                                                                                                                                                                                                                                                                                                         1       v   1    U��c     �         K    �         K    5�_�   8   :           9          ����                                                                                                                                                                                                                                                                                                                                         1       v   1    U��d    �         L      3      get :following, to: "relationships#following"5�_�   9   ;           :          ����                                                                                                                                                                                                                                                                                                                                         1       v   1    U��j     �                6      delete :following, to: "relationships#following"5�_�   :   <           ;          ����                                                                                                                                                                                                                                                                                                                                         1       v   1    U��k     �         K    �         K    5�_�   ;   >           <          ����                                                                                                                                                                                                                                                                                                                                         1       v   1    U��k    �         L      .      post :follow, to: "relationships#create"5�_�   <   ?   =       >      )    ����                                                                                                                                                                                                                                                                                                                                         1       v   1    U��u    �         L      0      delete :follow, to: "relationships#create"5�_�   >   @           ?           ����                                                                                                                                                                                                                                                                                                                                         1       v   1    U���     �         L    �         L    5�_�   ?   A           @          ����                                                                                                                                                                                                                                                                                                                                         1       v   1    U���     �         M      3      get :following, to: "relationships#following"5�_�   @   B           A      )    ����                                                                                                                                                                                                                                                                                                                                         1       v   1    U���    �         M      3      get :followers, to: "relationships#following"5�_�   A   C           B   C        ����                                                                                                                                                                                                                                                                                                                                                             U���     �   C   E   M    5�_�   B   D           C   C       ����                                                                                                                                                                                                                                                                                                                                                             U���     �   C   E   N    �   C   D   N    5�_�   C               D   D   &    ����                                                                                                                                                                                                                                                                                                                            D   &       D   *       v   *    U���    �   C   E   O      ,    get "search/moves", to: "searches#moves"5�_�   <           >   =      (    ����                                                                                                                                                                                                                                                                                                                                         1       v   1    U��t     �         L      /      delete :follow, to: "relationshipscreate"5�_�                    !       ����                                                                                                                                                                                                                                                                                                                                                  V        U�]/     �   !   "   @       5��