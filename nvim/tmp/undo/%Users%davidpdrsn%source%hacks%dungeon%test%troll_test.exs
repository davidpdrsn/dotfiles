Vim�UnDo� 4��hnxXɧ"���՘D���~�ò���xM	      	  DocTest            Q       Q   Q   Q    T��5   , _�                             ����                                                                                                                                                                                                                                                                                                                                                             T��n     �                   �               5�_�                            ����                                                                                                                                                                                                                                                                                                                                                             T��o     �                  5�_�                            ����                                                                                                                                                                                                                                                                                                                                       )           V        T��q     �             &     test "getting its size" do   "    dungeon = Dungeon.new {20, 20}       ,    assert Dungeon.size(dungeon) == {20, 20}     end       *  test "the dungeon is empty has first" do   "    dungeon = Dungeon.new {20, 20}       4    assert Dungeon.players(dungeon)[{1,1}] == :empty     end       (  test "adding trolls to the dungeon" do   "    dungeon = Dungeon.new {20, 20}       troll = Troll.new   6    {:ok, dungeon} = Dungeon.add dungeon, troll, {1,1}   6    {:ok, dungeon} = Dungeon.add dungeon, troll, {1,2}       3    assert Dungeon.players(dungeon)[{1,1}] == troll   3    assert Dungeon.players(dungeon)[{1,2}] == troll     end       K  test "adding something at a position where there already is something" do   "    dungeon = Dungeon.new {20, 20}       troll = Troll.new   6    {:ok, dungeon} = Dungeon.add dungeon, troll, {1,1}   3    {status, _} = Dungeon.add dungeon, troll, {1,1}           assert status == :error     end         test ".within_bounds?" do   "    dungeon = Dungeon.new {20, 20}       9    assert false == Dungeon.within_bounds? {0,0}, dungeon   ;    assert false == Dungeon.within_bounds? {21,21}, dungeon   8    assert true == Dungeon.within_bounds? {1,1}, dungeon     end5�_�                       
    ����                                                                                                                                                                                                                                                                                                                                                  V        T��r    �                defmodule DungeonTest do5�_�                            ����                                                                                                                                                                                                                                                                                                                                                  V        T��v    �                 5�_�                            ����                                                                                                                                                                                                                                                                                                                                                  V        T���     �                 �             5�_�                           ����                                                                                                                                                                                                                                                                                                                                                  V        T���    �             �               	  test ""5�_�      	                      ����                                                                                                                                                                                                                                                                                                                                                  V        T���     �             �             5�_�      
           	           ����                                                                                                                                                                                                                                                                                                                            	           	           V        T���    �                 5�_�   	              
          ����                                                                                                                                                                                                                                                                                                                                                V       T���    �      	       �             5�_�   
                        ����                                                                                                                                                                                                                                                                                                                                                V       T���     �                6    {:ok, dungeon} = Dungeon.add dungeon, troll, {1,2}5�_�                           ����                                                                                                                                                                                                                                                                                                                                                V       T��    �   	                  �   	          �         
          �      	   	    5�_�                    
       ����                                                                                                                                                                                                                                                                                                                                                V       T��;     �   	                  assert true == true5�_�                    
   0    ����                                                                                                                                                                                                                                                                                                                                                V       T��K     �   	            1      new_position = Troll.new_position troll, {}5�_�                           ����                                                                                                                                                                                                                                                                                                                                                V       T��P     �                   �             5�_�                           ����                                                                                                                                                                                                                                                                                                                                                V       T��R     �                   current_pos = {}5�_�                       6    ����                                                                                                                                                                                                                                                                                                                                                V       T��S     �      	         6    {:ok, dungeon} = Dungeon.add dungeon, troll, {1,1}5�_�                       :    ����                                                                                                                                                                                                                                                                                                                                                V       T��W     �   
            :      new_position = Troll.new_position troll, current_pos5�_�                       <    ����                                                                                                                                                                                                                                                                                                                                                V       T��\     �   
            C      new_position = Troll.new_position troll, current_pos, dungeon5�_�                       B    ����                                                                                                                                                                                                                                                                                                                                                V       T��_     �                     �             5�_�                       0    ����                                                                                                                                                                                                                                                                                                                                                V       T��q     �             �             5�_�                           ����                                                                                                                                                                                                                                                                                                                                                V       T��u   	 �               1      assert Dungeon.empty? new_position, dungeon5�_�                       "    ����                                                                                                                                                                                                                                                                                                                               "          "       V   "    T��{    �             5�_�                    
        ����                                                                                                                                                                                                                                                                                                                               "          "       V   "    T��*     �   	                 Enum.map 1..20, fn _ ->5�_�                            ����                                                                                                                                                                                                                                                                                                                               "          "       V   "    T��+    �                    end5�_�                            ����                                                                                                                                                                                                                                                                                                                               "          "       V   "    T���    �                     �             5�_�                           ����                                                                                                                                                                                                                                                                                                                               "          "       V   "    T���     �                   current_pos = {1,1}5�_�                           ����                                                                                                                                                                                                                                                                                                                               "          "       V   "    T���    �                   current_pos = {10,10}5�_�                   
       ����                                                                                                                                                                                                                                                                                                                               "          "       V   "    T��D     �   	                 # Enum.map 1..20, fn _ ->5�_�                            ����                                                                                                                                                                                                                                                                                                                               "          "       V   "    T��E    �                	    # end5�_�      !               	        ����                                                                                                                                                                                                                                                                                                                               "          "       V   "    T��O     �      	           5�_�       "           !          ����                                                                                                                                                                                                                                                                                                                                                V       T��P     �                "    dungeon = Dungeon.new {20, 20}       troll = Troll.new       current_pos = {10, 10}   <    {:ok, dungeon} = Dungeon.add dungeon, troll, current_pos5�_�   !   #           "          ����                                                                                                                                                                                                                                                                                                                                                V       T��R     �      
       �             5�_�   "   $           #   	       ����                                                                                                                                                                                                                                                                                                                                                V       T��S     �   	          5�_�   #   &           $          ����                                                                                                                                                                                                                                                                                                                            	                    V       T��U    �      
         "    dungeon = Dungeon.new {20, 20}       troll = Troll.new       current_pos = {10, 10}   <    {:ok, dungeon} = Dungeon.add dungeon, troll, current_pos5�_�   $   '   %       &          ����                                                                                                                                                                                                                                                                                                                            	                    V       T��a    �                   Enum.map 1..20, fn _ ->5�_�   &   (           '      '    ����                                                                                                                                                                                                                                                                                                                                                  V        T��     �   
            C      new_position = Troll.new_position troll, current_pos, dungeon5�_�   '   )           (      '    ����                                                                                                                                                                                                                                                                                                                                                  V        T��    �   
            =      new_position = Troll.new_position, current_pos, dungeon5�_�   (   *           )           ����                                                                                                                                                                                                                                                                                                                               '          '       V   '    T�E@     �                 �             5�_�   )   +           *          ����                                                                                                                                                                                                                                                                                                                               '          '       V   '    T�EB     �             �               	  test ""5�_�   *   ,           +           ����                                                                                                                                                                                                                                                                                                                                                V       T�EN     �             �             5�_�   +   -           ,           ����                                                                                                                                                                                                                                                                                                                                                V       T�EN     �                 5�_�   ,   .           -          ����                                                                                                                                                                                                                                                                                                                                                V       T�EP     �                9      assert Dungeon.within_bounds? new_position, dungeon�                1      assert Dungeon.empty? new_position, dungeon�                (      assert new_position != current_pos�                <      new_position = Troll.new_position current_pos, dungeon�                >      {:ok, dungeon} = Dungeon.add dungeon, troll, current_pos�                      current_pos = {10, 10}�                      troll = Troll.new�                $      dungeon = Dungeon.new {20, 20}5�_�   -   /           .          ����                                                                                                                                                                                                                                                                                                                                                V       T�EU     �               "    dungeon = Dungeon.new {20, 20}5�_�   .   0           /          ����                                                                                                                                                                                                                                                                                                                                                V       T�Ea     �                /    assert Dungeon.empty? new_position, dungeon   7    assert Dungeon.within_bounds? new_position, dungeon5�_�   /   3           0          ����                                                                                                                                                                                                                                                                                                                                                V       T�Ec    �               &    assert new_position != current_pos5�_�   0   4   2       3          ����                                                                                                                                                                                                                                                                                                                                                V       T�E�    �                   current_pos = {10, 10}5�_�   3   5           4           ����                                                                                                                                                                                                                                                                                                                                                V       T�FD     �                 �             5�_�   4   6           5          ����                                                                                                                                                                                                                                                                                                                                                V       T�FF     �               �                	  test ""5�_�   5   7           6          ����                                                                                                                                                                                                                                                                                                                                                V       T�FI     �         !        test "to string" do5�_�   6   8           7           ����                                                                                                                                                                                                                                                                                                                                                V       T�FL     �          !       5�_�   7   9           8           ����                                                                                                                                                                                                                                                                                                                                                V       T�FT     �      !   !    �          !    5�_�   8   :           9           ����                                                                                                                                                                                                                                                                                                                                                V       T�FU     �                 5�_�   9   ;           :          ����                                                                                                                                                                                                                                                                                                                                                V       T�FU     �      "   "          �      !   !    5�_�   :   <           ;          ����                                                                                                                                                                                                                                                                                                                                                V       T�F]     �         #        test ".to_string" do5�_�   ;   =           <   !       ����                                                                                                                                                                                                                                                                                                                                                V       T�F_     �       "   #          assert Troll.to_5�_�   <   >           =   !       ����                                                                                                                                                                                                                                                                                                                                                V       T�F`     �       "   #          assert Troll.to_s()5�_�   =   ?           >   !        ����                                                                                                                                                                                                                                                                                                                                                V       T�Ff     �       "   #      !    assert Troll.to_s troll == ""5�_�   >   @           ?   !       ����                                                                                                                                                                                                                                                                                                                                                V       T�F�     �       "   #      "    assert Troll.to_s troll == "T"5�_�   ?   A           @   !       ����                                                                                                                                                                                                                                                                                                                                                V       T�F�   " �       "   #          assert Troll.to_s() == "T"5�_�   @   B           A           ����                                                                                                                                                                                                                                                                                                                           "                      V        T�G>   # �                      test ".to_s" do       troll = Troll.new       #    assert Troll.to_s(troll) == "T"     end5�_�   A   D           B          ����                                                                                                                                                                                                                                                                                                                                                            T��$   $ �                 use ExUnit.Case5�_�   B   E   C       D           ����                                                                                                                                                                                                                                                                                                                                                 V        T��X     �      '       �             5�_�   D   F           E          ����                                                                                                                                                                                                                                                                                                                                                 V        T��Y     �         '    5�_�   E   G           F           ����                                                                                                                                                                                                                                                                                                                                                 V        T��Y     �          *    �          )        �         (    5�_�   F   H           G          ����                                                                                                                                                                                                                                                                                                                                                 V        T��f     �         +        Enum.each 1..100, fn _ _>5�_�   G   I           H           ����                                                                                                                                                                                                                                                                                                                                                 V        T��h     �                 5�_�   H   J           I          ����                                                                                                                                                                                                                                                                                                                                                 V        T��h     �                  end5�_�   I   M           J   )        ����                                                                                                                                                                                                                                                                                                                                                 V        T��i     �   )            �   )            5�_�   J   N   K       M           ����                                                                                                                                                                                                                                                                                                                                      (           V        T���     �                      Enum.each 1..100, fn _ ->   .  test "it stays if it can't move anywhere" do       dungeon = Dungeon.new {1,1}       troll = Troll.new       current_pos = {1,1}   <    {:ok, dungeon} = Dungeon.add dungeon, troll, current_pos       :    new_position = Troll.new_position current_pos, dungeon       &    assert new_position == current_pos     end5�_�   M   O           N           ����                                                                                                                                                                                                                                                                                                                                                 V        T���   ) �                  end5�_�   N   P           O           ����                                                                                                                                                                                                                                                                                                                                                 V        T��     �                 �             5�_�   O   Q           P          ����                                                                                                                                                                                                                                                                                                                                                 V        T��2     �               	  DocTest5�_�   P               Q          ����                                                                                                                                                                                                                                                                                                                                                 V        T��4   , �                 doctest MyModule Troll5�_�   J   L       M   K           ����                                                                                                                                                                                                                                                                                                                                     *           V        T��k   & �                 0    test "it stays if it can't move anywhere" do�      !          !      dungeon = Dungeon.new {1,1}�       "                troll = Troll.new�   !   #                current_pos = {1,1}�   "   $          >      {:ok, dungeon} = Dungeon.add dungeon, troll, current_pos�   $   &          <      new_position = Troll.new_position current_pos, dungeon�   &   (          (      assert new_position == current_pos�   '   )              end�   (   *            end�   )   +          end5�_�   K               L          ����                                                                                                                                                                                                                                                                                                                                     *           V        T��w   ' �         *        use ExUnit.Case, async: false5�_�   B           D   C          ����                                                                                                                                                                                                                                                                                                                                                            T��(     �                5�_�   0       1   3   2          ����                                                                                                                                                                                                                                                                                                                                                V       T�E�     �                   current_pos = {1i[], 10}5�_�   0           2   1           ����                                                                                                                                                                                                                                                                                                                                                V       T�E�     �               10, 10}5�_�   $           &   %          ����                                                                                                                                                                                                                                                                                                                            	                    V       T��]    �                   Enum.map 1..200, fn _ ->5�_�                           ����                                                                                                                                                                                                                                                                                                                               "          "       V   "    T���    �                   current_pos = {1,1}5��