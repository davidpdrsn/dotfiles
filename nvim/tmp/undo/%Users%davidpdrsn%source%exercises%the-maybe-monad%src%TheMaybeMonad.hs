Vim�UnDo� E&-�0l7��`�J��~4	7�չko�QR0;�J   a           +      P       P   P   P    UD�(    _�                            ����                                                                                                                                                                                                                                                                                                                                                  V        UD�     �         j    �         j    5�_�                            ����                                                                                                                                                                                                                                                                                                                                                  V        UD�     �                OdepositInterest a = transact (Deposit $ calculateInterest $ accountBalance a) a5�_�                           ����                                                                                                                                                                                                                                                                                                                                                V       UD��     �         k      OdepositInterest a = transact (Deposit $ calculateInterest $ accountBalance a) a5�_�                       )    ����                                                                                                                                                                                                                                                                                                                                                V       UD��     �         k      RdepositInterest a = transact =<< Deposit $ calculateInterest $ accountBalance a) a5�_�                       =    ����                                                                                                                                                                                                                                                                                                                                                V       UD��     �         k      RdepositInterest a = transact =<< Deposit . calculateInterest $ accountBalance a) a5�_�                       M    ����                                                                                                                                                                                                                                                                                                                                                V       UD��    �         k      RdepositInterest a = transact =<< Deposit . calculateInterest . accountBalance a) a5�_�                            ����                                                                                                                                                                                                                                                                                                                                                V       UD�     �                MdepositInterest a = transact =<< Deposit . calculateInterest . accountBalance5�_�      	                      ����                                                                                                                                                                                                                                                                                                                                                V       UD�     �                R-- depositInterest a = transact (Deposit $ calculateInterest $ accountBalance a) a5�_�      
           	           ����                                                                                                                                                                                                                                                                                                                                                  V        UD�     �                OdepositInterest a = transact (Deposit $ calculateInterest $ accountBalance a) a5�_�   	              
           ����                                                                                                                                                                                                                                                                                                                                                  V        UD�     �                P-- depositInterest a = transact =<< Deposit . calculateInterest . accountBalance5�_�   
                        ����                                                                                                                                                                                                                                                                                                                                                  V        UD�     �         k      MdepositInterest a = transact =<< Deposit . calculateInterest . accountBalance5�_�                           ����                                                                                                                                                                                                                                                                                                                                                  V        UD�    �         k      LdepositInterest  = transact =<< Deposit . calculateInterest . accountBalance5�_�                           ����                                                                                                                                                                                                                                                                                                                                                  V        UD�    �                R-- depositInterest a = transact (Deposit $ calculateInterest $ accountBalance a) a5�_�                            ����                                                                                                                                                                                                                                                                                                                                                  V        UD�"     �      !   j      'withdrawFee m = transact $ Withdrawal m5�_�                            ����                                                                                                                                                                                                                                                                                                                                                  V        UD�"     �      !   j      &withdrawFee  = transact $ Withdrawal m5�_�                            ����                                                                                                                                                                                                                                                                                                                                                  V        UD�$     �      !   j      %withdrawFee = transact $ Withdrawal m5�_�                        %    ����                                                                                                                                                                                                                                                                                                                                                  V        UD�$    �      !   j      %withdrawFee = transact . Withdrawal m5�_�                   G        ����                                                                                                                                                                                                                                                                                                                            G           J           V        UD�B     �   I   K                  Nothing -> Nothing�   H   J          %        Just a' -> depositInterest a'�   G   I          $    case withdrawFee monthlyFee a of�   F   H          monthlyBalance a =5�_�                   J        ����                                                                                                                                                                                                                                                                                                                            G           J           V        UD�F     �   J   O   j    �   J   K   j    5�_�                    K        ����                                                                                                                                                                                                                                                                                                                            K           N           V        UD�G   	 �   M   O          --         Nothing -> Nothing�   L   N          (--         Just a' -> depositInterest a'�   K   M          '--     case withdrawFee monthlyFee a of�   J   L          -- monthlyBalance a =5�_�                    K       ����                                                                                                                                                                                                                                                                                                                            K           N           V        UD�J     �   J   L   n      monthlyBalance a =5�_�                    K       ����                                                                                                                                                                                                                                                                                                                            K           N           V        UD�J     �   J   L   n      monthlyBalancea =5�_�                     L       ����                                                                                                                                                                                                                                                                                                                            K           N           V        UD�L     �   K   M   n      $    case withdrawFee monthlyFee a of5�_�      !               N       ����                                                                                                                                                                                                                                                                                                                            K           N           V        UD�M     �   M   N                  Nothing -> Nothing5�_�       "           !   M       ����                                                                                                                                                                                                                                                                                                                            M          M          v       UD�O     �   L   N   m      %        Just a' -> depositInterest a'5�_�   !   #           "   L       ����                                                                                                                                                                                                                                                                                                                            M          M          v       UD�Q     �   K   M   m          withdrawFee monthlyFee a of5�_�   "   $           #   L       ����                                                                                                                                                                                                                                                                                                                            M          M          v       UD�S     �   K   M   m           withdrawFee monthlyFee a >>=           depositInterest a'5�_�   #   %           $   L   3    ����                                                                                                                                                                                                                                                                                                                            L   !       L   +       v       UD�T   
 �   K   M   l      3    withdrawFee monthlyFee a >>= depositInterest a'5�_�   $   &           %   K       ����                                                                                                                                                                                                                                                                                                                            L   !       L   +       v       UD�U     �   J   L   l      monthlyBalance =   0    withdrawFee monthlyFee a >>= depositInterest5�_�   %   '           &   K   '    ����                                                                                                                                                                                                                                                                                                                            K   .       K   8       v       UD�W     �   J   L   k      =monthlyBalance = withdrawFee monthlyFee a >>= depositInterest5�_�   &   (           '   K   '    ����                                                                                                                                                                                                                                                                                                                            K   .       K   8       v       UD�X    �   J   L   k      <monthlyBalance = withdrawFee monthlyFeea >>= depositInterest5�_�   '   )           (   K       ����                                                                                                                                                                                                                                                                                                                            K   .       K   8       v       UD�^    �   J   L   k      ;monthlyBalance = withdrawFee monthlyFee >>= depositInterest5�_�   (   *           )   K   )    ����                                                                                                                                                                                                                                                                                                                            K   .       K   8       v       UD�d     �   J   L   k      =monthlyBalance = withdrawFee . monthlyFee >>= depositInterest5�_�   )   +           *   K       ����                                                                                                                                                                                                                                                                                                                            K   .       K   8       v       UD�f    �   J   L   k      )monthlyBalance = withdrawFee . monthlyFee5�_�   *   ,           +   K        ����                                                                                                                                                                                                                                                                                                                            K   .       K   8       v       UD�l     �   J   K          =monthlyBalance = depositInterest =<< withdrawFee . monthlyFee5�_�   +   -           ,   G        ����                                                                                                                                                                                                                                                                                                                            J           G           V        UD�m    �   I   K          --         Nothing -> Nothing�   H   J          (--         Just a' -> depositInterest a'�   G   I          '--     case withdrawFee monthlyFee a of�   F   H          -- monthlyBalance a =5�_�   ,   .           -   H       ����                                                                                                                                                                                                                                                                                                                            G           J           V        UD�     �   G   I   j      $    case withdrawFee monthlyFee a of5�_�   -   /           .   H       ����                                                                                                                                                                                                                                                                                                                            G           J           V        UD�     �   G   I   j          withdrawFee monthlyFee a of5�_�   .   0           /   H       ����                                                                                                                                                                                                                                                                                                                            G           J           V        UD�     �   G   I   j           withdrawFee monthlyFee a >>=   %        Just a' -> depositInterest a'5�_�   /   1           0   H   !    ����                                                                                                                                                                                                                                                                                                                            H   +       H   !       v   !    UD�     �   G   I   i      >    withdrawFee monthlyFee a >>= Just a' -> depositInterest a'5�_�   0   2           1   I       ����                                                                                                                                                                                                                                                                                                                            H   +       H   !       v   !    UD�     �   H   I                  Nothing -> Nothing5�_�   1   3           2   H   3    ����                                                                                                                                                                                                                                                                                                                            H   +       H   !       v   !    UD�    �   G   I   h      3    withdrawFee monthlyFee a >>= depositInterest a'5�_�   2   4           3   G       ����                                                                                                                                                                                                                                                                                                                            H   +       H   !       v   !    UD�    �   F   H   h      monthlyBalance a =   0    withdrawFee monthlyFee a >>= depositInterest5�_�   3   5           4   \       ����                                                                                                                                                                                                                                                                                                                            \          \   &       v   &    UD��     �   Z   \   g          case transact t a of   withdrawFee atmFee a'�   [   ]   g      (        Just a' -> withdrawFee atmFee a'5�_�   4   6           5   [   /    ����                                                                                                                                                                                                                                                                                                                            [   +       [   >       v   &    UD��     �   Z   \   f      /    case transact t a >>= withdrawFee atmFee a'5�_�   5   7           6   [       ����                                                                                                                                                                                                                                                                                                                            [   +       [   >       v   &    UD��     �   Z   \   f      ,    case transact t a >>= withdrawFee atmFee5�_�   6   8           7   \       ����                                                                                                                                                                                                                                                                                                                            [   +       [   >       v   &    UD��     �   [   \                  Nothing -> Nothing5�_�   7   J           8   Z        ����                                                                                                                                                                                                                                                                                                                            [   +       [   >       v   &    UD��    �   Y   [   e      atmTransact t a =   '    transact t a >>= withdrawFee atmFee5�_�   8   K   D       J   b       ����                                                                                                                                                                                                                                                                                                                            \           d           V        UD�     �   a   c   d          case transact t a of5�_�   J   L           K   b       ����                                                                                                                                                                                                                                                                                                                            \           d           V        UD�     �   a   c   d          transact t a of5�_�   K   M           L   c       ����                                                                                                                                                                                                                                                                                                                            \           d           V        UD�"     �   a   c   d          transact t a >>=   transactAll ts applied�   b   d   d      .        Just applied -> transactAll ts applied5�_�   L   N           M   b   +    ����                                                                                                                                                                                                                                                                                                                            \           c           V        UD�%     �   a   c   c      +    transact t a >>= transactAll ts applied5�_�   M   P           N   c       ����                                                                                                                                                                                                                                                                                                                            \           c           V        UD�&     �   b   c                  Nothing -> Nothing5�_�   N       O       P   a       ����                                                                                                                                                                                                                                                                                                                            \           c           V        UD�'    �   `              transactAll (t:ts) a =   #    transact t a >>= transactAll ts5�_�   N           P   O   `       ����                                                                                                                                                                                                                                                                                                                            \           b           V        UD�&     �   _   b   b      0transactAll [] a = Just a transactAll (t:ts) a =5�_�   8   E   9   J   D   Z   #    ����                                                                                                                                                                                                                                                                                                                            Y          Z          V       UD�     �   Y   [   d      0atmTransact t a = transact t awithdrawFee atmFee5�_�   D   H           E   Z       ����                                                                                                                                                                                                                                                                                                                            Y          Z          V       UD�     �   Y   [   d      atmTransact t a = transact t a5�_�   E   I   G       H   Z       ����                                                                                                                                                                                                                                                                                                                            Y          Z          V       UD�     �   Z   [   d    �   Y   [   d      0atmTransact t a = withdrawFee atmFeetransact t a5�_�   H               I   Z   $    ����                                                                                                                                                                                                                                                                                                                            Y          Z          V       UD�    �   Y   [   d      5atmTransact t a = withdrawFee atmFee =<< transact t a5�_�   E       F   H   G   Z       ����                                                                                                                                                                                                                                                                                                                            Y          Z          V       UD�     �   Z   [   d    �   Y   [   d      0atmTransact t a = twithdrawFee atmFeeransact t a5�_�   E           G   F   Z       ����                                                                                                                                                                                                                                                                                                                            Y          Z          V       UD�     �   Z   [   d    �   Y   [   d      0atmTransact t a = withdrawFee atmFeetransact t a5�_�   8   :       D   9   Z       ����                                                                                                                                                                                                                                                                                                                            Y          Z          V       UD��     �   Y   [   d      4atmTransact  a = transact t a >>= withdrawFee atmFee5�_�   9   ;           :   Z       ����                                                                                                                                                                                                                                                                                                                            Y          Z          V       UD��     �   Y   [   d      3atmTransact a = transact t a >>= withdrawFee atmFee5�_�   :   <           ;   Z       ����                                                                                                                                                                                                                                                                                                                            Y          Z          V       UD��     �   Y   [   d      2atmTransact  = transact t a >>= withdrawFee atmFee5�_�   ;   =           <   Z       ����                                                                                                                                                                                                                                                                                                                            Z          Z          v       UD��     �   Y   [   d      1atmTransact = transact t a >>= withdrawFee atmFee5�_�   <   >           =   Z       ����                                                                                                                                                                                                                                                                                                                            Z          Z          v       UD��    �   Y   [   d      -atmTransact = transact >>= withdrawFee atmFee5�_�   =   ?           >   Z       ����                                                                                                                                                                                                                                                                                                                            Z          Z          v       UD��     �   Y   [   d      $atmTransact = >>= withdrawFee atmFee5�_�   >   @           ?   Z       ����                                                                                                                                                                                                                                                                                                                            Z          Z          v       UD��     �   Y   [   d      #atmTransact = >= withdrawFee atmFee5�_�   ?   A           @   Z       ����                                                                                                                                                                                                                                                                                                                            Z          Z          v       UD��     �   Y   [   d      "atmTransact = = withdrawFee atmFee5�_�   @   B           A   Z       ����                                                                                                                                                                                                                                                                                                                            Z          Z          v       UD��     �   Y   [   d      !atmTransact =  withdrawFee atmFee5�_�   A   C           B   Z       ����                                                                                                                                                                                                                                                                                                                            Z          Z          v       UD��     �   Y   [   d       atmTransact = withdrawFee atmFee5�_�   B               C   Z        ����                                                                                                                                                                                                                                                                                                                            Z          Z          v       UD��    �   Y   [   d      -atmTransact = withdrawFee atmFee =<< transact5�_�                    K        ����                                                                                                                                                                                                                                                                                                                            G           J           V        UD�E     �   K   L   j    �   K   L   j      -- monthlyBalance a =   '--     case withdrawFee monthlyFee a of   (--         Just a' -> depositInterest a'   --         Nothing -> Nothing5�_�                    H        ����                                                                                                                                                                                                                                                                                                                            G           G           V        UD�>     �   G   I          '    -- case withdrawFee monthlyFee a of�   H   J          (    --     Just a' -> depositInterest a'�   I   K              --     Nothing -> Nothing5�_�                          ����                                                                                                                                                                                                                                                                                                                                                  V        UD�     �         k      BdepositInterest = =<< Deposit . calculateInterest . accountBalance5�_�                           ����                                                                                                                                                                                                                                                                                                                                                  V        UD�     �         k      AdepositInterest = << Deposit . calculateInterest . accountBalance5�_�                           ����                                                                                                                                                                                                                                                                                                                                                  V        UD�     �         k      @depositInterest = < Deposit . calculateInterest . accountBalance5�_�                           ����                                                                                                                                                                                                                                                                                                                                                  V        UD�     �         k      ?depositInterest =  Deposit . calculateInterest . accountBalance5�_�                           ����                                                                                                                                                                                                                                                                                                                                                  V        UD�     �         k      >depositInterest = Deposit . calculateInterest . accountBalance5�_�                        >    ����                                                                                                                                                                                                                                                                                                                                                  V        UD�    �         k      KdepositInterest = Deposit . calculateInterest . accountBalance >>= transact5��