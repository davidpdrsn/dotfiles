Vim�UnDo� ���;���V;Ov�Ȳ�k<��T�u��],B�      (// bob@example.com,Hi there,Its all good            ;       ;   ;   ;    T�_\    _�                             ����                                                                                                                                                                                                                                                                                                                                                             TR	�     �                   5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             TR	�     �                  // 5�_�                       '    ����                                                                                                                                                                                                                                                                                                                                                             TR	�    �                  '// SRP: Single responsibility principle5�_�                       .    ����                                                                                                                                                                                                                                                                                                                                                             TR	�     �                 // �               5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             TR
     �                 class MailSender {}5�_�                            ����                                                                                                                                                                                                                                                                                                                                                             TR
     �                 �             5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             TR
     �                 MailSender()5�_�      	                     ����                                                                                                                                                                                                                                                                                                                                                             TR
     �                 MailSender() {}5�_�      
           	          ����                                                                                                                                                                                                                                                                                                                                                             TR
     �                 MailSender() {5�_�   	              
          ����                                                                                                                                                                                                                                                                                                                                                             TR
     �                   �             5�_�   
                        ����                                                                                                                                                                                                                                                                                                                                                v       TR
#     �                   this.cvs = csv;�                 MailSender(String cvs) {5�_�                           ����                                                                                                                                                                                                                                                                                                                               	                 v       TR
%     �             5�_�                            ����                                                                                                                                                                                                                                                                                                                               	                 v       TR
&     �                 5�_�                           ����                                                                                                                                                                                                                                                                                                                               	                 v       TR
&     �             5�_�                           ����                                                                                                                                                                                                                                                                                                                               	                 v       TR
'     �         	        �             5�_�                           ����                                                                                                                                                                                                                                                                                                                               	                 v       TR
+     �         
        �      
   	    5�_�                    
       ����                                                                                                                                                                                                                                                                                                                               	                 v       TR
2     �   	              public function sendEmails()5�_�                    
       ����                                                                                                                                                                                                                                                                                                                               	                 v       TR
4     �   	              public function sendMails()5�_�                    
       ����                                                                                                                                                                                                                                                                                                                               	                 v       TR
5     �   	               public function sendMails() {}5�_�                    
       ����                                                                                                                                                                                                                                                                                                                               	                 v       TR
6     �   	              public function sendMails() {5�_�                    
       ����                                                                                                                                                                                                                                                                                                                               	                 v       TR
?     �   
                �   
          5�_�                       -    ����                                                                                                                                                                                                                                                                                                                               	                 v       TR
W     �   
            .    ArrayList<String> emails = this.parseCsv()5�_�                       .    ����                                                                                                                                                                                                                                                                                                                               	                 v       TR
W     �                   �             5�_�                       	    ����                                                                                                                                                                                                                                                                                                                               	                 v       TR
Z     �               
    for ()5�_�                            ����                                                                                                                                                                                                                                                                                                                               	                 v       TR
_     �                    for (String email in emails)5�_�                       "    ����                                                                                                                                                                                                                                                                                                                               	                 v       TR
_     �               #    for (String email in emails) {}5�_�                           ����                                                                                                                                                                                                                                                                                                                                                v       TR
c     �               "    for (String email in emails) {5�_�                           ����                                                                                                                                                                                                                                                                                                                                                v       TR
r     �             5�_�                           ����                                                                                                                                                                                                                                                                                                                                                v       TR
w     �                     �             5�_�                           ����                                                                                                                                                                                                                                                                                                                                                v       TR
}     �                 �             5�_�                     
       ����                                                                                                                                                                                                                                                                                                                                                v       TR
�     �   	              public function deliver() {5�_�      !                       ����                                                                                                                                                                                                                                                                                                                                                v       TR
�     �                5�_�       "           !   
   	    ����                                                                                                                                                                                                                                                                                                                                                v       TR
�     �   	            "  public function deliverMails() {5�_�   !   #           "          ����                                                                                                                                                                                                                                                                                                                                                v       TR
�     �                 private arra5�_�   "   $           #      %    ����                                                                                                                                                                                                                                                                                                                                                v       TR
�     �               &  private ArrayList<String> parseCsv()5�_�   #   %           $      (    ����                                                                                                                                                                                                                                                                                                                                                v       TR
�     �               )  private ArrayList<String> parseCsv() {}5�_�   $   &           %          ����                                                                                                                                                                                                                                                                                                                                                v       TR
�     �                   �             5�_�   %   '           &          ����                                                                                                                                                                                                                                                                                                                                                v       TR
�    �                   // Code for parsing the cs5�_�   &   (           '           ����                                                                                                                                                                                                                                                                                                                                                  V        TR�     �                  �               5�_�   '   )           (           ����                                                                                                                                                                                                                                                                                                                                                  V        TR�     �                 /**/5�_�   (   *           )          ����                                                                                                                                                                                                                                                                                                                                                  V        TR�     �                 new MailSender()5�_�   )   +           *          ����                                                                                                                                                                                                                                                                                                                                                  V        TR�     �                 new MailSender(/* csv /*)5�_�   *   ,           +          ����                                                                                                                                                                                                                                                                                                                                                  V        TR�     �                 new MailSender(/* csv /*);5�_�   +   -           ,          ����                                                                                                                                                                                                                                                                                                                                                  V        TR�     �                 new MailSender(/* csv /*);5�_�   ,   .           -          ����                                                                                                                                                                                                                                                                                                                                                  V        TR�     �                 new MailSender(/* csv *);�               5�_�   -   /           .          ����                                                                                                                                                                                                                                                                                                                                                  V        TR�     �                 new MailSender(/* csv */);5�_�   .   0           /      '    ����                                                                                                                                                                                                                                                                                                                                                  V        TR�    �                 (new MailSender(/* csv */).deliverMails()5�_�   /   1           0          ����                                                                                                                                                                                                                                                                                                                                                  V        TR     �                 )new MailSender(/* csv */).deliverMails();5�_�   0   2           1          ����                                                                                                                                                                                                                                                                                                                                                  V        TR     �                 #new MailSender(csv).deliverMails();5�_�   1   3           2          ����                                                                                                                                                                                                                                                                                                                                                  V        TR     �                  new MailSender().deliverMails();5�_�   2   4           3   
       ����                                                                                                                                                                                                                                                                                                                                                  V        TR     �   	              public void deliverMails() {5�_�   3   5           4          ����                                                                                                                                                                                                                                                                                                                                                  V        TR     �                    this.csv = csv;5�_�   4   6           5          ����                                                                                                                                                                                                                                                                                                                                                  V        TR    �                 MailSender(String csv) {5�_�   5   7           6           ����                                                                                                                                                                                                                                                                                                                                                 V       TR   	 �                  MailSender() {     }    5�_�   6   8           7      "    ����                                                                                                                                                                                                                                                                                                                                                             T�_>     �               // �             5�_�   7   9           8      '    ����                                                                                                                                                                                                                                                                                                                                                             T�_T     �             �             5�_�   8   :           9          ����                                                                                                                                                                                                                                                                                                                                                             T�_U     �               (// bob@example.com,Hi there,Its all good5�_�   9   ;           :          ����                                                                                                                                                                                                                                                                                                                                                             T�_W     �               *// alice@example.com,Hi there,Its all good5�_�   :               ;          ����                                                                                                                                                                                                                                                                                                                                                             T�_[    �               )// alice@example.com,Goodbye,Its all good5��