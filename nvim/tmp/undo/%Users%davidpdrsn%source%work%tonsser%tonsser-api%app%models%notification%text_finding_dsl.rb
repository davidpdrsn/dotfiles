Vim�UnDo� �t}'��ްۍ5L��*خ�������Hj�                                     T�<R    _�                            ����                                                                                                                                                                                                                                                                                                                                                v       T�3e     �               (    def initialize(context, obj, &block)5�_�                           ����                                                                                                                                                                                                                                                                                                                                                v       T�3f     �                      @obj = obj5�_�                    
   4    ����                                                                                                                                                                                                                                                                                                                                                v       T�3j     �   	            ;      finder = Notification::TextFinder.new(@context, @obj)5�_�                       !    ����                                                                                                                                                                                                                                                                                                                                                v       T�3l    �   
            )      finder.instance_exec(@obj, &@block)5�_�                           ����                                                                                                                                                                                                                                                                                                                                                v       T�3q    �   
            #      finder.instance_exec(&@block)5�_�                            ����                                                                                                                                                                                                                                                                                                                                                             T�<Q    �                4# TODO: Add comments explaining what this class does5��