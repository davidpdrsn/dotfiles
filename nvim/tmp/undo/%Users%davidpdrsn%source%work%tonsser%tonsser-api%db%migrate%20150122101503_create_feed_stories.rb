Vim�UnDo� gt�����gg��]�x��g7I�{C�7���k            t.string :slug                             T��    _�                             ����                                                                                                                                                                                                                                                                                                                                                             T�ͬ     �                     �             5�_�                       '    ����                                                                                                                                                                                                                                                                                                                                                             T���     �             �             5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             T���     �               (      t.integer :subject_id, null: false5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             T���     �               '      t.string :subject_id, null: false5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             T���     �             �             5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             T���     �         	      !      t.string :type, null: false5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             T���     �         	      $      t.meta_data :type, null: false5�_�      	                     ����                                                                                                                                                                                                                                                                                                                                                             T���     �         	      $      t.meta_data :json, null: false5�_�      
           	          ����                                                                                                                                                                                                                                                                                                                                                             T���     �         	            t.json :json, null: false5�_�   	              
          ����                                                                                                                                                                                                                                                                                                                                                             T���    �         	      $      t.json :meta_data, null: false5�_�   
                        ����                                                                                                                                                                                                                                                                                                                                                V       T���    �         	      !      t.string :type, null: false5�_�                           ����                                                                                                                                                                                                                                                                                                                                                V       T��s     �         	      (      t.integer :subject_id, null: false5�_�                           ����                                                                                                                                                                                                                                                                                                                                                V       T��v    �         	      *      t.integer :subject_slug, null: false5�_�                           ����                                                                                                                                                                                                                                                                                                                                                V       T��     �         	    �         	    5�_�                           ����                                                                                                                                                                                                                                                                                                                                                V       T��     �         
      ,      t.string :feed_story_type, null: false5�_�                           ����                                                                                                                                                                                                                                                                                                                                                V       T��    �         
      0      t.timestamps :feed_story_type, null: false5�_�                           ����                                                                                                                                                                                                                                                                                                                                                V       T��3    �                   �      
   
    5�_�                    
        ����                                                                                                                                                                                                                                                                                                                            
           
           V        T� �    �   
          �   
          5�_�                           ����                                                                                                                                                                                                                                                                                                                            
           
           V        T� �    �   
             *    add_index :feed_stories, :subject_slug5�_�                            ����                                                                                                                                                                                                                                                                                                                            
          
          V       T��    �                  1class CreateFeedStories < ActiveRecord::Migration     def change   %    create_table :feed_stories do |t|   )      t.string :subject_slug, null: false   ,      t.string :feed_story_type, null: false         t.timestamps         t.json :meta_data       end       *    add_index :feed_stories, :subject_slug     end   end5�_�                            ����                                                                                                                                                                                                                                                                                                                            
           
           V        T�w    �      	       �             5�_�                           ����                                                                                                                                                                                                                                                                                                                                                  V        T�{     �             �             5�_�                           ����                                                                                                                                                                                                                                                                                                                                                  V        T�|    �               (    add_index :foos, :slug, unique: true5�_�                            ����                                                                                                                                                                                                                                                                                                                                                V       T��    �      	               t.string :slug5��