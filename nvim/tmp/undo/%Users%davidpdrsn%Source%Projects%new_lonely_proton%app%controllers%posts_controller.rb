Vim�UnDo� n�J�V�F�ZB��L����Et��#�+)���   R                                  Tp]�    _�                             ����                                                                                                                                                                                                                                                                                                                                                             Tk��    �               R   :# Controller responsible for crud actions related to posts   -class PostsController < ApplicationController   E  before_action :require_authentication, only: [:create, :new, :edit,   B                                                :update, :destroy]         def index   %    @posts = DecoratedCollection.new(   $      Post.recently_published_first,         PostWithPrettyDate,       )     end       
  def show   :    @post = PostWithPrettyDate.new(Post.find(params[:id]))       *    require_authentication if @post.draft?     end       	  def new   #    @form = new_post_form(Post.new)     end         def create   $    new_post = Post.new(post_params)   H    new_post.slug = BuildsUniqueSlug.new(Post.all, new_post).unique_slug       *    @post = make_post_observable(new_post)           if @post.save   #      flash.notice = "Post created"         redirect_to @post       else   $      flash.alert = "Post not saved"         render :new       end     end       
  def edit   1    @form = new_post_form(Post.find(params[:id]))     end         def update   8    @post = make_post_observable(Post.find(params[:id]))            if @post.update(post_params)   #      flash.notice = "Post updated"         redirect_to @post       else   &      flash.alert = "Post not updated"         render :edit       end     end         def destroy   /    Post.find_by(id: params[:id]).try(:destroy)       redirect_to admin_path     end       	  private          def make_post_observable(post)       ObservableRecord.new(         post,         CompositeObserver.new([   6        PublishObserver.new(is_draft: params[:draft]),   6        ParseMarkdownObserver.new(MarkdownParser.new),   G        TaggingObserver.new(Tag.find_for_ids(params[:post][:tag_ids])),   	      ]),       )     end         def post_params   9    params.require(:post).permit :title, :markdown, :link     end         def new_post_form(post)       NewPostForm.new(         post,   5      DecoratedCollection.new(Tag.all, TagWithDomId),       )     end   end5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             Tp]v     �         S          �         R    5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             Tp]�     �         T          here is some more code 5�_�                            ����                                                                                                                                                                                                                                                                                                                               N                  V   N    Tp]�    �                    O    here is some more code BuildsUniqueSlug.new(Post.all, new_post).unique_slug5�_�                           ����                                                                                                                                                                                                                                                                                                                               N                  V   N    Tp]�     �         S          �         R    5�_�                            ����                                                                                                                                                                                                                                                                                                                               D                  V   D    Tp]�    �                    E    here is code BuildsUniqueSlug.new(Post.all, new_post).unique_slug5�_�                          ����                                                                                                                                                                                                                                                                                                                                                  V        Tp]�     �         R    5�_�                            ����                                                                                                                                                                                                                                                                                                                                                   V        Tp]�     �         S       �         S    5�_�                            ����                                                                                                                                                                                                                                                                                                                                                   V        Tp]�     �                4BuildsUniqueSlug.new(Post.all, new_post).unique_slug5�_�                            ����                                                                                                                                                                                                                                                                                                                                                  V        Tp]�     �                $    new_post = Post.new(post_params)   H    new_post.slug = BuildsUniqueSlug.new(Post.all, new_post).unique_slug5�_�                    %        ����                                                                                                                                                                                                                                                                                                                                                  V        Tp]�     �   %   (   P    �   %   &   P    5�_�                    '       ����                                                                                                                                                                                                                                                                                                                                                  V        Tp]�     �   '   )   S          �   '   )   R    5�_�                             ����                                                                                                                                                                                                                                                                                                                                                  V        Tp]�     �               S   :# Controller responsible for crud actions related to posts   -class PostsController < ApplicationController   E  before_action :require_authentication, only: [:create, :new, :edit,   B                                                :update, :destroy]         def index   %    @posts = DecoratedCollection.new(   $      Post.recently_published_first,         PostWithPrettyDate,       )     end       
  def show   :    @post = PostWithPrettyDate.new(Post.find(params[:id]))       *    require_authentication if @post.draft?     end       	  def new   #    @form = new_post_form(Post.new)     end         def create       *    @post = make_post_observable(new_post)           if @post.save   #      flash.notice = "Post created"         redirect_to @post       else   $      flash.alert = "Post not saved"         render :new       end     end       
  def edit   1    @form = new_post_form(Post.find(params[:id]))   $    new_post = Post.new(post_params)   H    new_post.slug = BuildsUniqueSlug.new(Post.all, new_post).unique_slug   8    BuildsUniqueSlug.new(Post.all, new_post).unique_slug     end         def update   8    @post = make_post_observable(Post.find(params[:id]))            if @post.update(post_params)   #      flash.notice = "Post updated"         redirect_to @post       else   &      flash.alert = "Post not updated"         render :edit       end     end         def destroy   /    Post.find_by(id: params[:id]).try(:destroy)       redirect_to admin_path     end       	  private          def make_post_observable(post)       ObservableRecord.new(         post,         CompositeObserver.new([   6        PublishObserver.new(is_draft: params[:draft]),   6        ParseMarkdownObserver.new(MarkdownParser.new),   G        TaggingObserver.new(Tag.find_for_ids(params[:post][:tag_ids])),   	      ]),       )     end         def post_params   9    params.require(:post).permit :title, :markdown, :link     end         def new_post_form(post)       NewPostForm.new(         post,   5      DecoratedCollection.new(Tag.all, TagWithDomId),       )     end   end5�_�                          ����                                                                                                                                                                                                                                                                                                                                                  V        Tp]�     �         R    �         R      ^    BuildsUniqueSlug.new(Post.all, new_post).unique_slug@post = make_post_observable(new_post)5�_�                           ����                                                                                                                                                                                                                                                                                                                                                  V        Tp]�     �         R    �         R      ^ BuildsUniqueSlug.new(Post.all, new_post).unique_slug   @post = make_post_observable(new_post)5�_�      	                    ����                                                                                                                                                                                                                                                                                                                                                V       Tp]�     �               5�_�      
           	          ����                                                                                                                                                                                                                                                                                                                               D                  V   D    Tp]�     �         P          �         Q          E    here is code BuildsUniqueSlug.new(Post.all, new_post).unique_slug5�_�   	               
           ����                                                                                                                                                                                                                                                                                                                               D                  V   D    Tp]�     �              5�_�                            ����                                                                                                                                                                                                                                                                                                                                                V       Tp]�     �               5��