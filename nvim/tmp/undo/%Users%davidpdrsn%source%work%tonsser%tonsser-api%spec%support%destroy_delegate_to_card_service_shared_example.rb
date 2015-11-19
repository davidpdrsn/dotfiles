Vim�UnDo� g���ɶ1�A
���`���
N�)�k!��A      9RSpec.shared_examples "create delegate to CardService" do            
       
   
   
    T��    _�                             ����                                                                                                                                                                                                                                                                                                                                                             T�]     �               5�_�                            ����                                                                                                                                                                                                                                                                                                                                                             T�_     �                  5�_�                            ����                                                                                                                                                                                                                                                                                                                                                  V        T�`    �                )  let(:user) { build_authenticated_user }     let(:match) { create :match }   /  let(:card_service) { double("card_service") }         before do   O    allow(card_service).to receive(:create!).with(match, user).and_return(card)   '    allow(CardService).to receive(:new)   J      .with(user, type.to_s.classify.constantize).and_return(card_service)     end       5  it "delegates to CardService to create the card" do   J    post :create, match_slug: match.slug, type => { user_slug: user.slug }       E    expect(card_service).to have_received(:create!).with(match, user)     end         it "exposes the card" do   *    expect(controller).to expose(card).at(         :post,         :create,   >      match_slug: match.slug, type => { user_slug: user.slug }   %    ).with_serializer(CardSerializer)     end5�_�                            ����                                                                                                                                                                                                                                                                                                                                                  V        T�e     �             �             5�_�                           ����                                                                                                                                                                                                                                                                                                                                                  V        T�f     �                    end�                I      expect(card_service).to have_received(:destroy!).with(user.id.to_s)�                "      delete :destroy, id: user.id�   	             5        .with(user, RedCard).and_return(card_service)�      
          )      allow(CardService).to receive(:new)�      	          B      allow(card_service).to receive(:destroy!).with(user.id.to_s)�                '      card = build_stubbed :yellow_card�                +      card_service = double("card_service")�                      match = create :match�                %      user = build_authenticated_user�                $    it "delegates to CardService" do5�_�                           ����                                                                                                                                                                                                                                                                                                                                                V       T�l    �                %    card = build_stubbed :yellow_card5�_�                    	       ����                                                                                                                                                                                                                                                                                                                                                V       T�v     �      
         3      .with(user, RedCard).and_return(card_service)5�_�      	              	   %    ����    	   )                                                                                                                                                                                                                                                                                                                                       V       T��     �      
         I      .with(user, type.to_s.classify.constatize).and_return(card_service)5�_�      
           	   	   %    ����    	   )                                                                                                                                                                                                                                                                                                                                       V       T��    �      
         I      .with(user, type.to_s.classify.concertize).and_return(card_service)5�_�   	               
          ����    	   )                                                                                                                                                                                                                                                                                                                      #                  V        T��    �                9RSpec.shared_examples "create delegate to CardService" do5��