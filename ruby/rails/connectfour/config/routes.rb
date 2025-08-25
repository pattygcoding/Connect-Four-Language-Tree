Rails.application.routes.draw do
  root 'games#index'
  resources :games do
    member do
      post :drop_piece
    end
  end
end