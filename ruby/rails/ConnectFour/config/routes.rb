# config/routes.rb
Rails.application.routes.draw do
  root to: "games#show", id: 1  # Adjust the ID as needed or implement game creation
  resources :games, only: [:show, :update]
end
