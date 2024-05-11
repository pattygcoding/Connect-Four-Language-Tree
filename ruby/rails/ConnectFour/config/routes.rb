Rails.application.routes.draw do
  resources :games, only: [:show, :new, :update]
  root 'games#new'
end
