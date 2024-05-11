# app/controllers/games_controller.rb
class GamesController < ApplicationController
  def show
    @game = Game.find(params[:id])
  end

  def update
    @game = Game.find(params[:id])
    if @game.drop_piece(params[:column])
      respond_to do |format|
        format.js
      end
    else
      render js: "alert('Invalid move!')"
    end
  end
end
