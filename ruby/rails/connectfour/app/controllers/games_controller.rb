class GamesController < ApplicationController
  before_action :set_game, only: [:show, :update]

  def new
    @game = Game.create
    redirect_to @game
  end

  def show
  end

  def update
    if @game.drop_piece(params[:column].to_i)
      redirect_to @game
    else
      flash[:alert] = "Invalid move or game over!"
      redirect_to @game
    end
  end  

  private

  def set_game
    @game = Game.find(params[:id])
  end
end
