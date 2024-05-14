#!/bin/bash

# Initialize the board
rows=6
cols=7
board=()

for ((i=0; i<rows; i++)); do
    for ((j=0; j<cols; j++)); do
        board[$i,$j]="."
    done
done

# Display the board
display_board() {
    for ((i=0; i<rows; i++)); do
        for ((j=0; j<cols; j++)); do
            printf "%s " "${board[$i,$j]}"
        done
        echo
    done
}

# Check for a win
check_win() {
    local player=$1
    local i j

    # Check horizontal
    for ((i=0; i<rows; i++)); do
        for ((j=0; j<cols-3; j++)); do
            if [[ "${board[$i,$j]}" == "$player" && "${board[$i,$((j+1))]}" == "$player" && "${board[$i,$((j+2))]}" == "$player" && "${board[$i,$((j+3))]}" == "$player" ]]; then
                return 0
            fi
        done
    done

    # Check vertical
    for ((i=0; i<rows-3; i++)); do
        for ((j=0; j<cols; j++)); do
            if [[ "${board[$i,$j]}" == "$player" && "${board[$((i+1)),$j]}" == "$player" && "${board[$((i+2)),$j]}" == "$player" && "${board[$((i+3)),$j]}" == "$player" ]]; then
                return 0
            fi
        done
    done

    # Check diagonal (bottom left to top right)
    for ((i=3; i<rows; i++)); do
        for ((j=0; j<cols-3; j++)); do
            if [[ "${board[$i,$j]}" == "$player" && "${board[$((i-1)),$((j+1))]}" == "$player" && "${board[$((i-2)),$((j+2))]}" == "$player" && "${board[$((i-3)),$((j+3))]}" == "$player" ]]; then
                return 0
            fi
        done
    done

    # Check diagonal (top left to bottom right)
    for ((i=0; i<rows-3; i++)); do
        for ((j=0; j<cols-3; j++)); do
            if [[ "${board[$i,$j]}" == "$player" && "${board[$((i+1)),$((j+1))]}" == "$player" && "${board[$((i+2)),$((j+2))]}" == "$player" && "${board[$((i+3)),$((j+3))]}" == "$player" ]]; then
                return 0
            fi
        done
    done

    return 1
}

# Player move
player_move() {
    local player=$1
    local col

    while true; do
        read -p "Player $player, enter column (1-$cols): " col
        ((col--))

        if [[ $col -ge 0 && $col -lt $cols ]]; then
            for ((i=rows-1; i>=0; i--)); do
                if [[ "${board[$i,$col]}" == "." ]]; then
                    board[$i,$col]=$player
                    return
                fi
            done
        fi

        echo "Invalid move, try again."
    done
}

# Main game loop
current_player="X"

while true; do
    display_board
    player_move $current_player

    if check_win $current_player; then
        display_board
        echo "Player $current_player wins!"
        break
    fi

    # Switch player
    if [[ $current_player == "X" ]]; then
        current_player="O"
    else
        current_player="X"
    fi
done
