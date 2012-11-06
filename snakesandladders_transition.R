#make the 10x10 board
  # snake on 8, going to 3
  # snake on 6, going to 1
  # ladder on 4, going to 7
board1 <- c(
1/6, 1/6, 1/6, 0/6, 1/6, 0/6, 2/6, 0, 0, 0, #1
1/6, 0, 2/6, 0/6, 1/6, 0/6, 2/6, 0/6, 0, 0, #2
1/6, 0, 1/6, 0/6, 1/6, 0/6, 2/6, 0/6, 1/6, 0, #3
0/6, 0, 0/6, 0, 0/6, 0/6, 6/6, 0/6, 0/6, 0/6, #4 --should never land on 4
1/6, 0, 1/6, 0, 1/6, 0/6, 1/6, 0/6, 1/6, 1/6, #5
1, 0, 0, 0, 0, 0/6, 0/6, 0/6, 0/6, 0/6,   #6 -- should never land on 6
0, 0, 1/6, 0, 0, 0, 3/6, 0/6, 1/6, 1/6,     #7
0, 0, 6/6, 0, 0, 0, 0, 0/6, 0/6, 0/6,       #8 -- should never land on 8
0, 0, 0, 0, 0, 0, 0, 0, 5/6, 1/6,         #9
0, 0, 0, 0, 0, 0, 0, 0, 0, 6/6            #10
  )

createboard <- function(board){
  rowlength = sqrt(length(board))
  return(rowlength)
}

movetospace <- function(rowlength, boardmatrix, at){
  return(sample(1:rowlength, 1, replace=FALSE, prob = boardmatrix[at,]))
}

game <- function(rowlength, boardmatrix){
  winner = 0
  playerone <- 1
  playertwo <- 2
  playerspaces <- c(1,1) #both start off the board
  currentplayer = playerone #start with playerone
  playcount = 0
  while(winner == 0){
    playerspaces[currentplayer] = movetospace(rowlength, boardmatrix, playerspaces[currentplayer])
    if(playerspaces[currentplayer] == rowlength){
      winner = currentplayer
    } else {
      currentplayer = 3 - currentplayer #switch players, repeat
    }
    playcount = playcount + 1
  }
  return(playcount/2) #a play is defined as one player's turn, 
}

playgame <- function(board){
  rowlength <- createboard(board)
  boardmatrix <- matrix(board, nrow = rowlength, ncol = rowlength, byrow=TRUE)
  playcount = game(rowlength, boardmatrix)
  # cat("The game was won in", playcount, "rounds\n")
  return(playcount)
}

