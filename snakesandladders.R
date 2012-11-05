# definitions
board <- c(0,0,0,0,0,0,0,0,0);
boardsize <- length(board);

# going to define 1 snake and 1 ladder
snake <- c(8,3) #if you hit square 8, go back to 3
ladder <- c(4,6) #if you hit square 4, go to square 6

playerone <- 1;
playertwo <- 2;

# returns a number between 1 and 6
rolldice <- function(x) {
  x1 <- sample(c(1,2,3,4,5,6), 1, replace=FALSE);
  return(x1);
}

game <- function(x){
  winner=0;
  playerspaces <- c(1,1);
  currentplayer <- playerone; #start with player 1
  printboard(playerspaces[1], playerspaces[2]);
  overall <- paste();
  while(TRUE){
    
    # playerone moves
    dieroll <- rolldice(1);
    oldspace <- playerspaces[currentplayer];
    moveahead <- playerspaces[currentplayer] + dieroll;
    overall <- paste(overall, "Player ", currentplayer, " rolls a ", dieroll, " and can move ahead ", moveahead);
    # check to see if this move is acceptable
    if(moveahead == boardsize){
      winner <- currentplayer;
      return(winner);
    } else if(moveahead < boardsize){
      # move ahead, check
      if(moveahead == snake[1]){
        playerspaces[currentplayer] <- snake[2];
      } else if(moveahead == ladder[1]){
        playerspaces[currentplayer] <- ladder[2]
      } else { #it's fine
        playerspaces[currentplayer] <- moveahead;
      }
    } # the other case is we have rolled past the board,
      # in which case we wait
   # writeLines(paste("Player ", currentplayer, " rolled a ",
   #                 dieroll, ", moving from ", oldspace,
   #                  " to ", playerspaces[currentplayer]));
    
    printboard(playerspaces[1], playerspaces[2]);
    # now the other player moves
    currentplayer = 3 - currentplayer;
    #if cp is 2, cp will be 1
  }
}

# prints the board
printboard <- function(p1, p2){
  loopcounter = 1;
  toprint <- paste();
  while(loopcounter <= boardsize){
    toprint <- paste(toprint, "[");
    if(loopcounter == p1){
      toprint <- paste(toprint, "1");
    }
    if(loopcounter == p2){
      toprint <- paste(toprint, "2");
    }
    toprint <- paste(toprint, "] ");
    loopcounter = loopcounter + 1;
  }
  writeLines(toprint);
}

playgame <- function(x) {
  winner <- game(1);
  writeLines(paste("The winner is Player ", winner));
}