#Deck of cards
deck <- c(1,2,3,4,5,6,7,8,9,10,"Jack","Queen","King", 1,2,3,4,5,6,7,8,9,10,"Jack","Queen","King", 1,2,3,4,5,6,7,8,9,10,"Jack","Queen","King", 1,2,3,4,5,6,7,8,9,10,"Jack","Queen","King")

win = 0
loss = 0
player_sum = 0
dealer_sum = 0
gameis <- "not over"
"Jack" <- 10
"Queen" <- 10
"King" <- 10
answer <- "Begin Game"

while(gameis == "not over"){
  
  while(answer == "Begin Game"){
  player_cards <- sample(deck,2) #deal 2 cards to player
  remaining <- setdiff(deck,player_cards) #what cards remain
  
  cat("You got ",player_cards, "\n")
      
  player_cards <- sapply(player_cards, function(card) 
    ifelse(card %in% c("Jack", "Queen", "King"), get(card), as.numeric(card)))
  player_sum = sum(player_cards)
  answer <- "Ask again"
}
  
  while(answer == "Yes"){
    hit_card <- sample(remaining, 1) # Hit Card
    
    hit_card <- sapply(hit_card, function(card) 
      ifelse(card %in% c("Jack", "Queen", "King"), get(card), as.numeric(card)))
    
    #add card to total
    player_sum = player_sum + sum(hit_card)
    
    remaining <- setdiff(remaining, hit_card) #update remaining
    cat("You hit and got ", hit_card, "\n")
    
    if(player_sum > 21){
      cat("You busted", "\n")
      gameis <- "over"
      answer <- "Begin game"
      loss = loss + 1
    }
    if(player_sum == 21){
      cat("BLACKJACK!", "\n")
      answer <- "No"
    }
    else{
    answer <- "Ask again"
    }
  }
  
  while(answer == "Ask again"){
    answer <- readline("Hit? Yes or No? \n")
  }
  
  while(answer == "No"){
    while(dealer_sum == 0){
    dealer_cards <- sample(remaining, 2)
    remaining <- setdiff(remaining, dealer_cards)
    
    dealer_cards <- sapply(dealer_cards, function(card) 
      ifelse(card %in% c("Jack", "Queen", "King"), get(card), as.numeric(card)))
    
    dealer_sum = sum(dealer_cards)
    
    cat("Dealer got ", dealer_cards, "\n")
    if(dealer_sum > player_sum & dealer_sum < 22){
      cat("Dealer wins (", dealer_sum, "-", player_sum, ")", "\n")
      gameis = "over"
      answer <- "Begin game"
      loss = loss + 1
    }
    else if(dealer_sum < player_sum & dealer_sum >= 17){
      cat("Dealer pushes" , "\n")
      gameis = "over"
      win = win + 1
      answer <- "Begin game"
    }
    }
    #keep hitting till push, win, or bust
    while(dealer_sum < player_sum & dealer_sum < 17){
      dealerhit_card <- sample(remaining, 1)
      
      dealerhit_card <- sapply(dealerhit_card, function(card) 
        ifelse(card %in% c("Jack", "Queen", "King"), get(card), as.numeric(card)))
      
      cat("Dealer hit and got ", dealerhit_card, "\n")
      
      dealer_sum = dealer_sum + sum(dealerhit_card)
      
      # if Dealer busts
      if(dealer_sum >= 22){
        cat("Dealer busted", "\n")
        gameis = "over"
        win = win + 1
        answer <- "Begin game"
      }
      #dealer wins
      else if(dealer_sum > player_sum & dealer_sum < 22){
        cat("Dealer wins (", dealer_sum, "-", player_sum, ")", "\n")
        gameis = "over"
        answer <- "Begin game"
        loss = loss + 1
      }
    }
    #dealer pushes
    if(dealer_sum < player_sum & dealer_sum >= 17){
      cat("Dealer pushes" , "\n")
      gameis = "over"
      win = win + 1
      answer <- "Begin game"
    }
    if(dealer_sum == player_sum){
      cat("Push (tie)" , "\n")
      gameis = "over"
      answer <- "Begin game"
    }
  }
  while(gameis == "over"){
    cat(win, "win(s) \n")
    cat(loss, "loss(es) \n")
    player_sum = 0
    dealer_sum = 0
    gameis <- "not over"
    answer <- "Begin Game"
    deck <- c(1,2,3,4,5,6,7,8,9,10,"Jack","Queen","King", 1,2,3,4,5,6,7,8,9,10,"Jack","Queen","King", 1,2,3,4,5,6,7,8,9,10,"Jack","Queen","King", 1,2,3,4,5,6,7,8,9,10,"Jack","Queen","King")
    remaining <- c()
  }
}


