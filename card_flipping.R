card_flip <- function(n) {
  
  cards = sample(n)
  flips = 0
  
  while (cards[1] != 1) {
    
    top_card = cards[1] 
    cards[1:top_card] = rev(cards[1:top_card])
    flips = flips + 1
    
  }
  
  in_pos = mean(1:n == cards)
  return(c(in_pos = in_pos, flips = flips))
  
}

ns = c(10, 50, 100, 500, 1000, 5000, 10000)
sims = 100
flip_res = matrix(nrow = length(ns), ncol = sims)
in_pos_res = matrix(nrow = length(ns), ncol = sims)

for (i in 1:length(ns)) {
  
  results = replicate(sims, card_flip(ns[i]))
  flip_res[i,] = results["flips",]
  in_pos_res[i,] = results["in_pos",]
  
  
}


plot(log(ns), log(rowMeans(flip_res)), 
     xlab = "log(deck size)", ylab = "log(Number of flips)", 
     main = "Number of flips are average of 100 simulations \n for each deck size")
abline(0,1)

plot(ns, rowMeans(in_pos_res), 
     xlab = "deck size", ylab = "Proportion of cards in correct position", 
     main = "Averages of 100 simulations \n for each deck size")






