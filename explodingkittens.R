options(warn = -1)
deck = c("EK", "EK", "EK", "EK", 
         "DF", "DF", "DF", "DF", "DF", "DF",
         "NO", "NO", "NO", "NO", "NO",
         "AT", "AT", "AT", "AT",
         "SK", "SK", "SK", "SK",
         "FA", "FA", "FA", "FA",
         "SH", "SH", "SH", "SH",
         "SF", "SF", "SF", "SF", "SF",
         "CC1", "CC2", "CC3", "CC4", "CC5",
         "CC1", "CC2", "CC3", "CC4", "CC5",
         "CC1", "CC2", "CC3", "CC4", "CC5",
         "CC1", "CC2", "CC3", "CC4", "CC5")
         
Deck = ""
DiscardPile = ""
Players = ""
playerNames = ""
alive = ""
lastcardplayed = ""
deadcount = 0
actions = ""
explode_probability = ""
order = ""

dbturn = ""

# Calculates the probability of a player exploding at any time 
CalculateExplodeProbability <- function(player) {
  if ("DF" %in% Players[[player]]) {
    exprob = 0 
  } else {
    num_kittens = length(Players) - deadcount - 1
    exprob = num_kittens / length(Deck)
  }
  return(exprob)
}


#Creates Players, Deals cards, shuffles deck.
StartGame <- function(numplayers = 2, playernames = c("player1", "player2"), computer = F) {
  if (numplayers > 5) {
    stop("currently not supported for 5+ player games. Working on it!")
  }
  strat = computer
  setupdeck = deck[deck != "EK" & deck != "DF"]
  setupdeck = sample(setupdeck)
  playerList = list()
  if(numplayers > length(playernames)) {
    for(i in (length(playernames)+1):numplayers) {
      playernames[i] = paste("player", i, sep = "")
    }
  }
  for (i in 1:numplayers) {
    hand = setupdeck[1:4]
    setupdeck = setupdeck[5:length(setupdeck)]
    hand[5] = "DF"
    playerList[[i]] = c("ALIVE", hand)
  }
  names(playerList) = playernames
  if (numplayers == 2) {
    setupdeck = c(setupdeck, "DF", "DF", "EK")
  } else {
    for (i in 1:(numplayers-1)) {
      setupdeck = c(setupdeck, "EK")
    }
    for (i in (numplayers+1):6) {
      setupdeck = c(setupdeck, "DF")
    }
  }
  setupdeck = sample(setupdeck)
  Deck <<- setupdeck
  DiscardPile <<- ""
  Players <<- playerList
  playerNames <<- names(Players)
  alive <<- playerNames
  lastcardplayed <<- ""
  deadcount <<- 0
  actions <<- "Start Game"
  explode_probability <<- ""
  Turns <<- data.frame(PlayerTurn = playerNames[1], Actions = actions, ExplodeProbability = explode_probability)
  
  order <<- rep(playerNames, 100)
  
  dbturn <<- ""
  for (i in 1:length(order)) {
    if (length(alive) == 1) {	# check if only one person is alive
      print(paste(alive, " wins! Everyone else exploded.", sep = ""))
      break
    } 
  	p = order[i]
    if (!(p %in% alive)) {
  	  next
  	}
    for (j in 1:(length(playerNames)-1)) {
      p_next = order[i + j]
      if (p_next %in% alive) {
        break
      }
    }
    actions <<- "StartTurn"
    explode_probability <<- CalculateExplodeProbability(p)
    turnover = F
    while (!turnover) {
      print(p)
      move = DoMoveHelper(p, strat)
  		turnover = DoMove(move, p, p_next, strat) 
  		if (lastcardplayed == "DF") {	# if player recently defused
  		  PlaceKitten(strat)
  		}
  		if (dbturn == "ON" & turnover == T) {
  		  if (lastcardplayed == "AT") {
  		    turnover = T
  		  } else {
  		    dbturn <<- "OFF"
          turnover = F
  		  }
  		}
  		Players[[p]][-1] <<- sample(Players[[p]][-1])
    }
  	turn = cbind(PlayerTurn = p, Actions = actions, ExplodeProbability = explode_probability)
    Turns <<- rbind(Turns, turn)
    
    
  }
	print("Game Over.")
}

PlaceKitten <- function(strat) {
  n = length(Deck)
  if (strat == T) {
    num = sample(1:length(Deck), size = 1)
  } else {
    print(paste("Deck has ", n, " cards"))
    num <- readline("Where would you like to place the Exploding Kitten? Anything else places it randomly. ")
    num = as.numeric(num)
  }
  if(is.na(num)) {
    place = sample(1:length(Deck), size = 1)
    former = Deck[1:place]
    latter = Deck[(place+1):n]
    Deck <<- c(former, "EK", latter)
    } else {
      if (num > n) {
        Deck <<- c(Deck, "EK")
        } else {
          if (num <= 1 ) {
            Deck <<- c("EK", Deck)
            } else {
              former = Deck[1:(num-1)]
              latter = Deck[(num+1):n]
              Deck <<- c(former, "EK", latter)
            }
        }
    }
}

DoMoveHelper <- function(p, strategy) {
  print("Your cards:")
  print(Players[[p]][-1])
  if (strategy == F) {
    move = readline(paste(p, ": What will you do? ", sep = ""))
    if (move == "quit") {
      stop("quitting game prematurely")
    }
  } else {
    computercards = Players[[p]][-1]
    move = DoComputerStrat(computercards)
  }
  return(move)
}

DoComputerStrat <- function(computercards) {
  card = computercards[1]
  move = ""
  if (is.na(card)) {
    move = "draw"
    print(move)
    return(move)
  }
  if (card == "AT") {
    move = "attack"
    print(move)
    return(move)
  
  }
  if (card == "SK") {
    move = "skip"
    print(move)
    return(move)
  }
  if (card == "FA") {
    move = "favor"
    print(move)
    return(move)
  }
  if (card == "SH") {
    move = "shuffle"
    print(move)
    return(move)
  }
  if (card == "SF") {
    move = "see the future"
    print(move)
    return(move)
  }
  if (card == "DF") {
    move = "draw"
    print(move)
    return(move)
  }
  if (card %in% computercards[-1]){
    move = "steal"
    print(move)
    return(move)
  } 
  if (move == "") {
    move = "draw"
    print(move)
    return(move)
  }
}

DoMove <- function(move, player, nextplayer, strat) {
	if (!(move %in% c("draw", "attack", "skip", 
					   "favor", "shuffle", 
					   "see the future", "steal"))) {
		print("invalid move")
		return(F)			   	
	}
	if (move == "draw") {
		return(Draw(player))
	}
	if (move == "attack") {
		return(Attack(player, nextplayer, strat))
	}
	if (move == "skip") {
		return(Skip(player, strat))
	}
	if (move == "favor") {
	  if (strat == T) {
	    return(Favor(player,
	                 target = alive[-match(player, alive)][1],
	                 strat))
	  } else {
		  return(Favor(player, target = "", strat))
	  }
	}
	if (move == "shuffle") {
		return(Shuffle(player, strat))
	}
	if (move == "see the future") {
		return(SeeTheFuture(player, strat))
	}
	if (move == "steal") {
	  if (strat == T) {
	    return(Steal(player, 
	                 target = alive[-match(player, alive)][1],
	                 cards = c(Players[[player]][2], Players[[player]][2]),
	                 strat))
	  } else {
		  return(Steal(player, target = "", cards = "", strat))
	  }
	}
}

Discard <- function(player, cards) {
  for (i in cards) {
    Players[[player]] <<- Players[[player]][-match(i, Players[[player]])]
    if (DiscardPile[1] != "") {
      DiscardPile <<- c(i, DiscardPile)
    } else {
      DiscardPile <<- cards
    }
  }
}

Draw <- function(player, nextplayer) {
    lastcardplayed <<- ""
	  nextcard = Deck[1]
  	Deck <<- Deck[-1]
  	if (nextcard == "EK" & !("DF" %in% Players[[player]])) {
  		print("You exploded. That's ok.  All kittens go to heaven.")
  		Discard(player, Players[[player]][2:length(Players[[player]])])
  		DiscardPile <<- c("EK", DiscardPile)
  		Players[[player]][1] <<- "DEAD"
    	deadcount <<- deadcount + 1
    	alive <<- alive[alive != player]
  		actions <<- c(actions, "Draw", "Exploded", "End Turn")
  		explode_probability <<- c(explode_probability, NA, NA, NA)
  	}
  	if (nextcard == "EK" & ("DF" %in% Players[[player]])) {
  		Discard(player, "DF")
  		lastcardplayed <<- "DF"
  		print("you ALMOST exploded. Hang in there. You're playing with kittens and you're still alive")
  		actions <<- c(actions, "Draw", "Defuse", "End Turn")
  		current_prob = CalculateExplodeProbability(player)
  		explode_probability <<- c(explode_probability, current_prob, current_prob, current_prob)
  	}
  	if (nextcard != "EK") {
  		Players[[player]] <<- c(Players[[player]], nextcard)
  		actions <<- c(actions, "Draw", "End Turn")
  		current_prob = CalculateExplodeProbability(player)
  		explode_probability <<- c(explode_probability, current_prob, current_prob)
  	}
  	return(T)
}

Nope <- function(player, count = 0, strat) {
	p = CheckNopes(player, strat)
	if (p == "None") {
		return(F)
	} else {
		return(PlayNopes(p, player, 1, strat))
	}
}

CheckNopes <- function(player, strat) {
	otherPlayers = playerNames[playerNames != player]
	for (i in otherPlayers) {
    if ("NO" %in% Players[[i]]) {
      if (strat == T) {
        reply = "Yes"
      } else {
        message = paste(i, ": Do you want to play your Nope? Type 'Yes' if you do. Otherwise, type anything else. ")
        reply <- readline(message)
      }
      if ("Yes" == reply) {
      	return(i)
      } else {
      	next
      }
    }
  }
	return("None")
}

PlayNopes <- function(noper, player, count, strat) {
	Discard(noper, "NO")
	actions <<- c(actions, paste(noper, " plays Nope", sep = ""))
	current_prob = CalculateExplodeProbability(player)
  explode_probability <<- c(explode_probability, current_prob)
	p = CheckNopes(noper, strat)
	if (p == "None") {
		if ((count %% 2) == 0) {
			return(F)
		} else {
			print("Previous action DENIED")
			return(T)
		}
	} else {
	  count = count + 1
		return(PlayNopes(p, player, count, strat))
	}
}


Attack <- function(player, nextplayer, strat) {
	if (!("AT" %in% Players[[player]])) {
		print("you do not have an Attack card")
		return(F)
    }
    lastcardplayed <<- "AT"
    actions <<- c(actions, "Attack")
    current_prob = CalculateExplodeProbability(player)
    explode_probability <<- c(explode_probability, current_prob)
    Discard(player, "AT")
    isNope <- Nope(player, count = 0, strat)
    if (isNope) {
    	return(F)
    } else {
    	dbturn <<- "ON"
    }
    return(T)
}

Skip <- function(player, strat) {
	if (!("SK" %in% Players[[player]])) {
    	print("you do not have an Skip card")
    	return(F)
  	}
	  lastcardplayed <<- "SK"
  	actions <<- c(actions, "Skip")
  	current_prob = CalculateExplodeProbability(player)
  	explode_probability <<- c(explode_probability, current_prob)
  	Discard(player, "SK")
  	isNope <- Nope(player, count = 0, strat)
    if (isNope) {
    	return(F)
    }
    return(T)
}
    
Favor <- function(player, target = "", strat) {
  if (strat == T) {
    actions <<- c(actions, "Favor")
    current_prob = CalculateExplodeProbability(player)
    explode_probability <<- c(explode_probability, current_prob)
    Discard(player, "FA")
    isNope <- Nope(player, count = 0, strat)
    if (isNope) {
      return(F)
    } else {
      card = Players[[target]][2]
      Players[[target]] <<- Players[[target]][-match(card, Players[[target]])]
      Players[[player]] <<- c(Players[[player]], card)
      return(F)
    }
  }
	if (!("FA" %in% Players[[player]])) {
    	print("you do not have a Favor card")
    	return(F)
  	}
	  lastcardplayed <<- "FA"
  	if (target == "") {
  		otherPlayers = playerNames[playerNames != player]
  		print("Players:")
  		print(otherPlayers)
  		message1 = "Who would you like a favor from?"
  		t <- readline(message1)
  		if (!(t %in% otherPlayers) || Players[[t]][1] == "DEAD" || length(Players[[t]]) < 2) {
  			print("Cannot get a favor from this player")
  			return(F)
  		}
  	} else {
  		t = target
  	}
	  actions <<- c(actions, "Favor")
	  current_prob = CalculateExplodeProbability(player)
	  explode_probability <<- c(explode_probability, current_prob)
	  Discard(player, "FA")
  	isNope <- Nope(player, count = 0, strat)
    if (isNope) {
    	return(F)
    } else {
      good = F
      while(!good) {
    	  print(paste(t, "'s cards:"))
  		  print(Players[[t]][-1])
    	  message2 = paste(t, ": Enter a card to give to ", player, sep = "")
    	  card <- readline(message2)
    	  if (!(card %in% Players[[t]])) {
    		  print("You do not have this card.")
    	  } else {
    	    good = T
    	  }
      }
      Players[[t]] <<- Players[[t]][-match(card, Players[[t]])]
      Players[[player]] <<- c(Players[[player]], card)
    }
  	return(F)
}

Shuffle <- function(player, strat) {
	if (!("SH" %in% Players[[player]])) {
    	print("you do not have a Shuffle card")
    	return(F)
  	}
	  lastcardplayed <<- "SH"
  	actions <<- c(actions, "Shuffle")
  	current_prob = CalculateExplodeProbability(player)
  	explode_probability <<- c(explode_probability, current_prob)
  	Discard(player, "SH")
  	isNope <- Nope(player, count = 0, strat)
    if (isNope) {
    	return(F)
    } 
    Deck <<- sample(Deck)
    print("Deck is shuffled")
    return(F)
}

SeeTheFuture <- function(player,strat) {
	if (!("SF" %in% Players[[player]])) {
    	print("you do not have a See the Future card")
    	return(F)
	}
    lastcardplayed <<- "SF"
  	actions <<- c(actions, "See the Future")
  	current_prob = CalculateExplodeProbability(player)
  	explode_probability <<- c(explode_probability, current_prob)
  	Discard(player, "SF")
  	isNope <- Nope(player, count = 0, strat)
    if (isNope) {
    	return(F)
    } 
    print("FOR PRIVATE VIEWING ONLY")
    print(Deck[1:3])
    return(F)
}

Steal <- function(player, target = "", cards = "", strat) {
  otherPlayers = playerNames[playerNames != player]
	if (length(Players[[player]]) < 2) {
    	print("you do not have any cards to steal with")
    	return(F)
  	}
  	cardset = cards
  	if (cardset == "") {
  		print(Players[[player]][-1])
  		message1 = "How will you steal? Enter number of cards: "
  		n <- readline(message1)
  		if (!(n %in% c(2, 3, 5))) {
  			print("cannot steal with this number of cards")
  			return(F)
  		}
  		for (i in 1:n) {
  			inputcard <- readline("Enter card: ") 
  			if (cardset != "") {
  				cardset = c(cardset, inputcard)
  			} else {
  				cardset = inputcard
  			}
  		}
  		if (!CheckCardSet(cardset, player)) {
  			print("you do not have one or more of these cards, or cards are not a valid set")
  			return(F)
  		}
  	}
  	t = target
  	if (t == "") {
  		print("You may steal from the 'discard pile' or from these players: ")
  		print(otherPlayers)
  		message2 = "Who/where would you like to steal from? "
  		t <- readline(message2)
  	}
  	if (t == "discard pile") {
  		if (length(cardset) != 5) {
  			print("you need 5 unique cards to steal from the 'discard pile'")
  			return(Steal(player, target = "", cards = cardset, strat))
  		}
  		print(DiscardPile)
  		message3 = "Enter the card you want to steal from the discard pile: "
  		cd <- readline(message3)
  		if (!(cd %in% DiscardPile)) {
  			print(paste(cd, " has not been discarded yet.", sep = ""))
  			return(Steal(player, target = t, cards = cardset, strat))
  		}
  		actions <<- c(actions, "Steal")
  		current_prob = CalculateExplodeProbability(player)
  		explode_probability <<- c(explode_probability, current_prob)
  		lastcardplayed <<- cardset[1]
  		Discard(player, cardset)
  		isNope <- Nope(player, count = 0, strat)
    		if (isNope) {
    			return(F)
    		}
        DiscardPile <<- DiscardPile[-match(cd, DiscardPile)]
        Players[[player]] <<- c(Players[[player]], cd)
        return(F)
    }
  	if (!(t %in% otherPlayers) || Players[[t]][1] == "DEAD" || length(Players[[t]]) < 2) {
  		print("Cannot steal from this player")
  		return(F)
  	}
  	actions <<- c(actions, "Steal")
  	current_prob = CalculateExplodeProbability(player)
  	explode_probability <<- c(explode_probability, current_prob)
  	lastcardplayed <<- cardset[1]
  	Discard(player, cardset)
  	isNope <- Nope(player, count = 0, strat)
    if (isNope) {
    	return(F)
    }
    if (length(cardset) == 2) {
    	cd = sample(Players[[t]][-1], size = 1)
    } 
    if (length(cardset) == 3) {
    	message4 = paste("Enter the card you want to steal from ", target, sep = "")
    	cd <- readline(message4)
    	if (!(cd %in% Players[[t]])) {
    		print("They do not have this card.  Tough luck.")
    		return(F)
    	}
    }
    if (length(cardset) == 5) {
      print("Did you mean to steal from the 'discard pile'?")
      return(Steal(player, cards = cardset, target = "discard pile"))
    }
    Players[[target]] <<- Players[[target]][-match(cd, Players[[target]])]
    Players[[player]] <<- c(Players[[player]], cd)
  	return(F)
}

CheckCardSet <- function(set, player) {
	hand = Players[[player]][-1]
	if ((length(set) == 2 || length(set) == 3) & length(unique(set)) == 1) {
		return(CheckHand(set, hand))
	}
	if (length(unique(set)) == 5) {
		return(CheckHand(set, hand))
	}
	return(F)
}

CheckHand <- function(set, hand) {
	if (length(set) == 1) {
    	if (set[1] %in% hand) {
      		return(T)
    	} else {
      		return(F)
    	}
  	}
  	if (length(set) > 1) {
	  for (i in 1:length(hand)) {
		  if (set[1] == hand[i])	{
			  return(CheckHand(set[-1], hand[-i]))
		  } 
	  }
  }
}
    


