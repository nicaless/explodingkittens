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
actions = ""
explode_probability = ""
order = ""

# Calculates the probability of a player exploding at any time 
CalculateExplodeProbability <- function(player) {
  if ("DF" %in% Players[[player]]) {
    exprob = 0 
  } else {
    num_kittens = length(Players) - deadcount + 1
    exprob = num_kittens / length(Deck)
  }
  return(exprob)
}


#Creates Players, Deals cards, shuffles deck.
StartGame <- function(numplayers = 2, playernames = c("player1", "player2")) {
  if (numplayers > 4) {
    stop("currently not supported for 5+ player games. Working on it!")
  }
  setupdeck = deck[deck != "EK" & deck != "DF"]
  setupdeck = sample(setupdeck)
  playerList = list()
  if(numplayers > length(playernames)) {
    n = numplayers - length(playernames)
    for(i in (numplayers+1):n) {
      playernames[i] = paste("player", i, sep = "")
    }
  }
  for (i in 1:numplayers) {
    hand = setupdeck[1:4]
    setupdeck = setupdeck[5:length(setupdeck)]
    hand[5] = "DF"
    playerList[[i]] = c("WAIT", hand)
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
  DeadKittenPile <<- ""
  Players <<- playerList
  playerNames <<- names(Players)
  alive <<- playerNames
  lastcardplayed <<- ""
  deadcount <<- 0
  #Players[[1]][1] <<- "TURN"
  #Players[[1]][1] <<- "ALIVE"
  actions <<- "Start Game"
  explode_probability <<- ""
  Turns <<- data.frame(PlayerTurn = playerNames[1], Actions = actions, ExplodeProbability = explode_probability)
  
  order <<- rep(playerNames, 100)
  for (i in 1:length(order)) {
    if (length(alive) == 1) {	# check if only one person is alive
      print(paste(alive, " wins! Everyone else exploded.", sep = ""))
      break
    } 
  	p = order[i]
  	#if (!(Players[[p]][1] %in% c("TURN", "DOUBLETURN"))) {
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
      move = DoMoveHelper(p)
  		#move = DoMoveHelperStrat(p)
  		turnover = DoMove(move, p, p_next) 
  		if (lastcardplayed == "DF") {	# if player recently defused
  		  PlaceKitten()
  		}
  		if (Players[[p]][1] == "DOUBLETURN") {
  		  #if (lastcardplayed == "AT") {
  		  #  Players[[p]][1] <<- "ALIVE"
  		  #  Players[[p_next]][1] <<- "TURN"
  		  #} else {
  		    Players[[p]][1] <<- "ALIVE"
          turnover = F
  		  #}
  		}
    }
    #current_prob = CalculateExplodeProbability(player)
  	turn = cbind(PlayerTurn = p, Actions = actions, ExplodeProbability = explode_probability)
    print(turn)
    Turns <<- rbind(Turns, turn)
    
    
  }
	print("Game Over.")
}

PlaceKitten <- function() {
  n = length(Deck)
  print(paste("Deck has ", n, " cards"))
  num <- readline("Where would you like to place the Exploding Kitten? Anything else places it randomly.")
  num = as.numeric(num)
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

DoMoveHelper <- function(p) {
  print("Your cards:")
  print(Players[[p]][-1])
  move = readline(paste(p, ": What will you do?", sep = ""))
  if (move == "quit") {
    stop("quitting game prematurely")
  }
  return(move)
}

DoMove <- function(move, player, nextplayer) {
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
		return(Attack(player, nextplayer))
	}
	if (move == "skip") {
		return(Skip(player))
	}
	if (move == "favor") {
		return(Favor(player))
	}
	if (move == "shuffle") {
		return(Shuffle(player))
	}
	if (move == "see the future") {
		return(SeeTheFuture(player))
	}
	if (move == "steal") {
		return(Steal(player))
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

Nope <- function(player, count = 0) {
	p = CheckNopes(player)
	if (p == "None") {
		return(F)
	} else {
		return(PlayNopes(p, player, 1))
	}
}

CheckNopes <- function(player) {
	otherPlayers = playerNames[playerNames != player]
	for (i in otherPlayers) {
    if ("NO" %in% Players[[i]]) {
      message = paste(i, ": Do you want to play your Nope? Type 'Yes' if you do. Otherwise, type anything else. ")
      reply <- readline(message)
      if ("Yes" == reply) {
      	return(i)
      } else {
      	next
      }
    }
  }
	return("None")
}

PlayNopes <- function(noper, player, count) {
	Discard(noper, "NO")
	actions <<- c(actions, paste(noper, " plays Nope", sep = ""))
	current_prob = CalculateExplodeProbability(player)
  explode_probability <<- c(explode_probability, current_prob)
	p = CheckNopes(noper)
	if (p == "None") {
		if ((count %% 2) == 0) {
			return(F)
		} else {
			print("Previous action DENIED")
			return(T)
		}
	} else {
	  count = count + 1
		return(PlayNopes(p, player, count))
	}
}


Attack <- function(player, nextplayer) {
	if (!("AT" %in% Players[[player]])) {
		print("you do not have an Attack card")
		return(F)
    }
    lastcardplayed <<- "AT"
    actions <<- c(actions, "Attack")
    Discard(player, "AT")
    isNope <- Nope(player, count = 0)
    if (isNope) {
    	return(F)
    } else {
    	Players[[nextplayer]][1] <<- "DOUBLETURN"
    	Players[[player]][1] <<- "ALIVE"
    }
  	current_prob = CalculateExplodeProbability(player)
  	explode_probability <<- c(explode_probability, current_prob)
    return(T)
}

Skip <- function(player) {
	if (!("SK" %in% Players[[player]])) {
    	print("you do not have an Skip card")
    	return(F)
  	}
	  lastcardplayed <<- "SK"
  	actions <<- c(actions, "Skip")
  	Discard(player, "SK")
  	isNope <- Nope(player, count = 0)
    if (isNope) {
    	return(F)
    }
  	current_prob = CalculateExplodeProbability(player)
  	explode_probability <<- c(explode_probability, current_prob)
    return(T)
}
    
Favor <- function(player, target = "") {
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
  	isNope <- Nope(player, count = 0)
    if (isNope) {
    	return(F)
    } else {
    	print(paste(t, "'s cards:"))
  		print(Players[[t]][-1])
    	message2 = paste(t, ": Enter a card to give to ", player, sep = "")
    	card <- readline(message2)
    	if (!(card %in% Players[[t]])) {
    		print("You do not have this card.")
    		return(Favor(player, target = t))
    	} else {
    	  actions <<- c(actions, "Favor")
    	  Discard(player, "FA")
    		Players[[t]] <<- Players[[t]][-match(card, Players[[t]])]
    		Players[[player]] <<- c(Players[[player]], card)
    	}  	
    }
    current_prob = CalculateExplodeProbability(player)
  	explode_probability <<- c(explode_probability, current_prob)
  	return(F)
}

Shuffle <- function(player) {
	if (!("SH" %in% Players[[player]])) {
    	print("you do not have a Shuffle card")
    	return(F)
  	}
	  lastcardplayed <<- "SH"
  	actions <<- c(actions, "Shuffle")
  	Discard(player, "SH")
  	isNope <- Nope(player, count = 0)
    if (isNope) {
    	return(F)
    } 
    Deck <<- sample(Deck)
    print("Deck is shuffled")
    current_prob = CalculateExplodeProbability(player)
  	explode_probability <<- c(explode_probability, current_prob)
    return(F)
}

SeeTheFuture <- function(player) {
	if (!("SF" %in% Players[[player]])) {
    	print("you do not have a See the Future card")
    	return(F)
  	}
  	actions <<- c(actions, "See the Future")
  	Discard(player, "SF")
  	isNope <- Nope(player, count = 0)
    if (isNope) {
    	return(F)
    } 
    print("FOR PRIVATE VIEWING ONLY")
    print(Deck[1:3])
    current_prob = CalculateExplodeProbability(player)
  	explode_probability <<- c(explode_probability, current_prob)
    return(F)
}

Steal <- function(player, target = "", cards = "") {
  print(player)
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
  			return(Steal(player, target = "", cards = ""))
  		}
  	}
  	t = target
  	if (t == "") {
  		otherPlayers = playerNames[playerNames != player]
  		print("You may steal from the 'discard pile' or from these players: ")
  		print(otherPlayers)
  		message2 = "Who/where would you like to steal from? "
  		t <- readline(message2)
  	}
  	if (t == "discard pile") {
  		if (length(cardset) != 5) {
  			print("you need 5 unique cards to steal from the 'discard pile'")
  			return(Steal(player, target = "", cards = cardset))
  		}
  		print(DiscardPile)
  		message3 = "Enter the card you want to steal from the discard pile: "
  		cd <- readline(message3)
  		if (!(cd %in% DiscardPile)) {
  			print(paste(cd, " has not been discarded yet.", sep = ""))
  			return(Steal(player, target = t, cards = cardset))
  		}
  		actions <<- c(actions, "Steal")
  		lastcardplayed <<- cardset
  		Discard(player, cardset)
  		isNope <- Nope(player, count = 0)
    		if (isNope) {
    			return(F)
    		}
        DiscardPile <<- DiscardPile[-match(cd, DiscardPile)]
        Players[[player]] <<- c(Players[[player]], cd)
        return(F)
    }
  	if (!(t %in% otherPlayers) || Players[[t]][1] == "DEAD" || length(Players[[t]]) < 2) {
  		print("Cannot steal from this player")
  		return(Steal(player, cards = cardset))
  	}
  	actions <<- c(actions, "Steal")
  	lastcardplayed <<- cardset
  	Discard(player, cardset)
  	isNope <- Nope(player, count = 0)
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
    current_prob = CalculateExplodeProbability(player)
  	explode_probability <<- c(explode_probability, current_prob)
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
			  #result = CheckCardSet(set[-1], hand[-i])
			  return(CheckHand(set[-1], hand[-i]))
		  } 
	  }
    #return(result)
  }
}




    
    


