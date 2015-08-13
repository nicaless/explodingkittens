deck = c("EK", "EK", "EK", "EK", 
         "DF", "DF", "DF", "DF", "DF", "DF",
         "NO", "NO", "NO", "NO", "NO",
         "AT", "AT", "AT", "AT",
         "SK", "SK", "SK", "SK",
         "FA", "FA", "FA", "FA",
         "SH", "SH", "SH", "SH",
         "SF", "SF", "SF", "SF",
         "CC1", "CC2", "CC3", "CC4", "CC5",
         "CC1", "CC2", "CC3", "CC4", "CC5",
         "CC1", "CC2", "CC3", "CC4", "CC5",
         "CC1", "CC2", "CC3", "CC4", "CC5")

Deck = ""
Players = ""
playerNames = ""
alive = ""

actions = ""

deadcount = 0



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
    #playerList[[i]] = CreatePlayer(playernames[i], hand)
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
  Players <<- playerList
  playerNames <<- names(Players)
  alive <<- playerNames
  Players[[1]][1] <<- "TURN"
  actions <<- "Start Turn"
  Turns <<- data.frame(PlayerTurn = playerNames[1], Actions = actions, ExplodeProbability = 0)
  #gameStart = list(Deck = setupdeck, Players = playerList)
  #return(gameStart)
}

CalculateExplodeProbability <- function(player) {
	return(0)
}

EndTurn <- function(player, explode = F, dead = F, attack = F) {
  #set player number and nextplayernumber
  doubleturn = (Players[[player]][1] == "DOUBLETURN")
  playernumber = match(player, playerNames)
  if (playernumber == length(playerNames)) {
    nextplayernumber = 1
  } else {
    nextplayernumber = playernumber + 1
  }

  #if i'm not dead or I am recently exploded, append my turn info
  if (!dead || (dead & explode)) {
    #prob = CalculateExplodeProbability(player)
    #turn = cbind(PlayerTurn = player, Actions = actions, ExplodeProbability = prob)
    turn = cbind(PlayerTurn = player, Actions = actions, ExplodeProbability = 0)
    Turns <<- rbind(Turns, turn)
  }
  
  #if I am dead , ensure I stay dead.  Otherwise, wait for next turn unless i have a double turn from attack.
  if (dead) {
    Players[[player]][1] <<- "DEAD"
  } else {
    if (doubleturn) {
      Players[[player]][1] <<- "TURN"
    } else {
      Players[[player]][1] <<- "WAIT"
    }
  }
  
  ### Check if everyone is dead
  otherPlayers = playerNames[playerNames != player]
  for (i in otherPlayers) {
    if ("DEAD" %in% Players[[i]]) {
      deadcount <<- deadcount + 1
      alive <<- alive[alive != i]
    } 
  }
  if (deadcount == (length(playerNames)-1)) {
    stop(paste("Everyone exploded. ", alive, " wins!", sep = ""))
  } else {
    actions <<- "Start Turn"
  }
  
  #if I exploded and defused (dd not die), PlaceKitten
  if (explode) {
    if (dead) {
      PlaceKitten(dead = T)
    } else {
      PlaceKitten()
    }
  }
  
  #if previous turn (either my current turn or the turn before mine if i am dead) ended with an attack, 
  #check if the next player is dead. if they are, attack the next player.
  if (attack) {
    Players[[player]][1] <<- "WAIT"
    if (Players[[nextplayernumber]][1] == "DEAD") {
      EndTurn(names(Players)[nextplayernumber], dead = T, attack = T)
    } else {
      DoubleTurn(playerNames[nextplayernumber])
    } 
  } else {
    if (doubleturn) {
      Players[[nextplayernumber]][1] <<- "WAIT"
    } else {
      Players[[nextplayernumber]][1] <<- "TURN"
    }
  }
}

Discard <- function(player, cards) {
  for (i in cards) {
    if (exists("DiscardPile")) {
      Players[[player]] <<- Players[[player]][-match(i, Players[[player]])]
      DiscardPile <<- c(DiscardPile, i)
    } else {
      DiscardPile <<- cards
    }
  }
}

Draw <- function(player) {
  if(!(player %in% playerNames) || Players[[player]][1] == "WAIT") {
    stop("not your turn")
  }
  nextcard = Deck[1]
  Deck <<- Deck[-1]
  if (nextcard == "EK" & !("DF" %in% Players[[player]])) {
    actions <<- c(actions, "Draw", "Exploded")
    Discard(player, Players[[player]][2:length(Players[[player]])])
    print("you exploded")
    #PlaceKitten(dead = T)
    #PlaceKitten(player)
    EndTurn(player, explode = T, dead = T)
  } 
  if (nextcard == "EK" & ("DF" %in% Players[[player]])) {
    Discard(player, "DF")
    actions <<- c(actions, "Draw", "Defuse")
    print("you ALMOST exploded")
    #PlaceKitten(player)
    EndTurn(player, explode = T)
  }
  if (nextcard != "EK") {
    Players[[player]] <<- c(Players[[player]], nextcard)
    actions <<- c(actions, "Draw", "End Turn")
    EndTurn(player)
  }
}

PlaceKitten <- function(dead = F) {
  n = length(Deck)
  if (dead) {
    if (exists("DeadKittenPile")) {
      DeadKittenPile <<- "EK"
    } else {
      DeadKittenPile <<- c(DeadKittenPile, "EK")
    }
  }
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

Attack <- function(player) {
  if(!(player %in% playerNames) || Players[[player]][1] == "WAIT") {
    stop("not your turn")
  }
  if (!("AT" %in% Players[[player]])) {
    stop("you do not have an Attack card")
  }
  actions <<- c(actions, "Attack")
  checkNope = CheckNopes(player)
  successNope = F
  if (is.character(checkNope)) {
    successNope = Nope(checkNope, player, cards)
  } 
  if (successNope) {
    #actions <<- c(actions, paste(target, " Noped by ", player, sep = ""))
    #Discard(target, "AT")
  } else {
    Discard(player, "AT")
    EndTurn(player, attack = T)  
  }
}

DoubleTurn <- function(player) {
  Players[[player]][1] <<- "DOUBLETURN"
}

Skip <- function(player) {
  if(!(player %in% playerNames) || Players[[player]][1] == "WAIT") {
    stop("not your turn")
  }
  if (!("SK" %in% Players[[player]])) {
    stop("you do not have an Skip card")
  }
  actions <<- c(actions, "Skip")
  checkNope = CheckNopes(player)
  successNope = F
  if (is.character(checkNope)) {
    successNope = Nope(checkNope, player, "SK")
  } 
  if (successNope) {
    #actions <<- c(actions, paste(target, " Noped by ", player, sep = ""))
    #Discard(target, "SK")
  } else {
    Discard(player, "SK")
    EndTurn(player)  
  }
}

Favor <- function(player, target) {
  if(!(player %in% playerNames) || Players[[player]][1] == "WAIT") {
    stop("not your turn")
  }
  if (!("FA" %in% Players[[player]])) {
    stop("you do not have an Favor card")
  }
  otherPlayers = playerNames[playerNames != player]
  if(!(target %in% otherPlayers)) {
    stop("no favors given here")
  }
  if (Players[[target]][1] == "DEAD" || length(Players[[target]]) < 2) {
    stop("this player is dead or has no cards to steal")
  }
  actions <<- c(actions, paste("Favor from ", target, sep = ""))
  checkNope = CheckNopes(player)
  successNope = F
  if (is.character(checkNope)) {
    successNope = Nope(checkNope, player, "FA")
  } 
  if (successNope) {
    #actions <<- c(actions, paste(target, " Noped by ", player, sep = ""))
    #Discard(target, "FA")
  } else {
    message = paste(target, ": Enter a card to give to ", player, sep = "")
    card <- readline(message)
    if (!(card %in% Players[[target]])) {
      print("You do not have this card.")
      Favor(player, target)
    } 
      Discard(player, "FA")
      Players[[target]] <<- Players[[target]][-match(card, Players[[target]])]
      Players[[player]] <<- c(Players[[player]], card)
  }
}

Shuffle <- function(player) {
  if(!(player %in% playerNames) || Players[[player]][1] == "WAIT") {
    stop("not your turn")
  }
  if (!("SH" %in% Players[[player]])) {
    stop("you do not have an Shuffle card")
  }
  actions <<- c(actions, "Shuffle")
  checkNope = CheckNopes(player)
  successNope = F
  if (is.character(checkNope)) {
    successNope = Nope(checkNope, player, "SH")
  } 
  if (successNope) {
    #actions <<- c(actions, paste(target, " Noped by ", player, sep = ""))
    #Discard(target, "SH")
  } else {
    Deck <<- sample(Deck)
    Discard(player, "SH")
  }
}

SeeTheFuture <- function(player) {
  if(!(player %in% playerNames) || Players[[player]][1] == "WAIT") {
    stop("not your turn")
  }
  if (!("SF" %in% Players[[player]])) {
    stop("you do not have an See the Future card")
  }
  actions <<- c(actions, "See The Future")
  checkNope = CheckNopes(player)
  successNope = F
  if (is.character(checkNope)) {
    successNope = Nope(checkNope, player, cards)
  } 
  if (successNope) {
    #actions <<- c(actions, paste(target, " Noped by ", player, sep = ""))
    #Discard(target, "SF")
  } else {
      print("EVERYONE LOOK AWAY")
      print(Deck[1:3])
      Discard(player, "SF")
  }
}

CheckCardSet <- function(set, hand) {
	if (set[1] == "") {
		return(T)
	}
	for (i in 1:length(hand)) {
		if (set[1] == hand[i])	{
			CheckCardSet(set[-1], hand[-i])
		} else {
			return(F)
		}
	}
}

# Have not yet implemented steal from discard
Steal <- function(player, target, cards) {
  if(!(player %in% playerNames) || Players[[player]][1] == "WAIT") {
    stop("not your turn")
  }
  otherPlayers = playerNames[playerNames != player]
  if(!(target %in% c(otherPlayers, "Discard"))) {
    stop("cannot steal from this target")
  }
  if (length(cards) != 2 & length(cards) != 3 & length(cards) != 5) {
    stop("need two of a kind or three of a kind to steal")
  }
  if (length(cards) == 2) {
    if (cards[1] != cards[2]) {
      stop("these cards do not match")
    }
    if (target == "Discard") {
    	stop("can only steal from a player")
    }
	if (!CheckCardSet(cards, Players[[player]][-1])) {
    #if (!(cards[1] %in% Players[[player]]) || !(cards[2] %in% Players[[player]]) ) {
      stop("you do not have one or more of these cards")
    }
    # select a card at random to steal
    actions <<- c(actions, paste("Discarded ", cards, " to steal from ", target, sep = ""))
    checkNope = CheckNopes(player)
    successNope = F
    if (is.character(checkNope)) {
      successNope = Nope(checkNope, player, cards)
    } 
    if (successNope) {
      #actions <<- c(actions, paste(target, " Noped by ", player, sep = ""))
      #Discard(target, cards)
    } else {
      #actions <<- c(actions, paste("Steal with ", cards, sep = ""))
      cd = sample(Players[[target]][-1], size = 1)
      Discard(player, cards)
      Players[[target]] <<- Players[[target]][-match(cd, Players[[target]])]
      Players[[player]] <<- c(Players[[player]], cd)
    }
  }
  
  if (length(cards) == 3) {
    if (unique(cards) != 1) {
      stop("these cards do not match")
    }
    if (target == "Discard") {
    	stop("can only steal from a player")
    }
    if (!CheckCardSet(cards, Players[[player]][-1])) {
    #if (!(cards[1] %in% Players[[player]]) || !(cards[2] %in% Players[[player]]) || !(cards[3] %in% Players[[player]])) {
      stop("you do not have one or more of these cards")
    }
    actions <<- c(actions, paste("Discarded ", cards, " to steal from ", target, " ", sep = ""))
    checkNope = CheckNopes(player)
    successNope = F
    if (is.character(checkNope)) {
      successNope = Nope(checkNope, player, cards)
    } 
    if (successNope) {
      #actions <<- c(actions, paste(target, " Noped by ", player, sep = ""))
      #Discard(target, cards)
    } else {
      message = paste(player, ": Enter the card you want to steal from ", target, " ", sep = "")
      cd <- readline(message)
      if (!(cd %in% Players[[target]])) {
        print("They do not have this card. Tough luck.")
        actions <<- c(actions, paste("Unsuccessful steal with ", cards, sep = ""))
        Discard(player, cards)
      } else {
        #actions <<- c(actions, paste("Steal with ", cards, sep = ""))
        Discard(player, cards)
        Players[[target]] <<- Players[[target]][-match(cd, Players[[target]])]
        Players[[player]] <<- c(Players[[player]], cd)
      }
    }
  }
  if (length(cards) == 5) {
  	if (unique(cards) != 5) {
  		stop("need 5 unique cards to steal from Discard")
  	}
  	if (target != "Discard") {
  		stop("can only steal from Discard")
  	}
  	if (!CheckCardSet(cards, Players[[player]][-1])) {
      stop("you do not have one or more of these cards")
    }
    actions <<- c(actions, paste("Discarded ", cards, " to steal from Discard.", sep = ""))
    checkNope = CheckNopes(player)
    successNope = F
    if (is.character(checkNope)) {
      successNope = Nope(checkNope, player, cards)
    } 
    if (successNope) {
    
    } else {
    	message = paste(player, ": Enter the card you want to steal from the Discard: ", sep = "")
    	cd <- readline(message)
    	### make this better
    	if (!(cd %in% DiscardPile)) {
    		print(paste(cd, " has not been discarded yet.", sep = ""))
    		Steal(player, target, cards)
    	} else {
    		actions <<- c(actions, paste("Discarded ", cards, " to steal from ", target, sep = ""))
    		Discard(player, cards)
        Players[[target]] <<- Players[[target]][-match(cd, Players[[target]])]
        Players[[player]] <<- c(Players[[player]], cd)
    	}
    }
  }
}

#need way to check if "nope" is other than "yes" or "no"
#player is the player of the cards that may or may not be Nope'd
CheckNopes <- function(player) {
  otherPlayers = playerNames[playerNames != player]
  for (i in otherPlayers) {
    if ("NO" %in% Players[[i]]){
      message = paste(i, " :Do you want to play your Nope? Type 'Yes' if you do. Otherwise, type anything else. ")
      nope <- readline(message)
      if ("Yes" == nope) {
        return(i)
      } else {
        next
      }
    }
  }
  return(F)
}

#player is the player who played the Nope
#target is whom the Nope affects
#targetCard is the action card(s) being Nope'd
Nope <- function(player, target, targetCard) {
  Discard(player, "NO")
  checkNope = CheckNopes(player)
  if (is.character(checkNope)) {
    #Nope(target, player, "NO")
    #Nope(checkNope, player, targetCard)
    
    #Need to maintain the original player of the card
    Nope(checkNope, target, targetCard)
  } else {
    actions <<- c(actions, paste(target, " Noped by ", player, sep = ""))
    Discard(target, targetCard)
    return(T)
  }
}

#Test Steal, minor things to fix in 5 card steal
# Fix Nopes ?? should be good now
# Rethink Turns Data.frame???
#Need Calc Probability Function
