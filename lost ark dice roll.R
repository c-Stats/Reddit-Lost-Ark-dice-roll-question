#Question url:
#https://www.reddit.com/r/statistics/comments/t4vr3j/q_optimization_and_modeling_of_a_game_mechanic/

#Structure to hold the states
states <- matrix(0, 66, 4)
colnames(states) <- c("S", "F", "i", "P")

k <- 1
for(i in 0:10){

	for(j in 0:(10-i)){

		states[k, 1] <- i
		states[k, 2] <- j

		k <- k + 1

	}

}

states[, 3] <- c(1:66)
states[, 4] <- 0.75 + 0.1 * states[, 2] - 0.1 * states[, 1]
states[, 4] <- sapply(states[, 4], function(x){max(0, min(0.75, x))})


#Transition matrix
T_mat <- matrix(0, 66, 66)
for(i in 1:66){

	from <- states[i, ]

	for(j in 1:66){

		to <- states[j, ]
		#Check if move is possible
		increment <- to[c(1,2)] - from[c(1,2)]

		#S increases by 1
		if(increment[1] == 1 & increment[2] == 0){

			T_mat[i,j] <- from[4]

		#F increases by 1
		} else if(increment[1] == 0 & increment[2] == 1){

			T_mat[i,j] <- 1 - from[4]

		}		

	}

}

#Fill the terminal states where S + F = 10
terminal_states <- which(states[, 1] + states[, 2] == 10)
for(i in terminal_states){

	T_mat[i,i] <- 1

}

#Check that all rows add up to 1
row_sums <- apply(T_mat, 1, sum)
which(row_sums != 1)

#We start at (S,F) = (0,0)
position_vector <- rep(0, 66)
position_vector[which(states[, 1] == 0 & states[, 2] == 0)] <- 1

#We perform 10 moves
for(i in 1:10){

	position_vector <- position_vector %*% T_mat

}

#Compute the expected value
print(position_vector %*% states[, 1])
