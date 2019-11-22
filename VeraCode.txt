# EXERCISE 8

# select the work directory
# instal the packages ggplot2 and select it 

setwd('/Users/claudiaveraarias/Documents/ND_Classes/Fall_Semester_2019/Biocomputing/R/W11_BC/Tutorial/')
install.packages("ggplot2")
library("ggplot2")


#1) Using the score-by-score information from this game summarized in 
#“UWvMSU_1-22-13.txt” generate a graph

	# read the file
UWvsMSU <- read.delim("/Users/claudiaveraarias/Documents/ND_Classes/Fall_Semester_2019/Biocomputing/R/W11_BC/Tutorial/IBC_Exercise_08-master/UWvMSU_1-22-13.txt", header = TRUE, sep = "\t", dec = ".")

	UW_MSU <- matrix(data = NA, nrow = 51, ncol = 3)
  UW_MSU[1,]=0
  
	# Loop to create a new table with the score sum

    for(i in 1:nrow(UWvsMSU)){
    if(UWvsMSU$team[i] == "UW"){
      UW_MSU[i+1,2] <- UW_MSU[i, 2] + UWvsMSU$score[i]
      UW_MSU[i+1,3] <- UW_MSU[i, 3]  
    } else {
      UW_MSU[i+1,3] <- UW_MSU[i, 3] + UWvsMSU$score[i]
      UW_MSU[i+1,2] <- UW_MSU[i, 2]     
      }
    }

	# Add a extra row in the original file "UWvsMSU" to have the same
		# length of the new table "UW_MSU
    
    UWvsMSU2 <- rbind(c(NA,NA,NA), UWvsMSU)
    UWvsMSU2[is.na(UWvsMSU2)] <- 0
    
    UWscore <- UW_MSU[,2]
    MSUscore <- UW_MSU[,3]
    Time <- UWvsMSU2[,1]
    
	# Plot the graph

    ggplot() + geom_line(aes(x=Time, y=UWscore), color="green") +
      geom_line(aes(x=Time, y=MSUscore), color="blue") +
      labs( y = "Score", x = "Time", title = "UW vs MSU")
    

#2) Write a game called “guess my number”. The computer will generate a 
# random number between 1 and 100. The user types in a number and the computer 
# replies “lower” if the random number is lower than the guess, “higher” if the 
# random number is higher, and “correct!” if the guess is correct. The player can 
# continue guessing up to 10 times.

##  Guess my numbers

# Initial parameters

x = 0
gotRight = 0
failed = 0

#Initial lambda for our random var
correct = sample(1:100, 1, replace = F) 
initial = correct

# how many guesses should we allow per number
maxGuesses = 10

while(x != Inf) {
  correct = rpois(1,correct) +1
  
  cat("I am thinking of a number between 1 and 100. What is it? (type Inf to quit)n")
  
  # solicit input from the user
  x = scan(n=1) # just one item is this vector
  
  if(x == Inf) {
    cat("The correct answer was", correct, "n")
    cat("You got", gotRight, "right and failed", failed, "times. Maximum allowed guesses was", maxGuesses, "and initial lambda was", initial, ". Goodbye.n")
    break
  }
  for (i in 1:maxGuesses) {
    if(x == correct) {
      print("Correct")
      gotRight = gotRight + 1
      break
    } else {
      if(i == maxGuesses) {
        cat("You ran out of guesses. I will pick a new random number based on the last one.n")
        failed = failed + 1
      } else {
        if(x < correct) {
          cat("Higher.n")
        } else {
          cat("Lower.n")
        }
      x = scan(n=1)
        }
    }
  }
}


