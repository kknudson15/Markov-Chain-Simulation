Part 3: Code

P = matrix(c(0.7, 0.3, 0.2, 0.2, 0.5, 0.6, 0.1, 0.2, 0.2), 3)
print(P)
x = c("S", "C", "R") ## The weather state space
n = 1000000 ## We plan to generate 10000 states one for each of the 10000 consecutive days.
states = character(n+100) ## Intentionally generate 100 more and the first 100 (called burn-in's) will be abandoned in order to generate a quality sequence.
states[1] = "C"  ## Set the initial state to any of the states, say "C" here. If the steady-state distribution is available, we can generate the initial state according to this distribution.
for (i in 2:(n+100)){
  if (states[i-1] == "S") {cond.prob = P[1,]}
  else if (states[i-1] == "C") {cond.prob = P[2,]}
  else {cond.prob = P[3,]}
  states[i]=sample(x, 1, prob = cond.prob )
}
print(states[1:100])
states = states[-(1:100)]   ## Abandon the bur-in's
head(states)
tail(states)
states[1:200]

#part a
collapsed <- paste(states, collapse="") 
length(gregexpr("S", collapsed)[[1]])
length(gregexpr("SCC", collapsed)[[1]])
prob =(length(gregexpr("SCC", collapsed)[[1]])) / length(gregexpr("S", collapsed)[[1]])
prob

#part b
collapsed <- paste(states, collapse="")
length(gregexpr("R", collapsed)[[1]])
length(gregexpr("RRS", collapsed)[[1]])
length(gregexpr("RSS", collapsed)[[1]])
length(gregexpr("RCS", collapsed)[[1]])
prob2 =((length(gregexpr("RRS", collapsed)[[1]])) + length(gregexpr("RSS", collapsed)[[1]]) + length(gregexpr("RCS", collapsed)[[1]]) ) / length(gregexpr("R", collapsed)[[1]])
prob2
