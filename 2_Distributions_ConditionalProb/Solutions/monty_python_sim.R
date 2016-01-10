# Simulate Monty Hall Problem

doors = 3
goats = 2
cars = doors - goats

n_sims = 10000

switch_outcomes = c()
stay_outcomes = c()
smart_sample = function(x, ...) x[sample.int(length(x), ...)]

create_outcomes = function(n_sims){
  for (i in 1:n_sims){
    prizes = sample(c(rep('car',cars),rep('goat',goats)),doors)
    first_door = sample(1:doors,1)
    
    revealed_goat = smart_sample(setdiff(which(prizes=='goat'),first_door),size=1)
    
    door_if_switched = smart_sample(setdiff(1:doors,c(first_door,revealed_goat)),size=1)
    switch_outcomes = c(switch_outcomes,prizes[door_if_switched]=='car')
    stay_outcomes = c(stay_outcomes, prizes[first_door]=='car')
    
  }
  return(data.frame('switch'=switch_outcomes,
                    'stay'=stay_outcomes))
}

monty_frame = create_outcomes(n_sims)

