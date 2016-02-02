##----------------------------------------
##
##  Sample Genetic Algorithm
##
##  
##
##----------------------------------------

##-----Declare parameters-----
pop_size           = 100   # Number of individuals in solution population
individual_length  = 100   # How many parameters in an individual
p_mutation         = 0.25/individual_length # Probability that each parameter changes
selection_strength = 0.25  # Top Percentage of population to keep (most fit)
generations        = 1000    # Number of generations
visualize_solution = 50  # Graph best solution every X generations

# Declare the truth (optimal solution)
# Normally, we don't know the truth, but we have some problem specific way of
# determining the fitness of the individual.
# For us, the fitness will be the MSE difference of the individuals and the truth:
max_x = 2*pi
y_limits = c(-1.5, 1.5) # just a plotting parameter
truth = sin(max_x*(1:pop_size)/pop_size)


##-----Declare plot function-----
plot_example = function(individual, truth, max_x, y_limits){
  pop_size = length(individual)
  plot((1:pop_size)/max_x, truth, type="l", ylim=y_limits,lwd=2,
       ylab="y", xlab="x")
  lines((1:pop_size)/max_x, individual, col="red", lty=2)
}


##-----Declare the population functions-----
#-------------------------------------------

##-----Initialize population------
initialize_pop = function(pop_size, individual_length){
  
  population = lapply(1:pop_size, function(x) rnorm(individual_length))
  
  return(population)
}


##-----Fitness Function-----
# This is the key in Genetic Algorithms.  The fitness function is usually
# very expensive.  Here, it's not expensive at all because we know the truth.
# Usually you have to take every individual in the population and evaluate them
# This can get expensive.
#
# E.g. if each individual represents a model that is used to predict data,
#      you have to run the training/prediction for each individual for every
#      generation.
# 
get_individual_fitness = function(individual, truth){
  stopifnot(length(individual)==length(truth))
  # Return the MSE
  return(mean( (individual - truth)^2 ))
}


##-----Selection Function-----
selection = function(population, pop_fitness, selection_strength){
  # Figure out how many to save
  num_to_save = round(selection_strength*length(population))
  
  # Find indices of individuals
  individuals_to_save = order(pop_fitness)[1:num_to_save]
  
  # Save parent population
  parents = population[individuals_to_save]
  
  return(parents)
}


##-----Recombination Function-----
recombination = function(parents, pop_size){
  # Need to create N children,
  #  where N = pop_size - length(parents)
  num_children = pop_size - length(parents)
  
  # Initialize Children
  children = list()
  
  # Create Children
  for (c in 1:num_children){
    # Randomly get 2 parents
    temp_parents = sample(parents, 2)
    # Now select a random crossover point
    crossover_pt = sample(2:length(temp_parents[[1]]), 1)
    # Create child
    child = c(temp_parents[[1]][1:(crossover_pt-1)], temp_parents[[2]][crossover_pt:length(temp_parents[[2]])])
    # Save child
    children = c(children, list(child))
  }
  
  return(children)
}


##-----Mutation Function-----
mutate = function(individual, p_mutation){
  # generate random uniform vector same length as individual
  prob_vector = runif(length(individual))
  
  # Find where to mutate individual
  indices_to_mutate = prob_vector <= p_mutation
  
  # Set those values to a new value
  individual[indices_to_mutate] = rnorm(length(individual))[indices_to_mutate]
  
  return(individual)
}


##-----Start Genetic Algorithm-----
population = initialize_pop(pop_size, individual_length)

best_fitness_sequence = c()

for (g in 1:generations){
  print(paste('Starting generation',g,'out of',generations,'generations.'))
  
  # Get fitness (sapply because we want a vector out of it)
  pop_fitness = sapply(population, function(x) get_individual_fitness(x, truth))
  
  # Save best fitness
  best_fitness = min(pop_fitness)
  best_fitness_sequence = c(best_fitness_sequence, best_fitness)
  
  # Get parents
  parents = selection(population, pop_fitness, selection_strength)
  
  # Get Children
  children = recombination(parents, pop_size)
  
  # Mutate children (lapply because we want a list returned)
  children = lapply(children, function(x) mutate(x, p_mutation))
  
  # Combine parents and children
  population = c(parents, children)
  
  # Plot?
  if (g %% visualize_solution == 0){
    # find best individual
    best_individual = population[[which.max(pop_fitness)]]
    # Plot best individual
    plot_example(best_individual, truth, max_x, y_limits)
    Sys.sleep(1)
  }
}


plot(1:generations, best_fitness_sequence, type="l",
     main="fitness vs. generations", xlab="generations", ylab="fitness (MSE)")
