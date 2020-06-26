# ------------------------------------------- DIRECTORIO DEL PROYECTO ---------------------------------------------------------------------------

# Clear environment
rm(list=ls())
# Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# ------------------------------------------- LIBRERIAS Y SCRIPT ---------------------------------------------------------------------------

# Install required packages
library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)
library(gtools)

# Include algorithm functions
source("../algorithms/blind/expand-node.R")
source("../algorithms/informed/hill-climbing-search.R")
source("../algorithms/informed/random-restart-hill-climbing.R")
source("../algorithms/informed/local-beam-search.R")

# Include functions for data analysis and result plot
source("../algorithms/results-analysis/analyze-results.R")

# Include the problem
source("../problem/more-pizza-problem-complete.R")

# ------------------------------------------- ALGORITMO HILL CLIMBING ---------------------------------------------------------------------------

# Executes hill climbing search and return the results
#Metodo que recibe por parametro un fichero de texto, ese fichero de texto contiene la descripcion de una instancia del problema
execute.hill.climbing <- function(filename) {
  # Initialize problem
  problem <- initialize.problem(filename = filename)
  # Print initial state
  to.string(problem$state_initial, problem)
  # Execute hill climbing
  return(hill.climbing.search(problem, max_iterations = 100, count_print = 50))
}

# Execute hill climbing
#Invocacion del algoritmo

#--- PROBLEMA SMALL -- 
hill_climbing_1   <- execute.hill.climbing(filename = "../data/1-small.txt")
hill_climbing_2   <- execute.hill.climbing(filename = "../data/1-small.txt")
hill_climbing_3   <- execute.hill.climbing(filename = "../data/1-small.txt")
hill_climbing_4   <- execute.hill.climbing(filename = "../data/1-small.txt")
hill_climbing_5   <- execute.hill.climbing(filename = "../data/1-small.txt")
hill_climbing_6   <- execute.hill.climbing(filename = "../data/1-small.txt")
hill_climbing_7   <- execute.hill.climbing(filename = "../data/1-small.txt")
hill_climbing_8   <- execute.hill.climbing(filename = "../data/1-small.txt")
hill_climbing_9   <- execute.hill.climbing(filename = "../data/1-small.txt")
hill_climbing_10   <- execute.hill.climbing(filename = "../data/1-small.txt")

#--- PROBLEMA MEDIUM -- 
hill_climbing_11   <- execute.hill.climbing(filename = "../data/2-medium.txt")
hill_climbing_12   <- execute.hill.climbing(filename = "../data/2-medium.txt")
hill_climbing_13   <- execute.hill.climbing(filename = "../data/2-medium.txt")
hill_climbing_14   <- execute.hill.climbing(filename = "../data/2-medium.txt")
hill_climbing_15   <- execute.hill.climbing(filename = "../data/2-medium.txt")
hill_climbing_16   <- execute.hill.climbing(filename = "../data/2-medium.txt")
hill_climbing_17   <- execute.hill.climbing(filename = "../data/2-medium.txt")
hill_climbing_18   <- execute.hill.climbing(filename = "../data/2-medium.txt")
hill_climbing_19   <- execute.hill.climbing(filename = "../data/2-medium.txt")
hill_climbing_20   <- execute.hill.climbing(filename = "../data/2-medium.txt")

# Clear console
cat("\014")
graphics.off()

# Initialize a problem instance for the analysis
problemSmall <- initialize.problem(filename = "../data/1-small.txt")
problemMedium <- initialize.problem(filename = "../data/2-medium.txt")

# Analyze results: 
local.analyze.results(list(hill_climbing_1, hill_climbing_2, hill_climbing_3, hill_climbing_4, hill_climbing_5,hill_climbing_6,hill_climbing_7,hill_climbing_8,hill_climbing_9,hill_climbing_10), problemSmall)
local.analyze.results(list(hill_climbing_11, hill_climbing_12, hill_climbing_13, hill_climbing_14, hill_climbing_15,hill_climbing_16,hill_climbing_17,hill_climbing_18,hill_climbing_19,hill_climbing_20), problemMedium)

# ------------------------------------------- ALGORITMO RANDOM RESTART HILL CLIMBING ---------------------------------------------------------------------------

# Clear console
cat("\014")
graphics.off()

# Ejecuta random restart hill climbing search and devuelve los resultados
#--- PROBLEMA SMALL -- 
execute.random.restart.hill.climbing.small <- function(filename) {
  # Initialize problem
  problem <- initialize.problem(filename = filename)
  # Print initial state
  to.string(problem$state_initial, problem)
  # Execute hill climbing
  return(random.restart.hill.climbing(problem, iterations = 7,max_iterations = 1000, count_print = 50,filename = "../data/1-small.txt"))
}

#--- PROBLEMA MEDIUM -- 
execute.random.restart.hill.climbing.medium <- function(filename) {
  # Initialize problem
  problem <- initialize.problem(filename = filename)
  # Print initial state
  to.string(problem$state_initial, problem)
  # Execute hill climbing
  return(random.restart.hill.climbing(problem, iterations = 7,max_iterations = 1000, count_print = 50,filename = "../data/2-medium.txt"))
}


# Ejecuta random restart hill climbing
#Invocacion del algoritmo
#--- PROBLEMA SMALL -- 
random_restart_1    <-execute.random.restart.hill.climbing.small(filename = "../data/1-small.txt")
random_restart_2    <-execute.random.restart.hill.climbing.small(filename = "../data/1-small.txt")
random_restart_3    <-execute.random.restart.hill.climbing.small(filename = "../data/1-small.txt")
random_restart_4    <-execute.random.restart.hill.climbing.small(filename = "../data/1-small.txt")
random_restart_5    <-execute.random.restart.hill.climbing.small(filename = "../data/1-small.txt")
random_restart_6    <-execute.random.restart.hill.climbing.small(filename = "../data/1-small.txt")
random_restart_7    <-execute.random.restart.hill.climbing.small(filename = "../data/1-small.txt")
random_restart_8    <-execute.random.restart.hill.climbing.small(filename = "../data/1-small.txt")
random_restart_9    <-execute.random.restart.hill.climbing.small(filename = "../data/1-small.txt")
random_restart_10    <-execute.random.restart.hill.climbing.small(filename = "../data/1-small.txt")

#--- PROBLEMA MEDIUM -- 
random_restart_11    <-execute.random.restart.hill.climbing.medium(filename = "../data/2-medium.txt")
random_restart_12    <-execute.random.restart.hill.climbing.medium(filename = "../data/2-medium.txt")
random_restart_13    <-execute.random.restart.hill.climbing.medium(filename = "../data/2-medium.txt")
random_restart_14    <-execute.random.restart.hill.climbing.medium(filename = "../data/2-medium.txt")
random_restart_15    <-execute.random.restart.hill.climbing.medium(filename = "../data/2-medium.txt")
random_restart_16    <-execute.random.restart.hill.climbing.medium(filename = "../data/2-medium.txt")
random_restart_17    <-execute.random.restart.hill.climbing.medium(filename = "../data/2-medium.txt")
random_restart_18    <-execute.random.restart.hill.climbing.medium(filename = "../data/2-medium.txt")
random_restart_19    <-execute.random.restart.hill.climbing.medium(filename = "../data/2-medium.txt")
random_restart_20    <-execute.random.restart.hill.climbing.medium(filename = "../data/2-medium.txt")

# Clear console
cat("\014")
graphics.off()

# Initialize a problem instance for the analysis
problem_small <- initialize.problem(filename = "../data/1-small.txt")
problem_medium <- initialize.problem(filename = "../data/2-medium.txt")

# Analyze results: 
local.analyze.results(list(random_restart_1,random_restart_2,random_restart_3,random_restart_4,random_restart_5,random_restart_6,random_restart_7,random_restart_8,random_restart_9,random_restart_10),problem_small)
local.analyze.results(list(random_restart_11,random_restart_12,random_restart_13,random_restart_14,random_restart_15,random_restart_16,random_restart_17,random_restart_18,random_restart_19,random_restart_20),problem_medium)

# ------------------------------------------- ALGORITMO LOCAL BEAM SEARCH ---------------------------------------------------------------------------
# Clear console
cat("\014")
graphics.off()

# K = 3
execute.local.beam.search.K3.small <- function(filename) {
  # Initialize problem
  problem <- initialize.problem(filename = filename)
  # Print initial state
  to.string(problem$state_initial, problem)
  # Execute hill climbing
  return(local.beam.search(problem,max_iterations = 100, count_print = 50,beams = 3,filename = "../data/1-small.txt"))
}

execute.local.beam.search.K3.medium <- function(filename) {
  # Initialize problem
  problem <- initialize.problem(filename = filename)
  # Print initial state
  to.string(problem$state_initial, problem)
  # Execute hill climbing
  return(local.beam.search(problem,max_iterations = 100, count_print = 50,beams = 3,filename = "../data/2-medium.txt"))
}

# K = 5
execute.local.beam.search.K5.small <- function(filename) {
  # Initialize problem
  problem <- initialize.problem(filename = filename)
  # Print initial state
  to.string(problem$state_initial, problem)
  # Execute hill climbing
  return(local.beam.search(problem,max_iterations = 100, count_print = 50,beams = 5,filename = "../data/1-small.txt"))
}

execute.local.beam.search.K5.medium <- function(filename) {
  # Initialize problem
  problem <- initialize.problem(filename = filename)
  # Print initial state
  to.string(problem$state_initial, problem)
  # Execute hill climbing
  return(local.beam.search(problem,max_iterations = 100, count_print = 50,beams = 5,filename = "../data/2-medium.txt"))
}

# K = 10
execute.local.beam.search.K10.small <- function(filename) {
  # Initialize problem
  problem <- initialize.problem(filename = filename)
  # Print initial state
  to.string(problem$state_initial, problem)
  # Execute hill climbing
  return(local.beam.search(problem,max_iterations = 100, count_print = 50,beams = 10,filename = "../data/1-small.txt"))
}

execute.local.beam.search.K10.medium <- function(filename) {
  # Initialize problem
  problem <- initialize.problem(filename = filename)
  # Print initial state
  to.string(problem$state_initial, problem)
  # Execute hill climbing
  return(local.beam.search(problem,max_iterations = 100, count_print = 50,beams = 10,filename = "../data/2-medium.txt"))
}

# Clear console
cat("\014")
graphics.off()

# Ejecuta local beam search
#Invocacion del algoritmo
local_beam_small_k3_1 <- execute.local.beam.search.K3.small(filename = "../data/1-small.txt")
local_beam_small_k3_2 <- execute.local.beam.search.K3.small(filename = "../data/1-small.txt")
local_beam_small_k3_3 <- execute.local.beam.search.K3.small(filename = "../data/1-small.txt")
local_beam_small_k3_4 <- execute.local.beam.search.K3.small(filename = "../data/1-small.txt")
local_beam_small_k3_5 <- execute.local.beam.search.K3.small(filename = "../data/1-small.txt")
local_beam_small_k3_6 <- execute.local.beam.search.K3.small(filename = "../data/1-small.txt")
local_beam_small_k3_7 <- execute.local.beam.search.K3.small(filename = "../data/1-small.txt")
local_beam_small_k3_8 <- execute.local.beam.search.K3.small(filename = "../data/1-small.txt")
local_beam_small_k3_9 <- execute.local.beam.search.K3.small(filename = "../data/1-small.txt")
local_beam_small_k3_10 <- execute.local.beam.search.K3.small(filename = "../data/1-small.txt")

local_beam_medium_k3_1 <- execute.local.beam.search.K3.medium(filename = "../data/2-medium.txt")
local_beam_medium_k3_2 <- execute.local.beam.search.K3.medium(filename = "../data/2-medium.txt")
local_beam_medium_k3_3 <- execute.local.beam.search.K3.medium(filename = "../data/2-medium.txt")
local_beam_medium_k3_4 <- execute.local.beam.search.K3.medium(filename = "../data/2-medium.txt")
local_beam_medium_k3_5 <- execute.local.beam.search.K3.medium(filename = "../data/2-medium.txt")
local_beam_medium_k3_6 <- execute.local.beam.search.K3.medium(filename = "../data/2-medium.txt")
local_beam_medium_k3_7 <- execute.local.beam.search.K3.medium(filename = "../data/2-medium.txt")
local_beam_medium_k3_8 <- execute.local.beam.search.K3.medium(filename = "../data/2-medium.txt")
local_beam_medium_k3_9 <- execute.local.beam.search.K3.medium(filename = "../data/2-medium.txt")
local_beam_medium_k3_10 <- execute.local.beam.search.K3.medium(filename = "../data/2-medium.txt")

local_beam_small_k5_1 <- execute.local.beam.search.K5.small(filename = "../data/1-small.txt")
local_beam_small_k5_2 <- execute.local.beam.search.K5.small(filename = "../data/1-small.txt")
local_beam_small_k5_3 <- execute.local.beam.search.K5.small(filename = "../data/1-small.txt")
local_beam_small_k5_4 <- execute.local.beam.search.K5.small(filename = "../data/1-small.txt")
local_beam_small_k5_5 <- execute.local.beam.search.K5.small(filename = "../data/1-small.txt")
local_beam_small_k5_6 <- execute.local.beam.search.K5.small(filename = "../data/1-small.txt")
local_beam_small_k5_7 <- execute.local.beam.search.K5.small(filename = "../data/1-small.txt")
local_beam_small_k5_8 <- execute.local.beam.search.K5.small(filename = "../data/1-small.txt")
local_beam_small_k5_9 <- execute.local.beam.search.K5.small(filename = "../data/1-small.txt")
local_beam_small_k5_10 <- execute.local.beam.search.K5.small(filename = "../data/1-small.txt")

local_beam_medium_k5_1 <- execute.local.beam.search.K5.medium(filename = "../data/2-medium.txt")
local_beam_medium_k5_2 <- execute.local.beam.search.K5.medium(filename = "../data/2-medium.txt")
local_beam_medium_k5_3 <- execute.local.beam.search.K5.medium(filename = "../data/2-medium.txt")
local_beam_medium_k5_4 <- execute.local.beam.search.K5.medium(filename = "../data/2-medium.txt")
local_beam_medium_k5_5 <- execute.local.beam.search.K5.medium(filename = "../data/2-medium.txt")
local_beam_medium_k5_6 <- execute.local.beam.search.K5.medium(filename = "../data/2-medium.txt")
local_beam_medium_k5_7 <- execute.local.beam.search.K5.medium(filename = "../data/2-medium.txt")
local_beam_medium_k5_8 <- execute.local.beam.search.K5.medium(filename = "../data/2-medium.txt")
local_beam_medium_k5_9 <- execute.local.beam.search.K5.medium(filename = "../data/2-medium.txt")
local_beam_medium_k5_10 <- execute.local.beam.search.K5.medium(filename = "../data/2-medium.txt")



local_beam_small_k10_1 <- execute.local.beam.search.K10.small(filename = "../data/1-small.txt") 
local_beam_small_k10_2 <- execute.local.beam.search.K10.small(filename = "../data/1-small.txt") 
local_beam_small_k10_3 <- execute.local.beam.search.K10.small(filename = "../data/1-small.txt") 
local_beam_small_k10_4 <- execute.local.beam.search.K10.small(filename = "../data/1-small.txt") 
local_beam_small_k10_5 <- execute.local.beam.search.K10.small(filename = "../data/1-small.txt") 
local_beam_small_k10_6 <- execute.local.beam.search.K10.small(filename = "../data/1-small.txt") 
local_beam_small_k10_7 <- execute.local.beam.search.K10.small(filename = "../data/1-small.txt") 
local_beam_small_k10_8 <- execute.local.beam.search.K10.small(filename = "../data/1-small.txt") 
local_beam_small_k10_9 <- execute.local.beam.search.K10.small(filename = "../data/1-small.txt") 
local_beam_small_k10_10 <- execute.local.beam.search.K10.small(filename = "../data/1-small.txt") 


local_beam_medium_k10_1 <- execute.local.beam.search.K10.medium(filename = "../data/2-medium.txt")
local_beam_medium_k10_2 <- execute.local.beam.search.K10.medium(filename = "../data/2-medium.txt")
local_beam_medium_k10_3 <- execute.local.beam.search.K10.medium(filename = "../data/2-medium.txt")
local_beam_medium_k10_4 <- execute.local.beam.search.K10.medium(filename = "../data/2-medium.txt")
local_beam_medium_k10_5 <- execute.local.beam.search.K10.medium(filename = "../data/2-medium.txt")
local_beam_medium_k10_6 <- execute.local.beam.search.K10.medium(filename = "../data/2-medium.txt")
local_beam_medium_k10_7 <- execute.local.beam.search.K10.medium(filename = "../data/2-medium.txt")
local_beam_medium_k10_8 <- execute.local.beam.search.K10.medium(filename = "../data/2-medium.txt")
local_beam_medium_k10_9 <- execute.local.beam.search.K10.medium(filename = "../data/2-medium.txt")
local_beam_medium_k10_10 <- execute.local.beam.search.K10.medium(filename = "../data/2-medium.txt")


# Initialize a problem instance for the analysis
problem_small_bs <- initialize.problem(filename = "../data/1-small.txt")
problem_medium_bs <- initialize.problem(filename = "../data/2-medium.txt")

# Analyze results: 
local.analyze.results(list(local_beam_small_k3_1,local_beam_small_k3_2,local_beam_small_k3_3,local_beam_small_k3_4,local_beam_small_k3_5,local_beam_small_k3_6,local_beam_small_k3_7,local_beam_small_k3_8,local_beam_small_k3_9,local_beam_small_k3_10),problem_small_bs)
local.analyze.results(list(local_beam_medium_k3_1,local_beam_medium_k3_2,local_beam_medium_k3_3,local_beam_medium_k3_4,local_beam_medium_k3_5,local_beam_medium_k3_6,local_beam_medium_k3_7,local_beam_medium_k3_8,local_beam_medium_k3_9,local_beam_medium_k3_10),problem_medium_bs)

local.analyze.results(list(local_beam_small_k5_1,local_beam_small_k5_2,local_beam_small_k5_3,local_beam_small_k5_4,local_beam_small_k5_5,local_beam_small_k5_6,local_beam_small_k5_7,local_beam_small_k5_8,local_beam_small_k5_9,local_beam_small_k5_10),problem_small_bs)
local.analyze.results(list(local_beam_medium_k5_1,local_beam_medium_k5_2,local_beam_medium_k5_3,local_beam_medium_k5_4,local_beam_medium_k5_5,local_beam_medium_k5_6,local_beam_medium_k5_7,local_beam_medium_k5_8,local_beam_medium_k5_9,local_beam_medium_k5_10),problem_medium_bs)

local.analyze.results(list(local_beam_small_k10_1,local_beam_small_k10_2,local_beam_small_k10_3,local_beam_small_k10_4,local_beam_small_k10_5,local_beam_small_k10_6,local_beam_small_k10_7,local_beam_small_k10_8,local_beam_small_k10_9,local_beam_small_k10_10),problem_small_bs)
local.analyze.results(list(local_beam_medium_k10_1,local_beam_medium_k10_2,local_beam_medium_k10_3,local_beam_medium_k10_4,local_beam_medium_k10_5,local_beam_medium_k10_6,local_beam_medium_k10_7,local_beam_medium_k10_8,local_beam_medium_k10_9,local_beam_medium_k10_10),problem_medium_bs)



