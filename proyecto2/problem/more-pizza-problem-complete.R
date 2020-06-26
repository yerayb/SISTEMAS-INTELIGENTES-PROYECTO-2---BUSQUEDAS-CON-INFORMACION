# This function must return a list with the information needed to solve the problem.
# (Depending on the problem, it should receive or not parameters)
#Parametro: fichero de texto que define la instancia del problema
#Fichero de texto: 2 filas.
#                  -Primera fila: Porciones de pizza necesarios / Cuantos tipos de pizza diferentes existen
#                   - Segunda fila: Vector con tamaños de pizzas
initialize.problem <- function(filename = "../data/1-small.txt") {
  problem <- list() # Default value is an empty list.

  # Read data from the file
  input_data                <- readLines(filename, n = 2, warn = FALSE) #lee lineas
  
  # Needed slices
  problem$needed_slices      <- as.numeric(strsplit(x = input_data[1], split =" ")[[1]][1]) #asigna valor al numero de porciones necesarios
  # Number of different types of pizza
  problem$different_pizzas  <- as.numeric(strsplit(x = input_data[1], split =" ")[[1]][2]) #asigna valor al numero de pizzas que hacen falta
  # List with the number of slices of each pizza
  problem$pizzas            <- as.numeric(strsplit(x = input_data[2], split =" ")[[1]]) #Vector de pizzas
  
  # This attributes are compulsory
  problem$name             <- paste0("More pizza - slices=", problem$needed_slices, 
                                                ", pizzas=", problem$different_pizzas)
  # Initial state is a random vector of 0 and 1. 
  # Each position corresponds to one pizza. It indicates whether it is purchased (1) or not (0).
  problem$state_initial    <- as.numeric(runif(length(problem$pizzas)) > 0.5) 
  
  #Estado : Secuencia de 0 y 1.El tamaño es el numero de pizzas en el vector de pizzas. Indica que pizzas se compran y cuales no. 1-> Se compra pizza. 
  #Calcular numero total de porcionas: Multiplicar 2 vector y sumar posiciones.

  # In this problem final state is unknown
  problem$state_final      <- NULL
  # Action is: Changing the "purchase" value of a pizza
  problem$actions_possible <- data.frame(action = c(1:length(problem$pizzas)), stringsAsFactors = FALSE) #Accion: Cambiar 0 por 1 o 1 por 0. Hay tantas acciones como elementos hay en el array de pizzas.
  
  return(problem)
}

# Analyzes if an action can be applied in the received state.
#Devuelve siempre TRUE,
is.applicable <- function (state, action, problem) {
  return(TRUE)
}

# Returns the state resulting on applying the action over the state
# La acccion representa un indice dentro del vector de pizzas, invierte el valor. Negas el valor 0->1 y 1->0
effect <- function (state, action) {
  result <- state
  
  # Change the value (1 to 0 | 0 to 1) of the position defined by the action
  result[action] <- !result[action]
  
  return(result)
}

# Analyzes if a state is final or not
#Devuelve Diferencia entre numero de porciones que he pedido y los que debia pedir. Si es igual a 0 devuelve TRUE-
is.final.state <- function (state, final_state, problem) {
  return(get.evaluation(state, problem) == 0)
}

# Transforms a state into a string
to.string = function (state, problem) {
  if (any(state > 0)) {
    print_state <- state * problem$pizzas
    print(paste0("Best node=", get.cost(state = state, problem = problem), " / needed=", problem$needed_slices), quote = FALSE)
    print(print_state[which(print_state > 1)])
  } else {
    print("NO pizza has been ordered", quote = FALSE)
  }
}

# Returns the cost of applying an action over a state
get.cost <- function (action, state, problem) {
  return(get.ordered.pizzas(state = state, problem = problem))
}

# Heuristic function used by Informed Search Algorithms
#Devuelve valor absoluto de la diferencia de las pizzas que necesito y las pizzas que he comprado.
get.evaluation <- function(state, problem) {
  return(abs(problem$needed_slices - get.ordered.pizzas(state, problem)))
}

# Calculate the total number of pizza slices ordered.
#Calcula cuantas porciones he pedido, multiplica estado actual * vector de las pizzas con diferentes tamaños u Hace sumatorio
get.ordered.pizzas <- function(state, problem) {
  # The vector of ordered pizzas (state) is multiplied by the vector containing
  # the slices of each pizza; and finally the sum of all the slices is calculated.
  return(sum(state * problem$pizzas))
}