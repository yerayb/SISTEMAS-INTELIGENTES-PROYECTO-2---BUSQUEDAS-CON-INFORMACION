random.restart.hill.climbing = function(problem, iterations, max_iterations = 1000, 
                                        count_print = 100, 
                                        trace = FALSE, filename) {
    # Get Start time
  start_time       <- Sys.time()
  
  limit <- 0 #Contador del bucle de ejecuciones del algoritmo Hill Climbing Search
  iteraciones <- 0 #Variable iteraciones a la hora de analizar los resultados
  
  state_initial    <- problem$state_initial #Creamos un estado inical aleatorio
  actions_possible <- problem$actions_possible #Acciones posibles
  
  #Inicializacion de la variable mejor_resultado: Encargada de almacenar el mejor resultado de las distintas ejeciones del algoritmo Hill Climbing Search
  mejor_resultado <- list(parent = c(),
                          state = state_initial,
                          actions = c(),
                          depth = 1,
                          cost = get.cost(state = state_initial, problem = problem),
                          evaluation = get.evaluation(state_initial, problem))
 
  #Analisis de los resultados
  report <- data.frame(iteration = numeric(),
                       nodes_frontier = numeric(),
                       depth_of_expanded = numeric(),
                       nodes_added_frontier = numeric())
  
  
  #Tantas ejecuciones de Hill Climbing Search como iteraciones se pasen por parametro
  while (limit <= iterations) {
    
    report <- rbind(report, data.frame(iteration = iteraciones,
                                       nodes_frontier = 1,
                                       depth_of_expanded = mejor_resultado$depth,
                                       nodes_added_frontier = 1))
    
    problem <- initialize.problem(filename = filename) #Inicializar el estado inicial del problema creando un estado nuevo
    
    #Invocacion de la funcion Hill Climbing Search
    resultado <- hill.climbing.search(problem,
                                               max_iterations = max_iterations,
                                               count_print = count_print, 
                                               trace = trace 
                                               )
    
    #Si el resultado es mejor que mejor_resultado se actualiza este valor (en la primera iteracion siempre se actualiza)
    #Si no es mejor, mejor_resultado no se modifica
    if(resultado$state_final$evaluation <= mejor_resultado$evaluation){
      mejor_resultado <- resultado$state_final
    }
  
    #Si mejor_resultado es estado final (usando is.final.state()
   if((is.final.state(state = mejor_resultado$state, final_state = resultado$state_final, problem = problem))){
     break #Se rompe el bucle principal con un break
   }
    else{
      limit <- limit + 1 #Si no es estado final se continua con el bucle
    }
    
    
  }
  
 
  
  # Get runtime
  end_time <- Sys.time()
  
  #Cuando se termina la secuencia iterativa se retorna como resultado mejor_resultado.
  result <- list()
  result$name        <- paste0("Random Restart Hill Climbing - It:")
  result$runtime     <- end_time - start_time
  result$state_final <- mejor_resultado
  result$report      <- report
  

  return(result)
}