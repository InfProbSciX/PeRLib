#' invisible
AssignData <- function(data, parse_results, env){

  for(var in parse_results$param_vars){
    eval(parse(text = parse_results$param_vars), envir = env) # evaluate the variables block as-is
  }

  if(!is.null(data)) for(i in 1:length(data)){
    assign(names(data)[i], data[[names(data)[i]]], envir = env) # assign list element to an object with the appropriate name from the list; will be used to pass on functions to SSMet
  }

}

#' invisible
Met_MCMC <- function(N_MCMC, N_MCMC_Prop, parse_results, state){

  state_rec <- state[ceiling(nrow(state) * N_MCMC_Prop):nrow(state), ]
  state <- state[1:floor(nrow(state) * N_MCMC_Prop), ]

  output_level <- state_rec[1, parse_results$output_name]
  probability_level <- state_rec[1, "Probability"]

  j = 1; while(j <= N_MCMC){

    this_run <- state[, parse_results$input_names]

    inps <- inps_rec <- this_run[j, ]
    inps_output_rec <- state[j, parse_results$output_name]

    for(i in 1:length(parse_results$input_names)){

      assign("q", as.numeric(inps[, parse_results$input_names[i]]))

      eval(parse(text = parse_results$inputs_dens[i]))

      dens <- 1/eval(parse(text = parse_results$input_names[i]))

      assign("q", inps[1, parse_results$input_names[i]] + sd(this_run[, parse_results$input_names[i]])*runif(1, -1, 1) ) # proposal

      eval(parse(text = parse_results$inputs_dens[i]))

      dens <- dens * eval(parse(text = parse_results$input_names[i]))

      if(dens == 0 | is.nan(dens)){
        dens <- 1.1
      }

      if(runif(1) < dens){
        inps[, parse_results$input_names[i]] <- q
      }else{
        inps[, parse_results$input_names[i]] <- inps_rec[, parse_results$input_names[i]]
      }

    }

    for(var in parse_results$input_names){
      assign(var, unname(unlist(inps[, var])))
    }

    if(sum(inps != inps_rec) > 0){ ################
      eval(parse(text = parse_results$output_eval_string))
    }else{
      assign(parse_results$output_name, inps_output_rec)
    }

    inps <- eval(parse(text = paste0("cbind(inps, ", parse_results$output_name, " = ", eval(parse(text = parse_results$output_name)), ")")))

    inps$Probability <- 0

    if(eval(parse(text = parse_results$output_name)) <= output_level){
      state <- rbind(state, inps)
    }else{
      inps[parse_results$input_names] <- inps_rec[parse_results$input_names]
      inps[parse_results$output_name] <- inps_output_rec
      state <- rbind(state, inps)
    }

    j <- j + 1; if(j >= nrow(this_run)) {j <- 1}

  }

  state <- state[sort(state[[parse_results$output_name]], index.return = TRUE)$ix,]

  state$Probability <- ppoints(nrow(state)) * probability_level

  state <- rbind(state, state_rec)

  rownames(state) <- 1:nrow(state)

  return(state)
}

#' invisible
NMC <- function(N_NMC, parse_results){

  inputs <- SampleFromInputs(N_NMC, parse_results$inputs_rand, parse_results$input_names) # get list of inputs

  for(var in names(inputs)){
    assign(var, unname(unlist(inputs[var]))) # assign the items in the list of inputs to the workspace
  } # this can probably be made more efficient by appending inputs$var_name to the output eval string

  eval(parse(text = parse_results$output_eval_string)) # use mapply to generate a list of outputs vars

  output_name <- parse_results$output_name

  rm(var, N_NMC, parse_results, inputs) # remove everything but inputs and output to be returned as a list

  df <- as.data.frame(as.list(environment())[names(as.list(environment())) != "output_name"]) # df of the inputs and corresponding outputs to be returned

  df <- df[sort(df[[output_name]], index.return = TRUE)$ix,] # sort df using the output var

  df$Probability <- ppoints(length(df[, 1])) # add equally spaced probability points; this might be wrong, the normal monte carlo estimator is sum(indicator[x <= i]) but the asymtotics might take care of the additional -0.5 factor

  rownames(df) <- 1:nrow(df) # rename rows

  return(df)
}

#' ParseTemplate
#'
#' @return This function outputs an example input to the SSMet routine.
#' @examples
#' SSMet(ParseTemplate())
#' @export
ParseTemplate <- function(){

  parse_string <- "
  variables {
  N_NMC = 300; # comments
  N_MCMC = 50; // comments
  N_MCMC_Prop = 0.1;
  N_SS = 6;
  }
  output {
  z = sum;
  }
  inputs {
  y[1:2] ~ norm(1, 0.5);
  }
  "

  cat(parse_string, "\n")

  return(parse_string)
}

#' invisible
Parser <- function(parse_string){

  parse_results <- list()

  parse_string <- gsub("(//|#).*?\n", "\n", parse_string) # Delete comments by deleting everything between // or # and \n
  parse_string <- gsub("( |\n)", "", parse_string) # Delete spaces and returns

  ###############################################
  ################################## INPUT STRING

  inputs <- gsub("(^.*inputs\\{|\\})", "", parse_string) # Get contents of inputs block by replacing everything before inputs{} and everything after it
  vector_inputs <- grep(".*\\[.*\\]", unlist(strsplit(parse_string, "(\\{|\\;)")), value = TRUE) # strsplit first gets all the lines in the string and grep look for any lines with []

  if(length(vector_inputs) != 0){

    inputs_split <- strsplit(inputs, ";")[[1]] # split the inputs string into a vector
    inputs <- paste0(inputs_split[!(inputs_split %in% vector_inputs)], collapse = ";") # group together inputs have not been entered as vectors
    input_vector_names <- "" # get names of inputs that have been expressed as vectors
    vector_outputs <- "" # these are input vectors like x[1:2] that will be converted to their own variables xD1 & xD2 short for x dimension 1

    for(i in 1:length(vector_inputs)){
      n <- as.integer(gsub("(.*\\:|\\].*)", "", vector_inputs[i])) # get the upper index of the vector input by looking for it between : and ]
      vector_inputs[i] <- gsub("\\[.*\\]", "D_SUB", vector_inputs[i]) # convert the [...] to D_SUB; the _SUB will be replaced with an index
      input_vector_names[i] <- gsub("D_SUB.*", "", vector_inputs[i]) # delete everything including and after the D_SUB to get the first bit which is the vector name
      vector_outputs[i] <- paste0(input_vector_names[i], # the vector name equals; e.g. x =
                                  "=c(", # a concatenation of
                                  paste0(sapply(1:n, function(k) gsub("D_SUB.*", paste0("D", k), vector_inputs[i])), collapse = ", "), # the individual scalar components; e.g. c(xD1, xD2); this is achieved by replacing everything including and after _SUB in xD_SUB with the right index
                                  ")") # close bracket
      vector_inputs[i] <- paste0(sapply(1:n, function(index) gsub("_SUB", index, vector_inputs[i])), collapse = ";") # get the scalarized input names e.g. xD1 and xD2 and make a single input string like xD1~norm(0,1);xD2~norm(0,1) consistent with the other string
    }

    vector_inputs <- paste0(vector_inputs, collapse = ";") # collapse the vector of scalarized input strings into a single input string consistent with the non-vector part of the original input string
    inputs <- paste0(vector_inputs, ";", inputs) # add the scalarized string to the scalar string the string

  }

  inputs_rand <- gsub("\\~", "<-r", inputs) # Make random number generator
  inputs_rand <- gsub("\\(", "(n,", inputs_rand) # and add the number argument
  inputs_dens <- gsub("\\~", "<-d", inputs) # Make density generator
  inputs_dens <- gsub("\\(", "(q,", inputs_dens) # and add the quantile argument

  parse_results$inputs_rand <- strsplit(inputs_rand, ";")[[1]] # Split using the ";" to get one sample statement per input
  parse_results$inputs_dens <- strsplit(inputs_dens, ";")[[1]] # Split using the ";" to get one sample statement per input

  parse_results$input_names <- gsub("<-.*", "", parse_results$inputs_rand) # Replace everything after assignment operator by nothing to get variable names

  ###############################################
  ################################# OUTPUT STRING

  output_formula <- gsub("(^.*output\\{|\\;}.*)", "", x = parse_string) # Get output formula from the outputs block

  if(exists("input_vector_names")){

    non_vector_inputs <- gsub("D[0-9]+.*", "", parse_results$input_names) # delete any occurance of D{Numbers} to obtain the names of the non-vector variables
    non_vector_inputs <- non_vector_inputs[!(non_vector_inputs %in% input_vector_names)] # then delete any name that appears in the list of vector variables

    non_vector_input_part_of_output_formula <- ifelse(length(non_vector_inputs) == 0,
                                                      "", # make a string that looks like var = var for all the non-vector inputs. This is achieved by a simple paste0
                                                      paste0(", ", paste0(non_vector_inputs, " = ", non_vector_inputs, collapse = ", ")))

    output_formula <- paste0(output_formula, "(",
                             paste0(vector_outputs, collapse = ", "),
                             non_vector_input_part_of_output_formula,
                             ")") # concatenate the different strings together to make the output function

    output_func <- gsub("^.*?=", "", output_formula) # delete the var name and the assignment operator to get the formula
    parse_results$output_name <- gsub("=.*", "", output_formula) # delete everything but the var name

    parse_results$output_eval_string <- paste0(parse_results$output_name,
                                               " <- mapply(FUN = function(",
                                               paste0(parse_results$input_names, collapse = ", "),
                                               ") ", output_func, ", ",
                                               paste0(parse_results$input_names, " = ", parse_results$input_names, collapse = ", "),
                                               ")") # this combines the ouput function with mapply and c so that at each evaluation, z gets the correct inputs

  }else{
    output_func <- gsub("(^.*?=|\\(.*$)", "", output_formula)
    parse_results$output_eval_string <- toString(paste0(parse_results$input_names, " = ", parse_results$input_names)) # make input list as a part of the mapply formula, e.g. x = x, y = y
    parse_results$output_name <- gsub("=.*", "", output_formula)
    parse_results$output_eval_string <- paste0(parse_results$output_name, " <- mapply(FUN = ", output_func, ", ", parse_results$output_eval_string, ")")
  }

  ###############################################
  ############################## VARIABLE STRINGS

  parse_results$param_vars <- strsplit(x = gsub("(^.*variables\\{|\\;\\}output.*)", "", parse_string), ";")[[1]] # Get the variables block

  return(parse_results)

}

#' Simulate an ECDF using Subset Simulation
#'
#' @param parse_string A string containing the simulation schema. For an example, use \code{ParseTemplate()}
#' @param data (optional) A named list of objects that's passed onto the SSMet function.
#' @return A data frame with the output function, the corresponding simulated inputs and stage descriptions ("NMC" or "MetMCMC_ChainX_StepX").
#' @examples
#' SSMet(ParseTemplate())
#' @export
SSMet <- function(parse_string, data = NULL) {

  parse_results <- Parser(parse_string) # Parse string

  AssignData(data, parse_results, environment()) # Assign the data list and any extra arguments passed onto SSMet

  print(parse_results)

  state <- NMC(N_NMC, parse_results)

  for(subset_number in 1:N_SS){
    state <- Met_MCMC(N_MCMC, N_MCMC_Prop, parse_results, state)
  }

  return(state)

}

#' invisible
SampleFromInputs <- function(n, inputs_rand, input_names){

  for(i in inputs_rand)
    eval(parse(text = i), envir = environment()) # sample each input randomly within the local function environment

  return(as.list(environment())[input_names]) # the random simulations are assigned to input variable names

}
