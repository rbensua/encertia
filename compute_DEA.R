recoger_info <- function(empresa, Nempleados, rts, secondstage, df, model = "linear",
                         directions, blacklist){
  dmu_eval <- which(df$DMU == empresa)
  datos <- make_deadata(datadea = df, inputs = 2:4, outputs = 5)
  dmu_ref_orig <- 1:nrow(df)
  # Filtro n. empleados -----
  dmu_ref <- which(df$Nemployees >= Nempleados[1] &
                     df$Nemployees <= Nempleados[2] )
 
  # Blacklist
  if(!is.null(blacklist)){
    dmu_ref <- dmu_ref_orig[-which(df$DMU %in% blacklist)]
   
  }
  
  
  if (!dmu_eval %in% dmu_ref) {
    dmu_ref <- sort(c(dmu_eval, dmu_ref))
  }
 
  
  
  # Para evitar errores numéricos, hay que re-escalar los datos ----
  reescala <- 1e4
  datos$input <- datos$input / reescala
  datos$output <- datos$output / reescala
  original_input <- datos$input[, dmu_eval]
  original_output <- datos$output[, dmu_eval]
  
  
  # Facilidad relativa de cambio (de 0 a 1)----
  dir_i1 <- directions[1]
  dir_i2 <- directions[2]
  dir_i3 <- directions[3]
  dir_o1 <- directions[4]
  
  # Modelo direccional
  auxmin <- min(c(datos$input[, dmu_eval], datos$output[, dmu_eval]))
  weight_slack_i <- auxmin / datos$input[, dmu_eval]
  weight_slack_o <- auxmin / datos$output[, dmu_eval]
  
  
  if(model == "linear"){
    
    # Cálculo de direcciones
    dir_input <- c(dir_i1, dir_i2, dir_i3) * original_input
    dir_output <- dir_o1 * original_output
   
    
    if (grepl("maxslack", secondstage)) {
      res <- model_basic(datos,
                         dmu_eval = dmu_eval,
                         dmu_ref = dmu_ref,
                         orientation = "dir",
                         rts = rts,
                         dir_input = dir_input,
                         dir_output = dir_output,
                         maxslack = (secondstage == "maxslack"),
                         weight_slack_i = weight_slack_i,
                         weight_slack_o = weight_slack_o)
    } else {
      res <- model_basic(datos,
                         dmu_eval = dmu_eval,
                         dmu_ref = dmu_ref,
                         orientation = "dir",
                         rts = rts,
                         dir_input = dir_input,
                         dir_output = dir_output,
                         maxslack = FALSE)
      proj_input <- targets(res)[[1]] + slacks(res)[[1]]
      proj_output <- targets(res)[[2]] - slacks(res)[[2]]
      nd <- ncol(datos$input)
      datos2 <- datos
      datos2$input[, dmu_eval] <- proj_input
      datos2$output[, dmu_eval] <- proj_output
      DMUaux <- model_addmin(datadea = datos2,
                             method = secondstage,
                             dmu_eval = dmu_eval,
                             dmu_ref = dmu_ref,
                             rts = rts,
                             weight_slack_i = weight_slack_i,
                             weight_slack_o = weight_slack_o)$DMU[[1]]
      res$DMU[[1]]$slack_input <- DMUaux$slack_input
      res$DMU[[1]]$slack_output <- DMUaux$slack_output
      res$DMU[[1]]$target_input <- DMUaux$target_input
      res$DMU[[1]]$target_output <- DMUaux$target_output
    }
  }else{
    # Cálculo de direcciones
    dir_input <- c(dir_i1, dir_i2, dir_i3)
    dir_output <- dir_o1
    print(secondstage)
    if (grepl("maxslack", secondstage)) {
      res <- model_qgo(datos,
                         dmu_eval = dmu_eval,
                         dmu_ref = dmu_ref,
                         orientation = "dir",
                         rts = rts,
                         d_input = dir_input,
                         d_output = dir_output,
                         maxslack = (secondstage == "maxslack"),
                         weight_slack_i = weight_slack_i,
                         weight_slack_o = weight_slack_o)
      
    } else {
      res <- model_qgo(datos,
                         dmu_eval = dmu_eval,
                         dmu_ref = dmu_ref,
                         orientation = "dir",
                         rts = rts,
                         d_ = dir_input,
                         d_ = dir_output,
                         maxslack = FALSE)
      proj_input <- targets(res)[[1]] #+ slacks(res)[[1]]
      print(proj_input)
      proj_output <- targets(res)[[2]] #- slacks(res)[[2]]
      nd <- ncol(datos$input)
      datos2 <- datos
      datos2$input[, dmu_eval] <- proj_input
      datos2$output[, dmu_eval] <- proj_output
      DMUaux <- model_addmin(datadea = datos2,
                             method = secondstage,
                             dmu_eval = dmu_eval,
                             dmu_ref = dmu_ref,
                             rts = rts,
                             weight_slack_i = weight_slack_i,
                             weight_slack_o = weight_slack_o)$DMU[[1]]
      res$DMU[[1]]$slack_input <- DMUaux$slack_input
      res$DMU[[1]]$slack_output <- DMUaux$slack_output
      res$DMU[[1]]$target_input <- DMUaux$effproj_input
      res$DMU[[1]]$target_output <- DMUaux$effproj_output
    }
    
    
    
  }
  
  # Beta
  beta <- efficiencies(res)
  
  # Targets
  if(model == "linear"){
    targ <- targets(res)
    targ_input <- targ$target_input
    targ_output <- targ$target_output
    sla <- slacks(res)
    slack_input <- sla$slack_input
    slack_output <- sla$slack_output
    proj_input <- targ_input + slack_input
    proj_output <- targ_output - slack_output
  }else{
    targ <- targets(res)
    proj_input <- targ$target_input
    proj_output <- targ$target_output
    sla <- slacks(res)
    slack_input <- sla$slack_input
    slack_output <- sla$slack_output
    targ_input <- proj_input + slack_input
    targ_output <- proj_output - slack_output
  }
  
  # Redondeo 
  targ_input <- signif(targ_input, 6)
  targ_output <- signif(targ_output, 6)
  proj_input <- signif(proj_input, 6)
  proj_output <- signif(proj_output, 6)
  original_input <- signif(original_input, 6)
  original_output <- signif(original_output, 6)
  
  targ_io <- c(targ_input, targ_output) * reescala
  proj_io <- c(proj_input, proj_output) * reescala
  original_io <- c(original_input, original_output) * reescala
  gmin <- c(targ_input, original_output) * reescala
  gmax <- c(original_input, targ_output) * reescala
  lmb <- res$DMU[[1]]$lambda
  
  blank <- c(1, 1)
  names(blank) <- c("2019", "2020")
  
  list_res <- list(beta = beta,
                   targ_io = targ_io,
                   proj_io = proj_io,
                   original_io = original_io,
                   gmin = gmin,
                   gmax = gmax,
                   lmb = lmb,
                   directions = directions,
                   mi = blank,
                   tc = blank,
                   ec = blank,
                   pech = blank,
                   sech = blank)
  
  
  return(list_res)
  
}