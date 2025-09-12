# Artificial data draws
artificial_data_poi <- function(N,n){
  resultado <- list()

  ## Population data
  xx        = cbind(rep(1,N),runif(N,0,2))
  beta_sim  = c(2,1.5)
  loc_sim   = xx%*%beta_sim
  numcov    = ncol(xx)
  dados_pop = NULL
  for(i in 1:N){
    dados_pop[i] = rnorm(1, loc_sim[i,1], 1)
    #dados_pop[i] = rnorm(1, loc_sim[i,1], (1+0.2*xx[i,1]))
    #dados_pop[i] = loc_sim[i,1] + rt(1, 3)
    #dados_pop[i] = loc_sim[i,1] + rsn(1,xi=-0.7740617,omega=1,alpha=4)
  }

  ## Sample data
  z_aux <- rnorm(N,mean=1+dados_pop, sd=.5)
  #z_aux <- rnorm(N,mean=1+1/3*dados_pop+xx[,2], sd=.5)
  #z_aux <- rnorm(N,mean=1+1/10*dados_pop+xx[,2], sd=.5)
  #z_aux <- rnorm(N,mean=1+3.5, sd=.5)
  var_aux <- 1/(1+exp(2.5-0.5*z_aux))
  probabilidades <- n*var_aux/sum(var_aux)
  w_pop <- 1/probabilidades
  aux_aux<-NULL
  for(i in 1:N){
    aux_aux[i] <- rbern(1,probabilidades[i])
  }
  dados <- dados_pop[which(aux_aux==1)]
  x     <- xx[which(aux_aux==1),]
  w_aux <- w_pop[which(aux_aux==1)]
  w     <- length(dados)*(w_aux/sum(w_aux))

  ## Final object
  resultado[['sample_data']] <- dados
  resultado[['x_matrix']]    <- x
  resultado[['weights']]     <- w
  resultado[['pop_data']]    <- dados_pop
  resultado[['x_pop']]       <- xx
  resultado[['weights_un']]  <- w_aux

  return(resultado)
}

# Artificial data draws
artificial_data_est <- function(N,n,n_est){
  resultado <- list()

  ## Population data
  xx        = cbind(rep(1,N),runif(N,0,2))
  beta_sim  = c(2,1.5)
  loc_sim   = xx%*%beta_sim
  numcov    = ncol(xx)
  dados_pop = NULL
  for(i in 1:N){
    dados_pop[i] = rnorm(1, loc_sim[i,1], 1)
    #dados_pop[i] = rnorm(1, loc_sim[i,1], (1+0.2*xx[i,1]))
    #dados_pop[i] = loc_sim[i,1] + rt(1, 3)
    #dados_pop[i] = loc_sim[i,1] + rsn(1,xi=-0.7740617,omega=1,alpha=4)
  }

  ## Sample data
  z_aux <- rnorm(N,mean=1+dados_pop, sd=.5)
  var_aux <- 1/(1+exp(2.5-0.5*z_aux))

  df     <- data.frame(cbind(dados_pop,xx,var_aux))
  colnames(df) <- c('y','x0','x1','z')
  df_ord <- df %>% arrange(z) %>%
    mutate(
      prop = z / sum(z),
      cprop = cumsum(prop),
      bins = cut(cprop, breaks = n_est)
    )
  df_ord$estrato <- factor(df_ord$bins,
                           labels = c(1:n_est))

  n_k <- n/n_est
  s_aux <- sampling::strata(df_ord,c("estrato"),size=rep(n_k,n_est), method="srswor")
  s <- getdata(df_ord,s_aux)
  s$w <- 1/s$Prob

  ## Final object
  resultado[['sample_data']] <- s$y
  resultado[['x_matrix']]    <- cbind(s$x0,s$x1)
  resultado[['weights']]     <- length(s$y)*(s$w/sum(s$w))
  resultado[['pop_data']]    <- df$y
  resultado[['x_pop']]       <- cbind(df$x0,df$x1)
  resultado[['weights_un']]  <- s$w

  return(resultado)
}

# Artificial data draws
artificial_data_sys <- function(N,n){
  resultado <- list()

  ## Population data
  xx        = cbind(rep(1,N),runif(N,0,2))
  beta_sim  = c(2,1.5)
  loc_sim   = xx%*%beta_sim
  numcov    = ncol(xx)
  dados_pop = NULL
  for(i in 1:N){
    dados_pop[i] = rnorm(1, loc_sim[i,1], 1)
    #dados_pop[i] = rnorm(1, loc_sim[i,1], (1+0.2*xx[i,1]))
    #dados_pop[i] = loc_sim[i,1] + rt(1, 3)
    #dados_pop[i] = loc_sim[i,1] + rsn(1,xi=-0.7740617,omega=1,alpha=4)
  }

  ## Sample data
  z_aux <- rnorm(N,mean=1+dados_pop, sd=.5)
  var_aux <- 1/(1+exp(2.5-0.5*z_aux))

  df           <- data.frame(cbind(dados_pop,xx,var_aux,rep(1,N)))
  colnames(df) <- c('y','x0','x1','z','estrato')
  s_aux <- sampling::strata(df,"estrato",size=n, method="systematic",pik=df$z)
  s     <- getdata(df,s_aux)
  s$w   <- 1/s$Prob

  ## Final object
  resultado[['sample_data']] <- s$y
  resultado[['x_matrix']]    <- cbind(s$x0,s$x1)
  resultado[['weights']]     <- length(s$y)*(s$w/sum(s$w))
  resultado[['pop_data']]    <- df$y
  resultado[['x_pop']]       <- cbind(df$x0,df$x1)
  resultado[['weights_un']]  <- s$w

  return(resultado)
}
