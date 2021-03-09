get_r2 = function(model, y) {
  r2 = 1 - (sum(model@fit$residuals^2) / sum((y - mean(y))^2))
  
  n = length(y)
  
  k = model@fit$robust.matcoef %>% as.data.frame() %>%
    rownames_to_column() %>%
    as_tibble %>%
    filter(str_detect(rowname, 'mu|ar1|mxreg')) %>%
    pull(rowname) %>%
    length()
  
  r2a = 1 - ((1 - r2) * (n - 1) / (n - k))
  
  return(tribble(
    ~r2, ~r2a,
    r2, r2a
  ))
}

## Estimate model function

# Different model specifications
vm = list('sGARCH', 'eGARCH', 'gjrGARCH', 'sGARCH', 'eGARCH', 'gjrGARCH', 'sGARCH', 'eGARCH', 'gjrGARCH')
dm = list('norm', 'norm', 'norm', 'std', 'std', 'std', 'sstd', 'sstd', 'sstd')

estimate_garch <- function(var_model = vm, dist_model = dm,
                           ext_regressors, dep_var = spr_ptax,
                           garch_order = c(1, 1), arma_order = c(0, 0)) {
  models <- map2(var_model,
                 dist_model, ~ {
                   a1 <- ugarchspec(variance.model = list(model = .x,
                                                          garchOrder = garch_order),
                                    mean.model = list(armaOrder = arma_order,
                                                      external.regressors = ext_regressors),
                                    distribution.model = .y)
                   
                   a2 <- ugarchfit(spec = a1, data = dep_var,
                                   solver.control = list(trace = 1))
                 })
  
  names(models) <- str_c(var_model, dist_model)
  
  return(models)
}

### Goodness-of-fit statistics:

# Load auxiliary functions copied from `rugarch` package
rugarch_boxtest <- function(stdresid, p=1, df = 0)
{
  if(any(!is.finite(stdresid))) stdresid[!is.finite(stdresid)]=0
  # p=1 normal case, p=2 squared std. residuals
  # Q-Statistics on Standardized Residuals
  #H0 : No serial correlation ==> Accept H0 when prob. is High [Q < Chisq(lag)]
  box10 = WeightedPortTest::Weighted.Box.test(stdresid, lag = 1, type = "Ljung-Box", fitdf = 0, if(p==2) sqrd.res = TRUE else sqrd.res = FALSE)
  box15 = WeightedPortTest::Weighted.Box.test(stdresid, lag = max(2, 2*df+df-1), type = "Ljung-Box", fitdf = df, if(p==2) sqrd.res = TRUE else sqrd.res = FALSE)
  box20 = WeightedPortTest::Weighted.Box.test(stdresid, lag = max(5, 4*df+df-1), type = "Ljung-Box", fitdf = df, if(p==2) sqrd.res = TRUE else sqrd.res = FALSE)
  LBSR<-matrix(NA,ncol=2,nrow=3)
  LBSR[1:3,1] = c(box10$statistic[[1]],box15$statistic[[1]],box20$statistic[[1]])
  LBSR[1:3,2] = c(box10$p.value[[1]],box15$p.value[[1]],box20$p.value[[1]])
  rownames(LBSR) = c(paste("Lag[1]",sep=""), paste("Lag[2*(p+q)+(p+q)-1][",max(2, 2*df+df-1),"]",sep=""), paste("Lag[4*(p+q)+(p+q)-1][",max(5, 4*df+df-1),"]",sep=""))
  colnames(LBSR) = c("statistic","p-value")
  return(LBSR)
}

rugarch_archlmtest <- function (x, sigma, lags, fitdf = 2, demean = FALSE)
{
  if(any(!is.finite(x))) x[!is.finite(x)] = 0
  x = as.vector(x)
  if(demean) x = scale(x, center = TRUE, scale = FALSE)
  result = WeightedPortTest::Weighted.LM.test(x, sigma^2, lag = lags, type = c("correlation", "partial")[1], fitdf = fitdf, weighted=TRUE)
  return(result)
}

rugarch_archlm <- function(object) {
  modelinc = object@model$modelinc
  gdf = sum(modelinc[8:9])
  L2 = rugarch_archlmtest(residuals(object), sigma(object), lags  = gdf+1, fitdf=gdf)
  L5 = rugarch_archlmtest(residuals(object), sigma(object), lags  = gdf+3, fitdf=gdf)
  L10 = rugarch_archlmtest(residuals(object), sigma(object), lags = gdf+5, fitdf=gdf)
  alm = matrix(0, ncol = 4, nrow = 3)
  alm[1,1:4] = as.numeric(c(L2$statistic, L2$parameter, L2$p.value))
  alm[2,1:4] = as.numeric(c(L5$statistic, L5$parameter, L5$p.value))
  alm[3,1:4] = as.numeric(c(L10$statistic, L10$parameter, L10$p.value))
  colnames(alm) = c("Statistic", "Shape", "Scale", "P-Value")
  rownames(alm) = c(paste("ARCH Lag[",gdf+1,"]",sep=""), paste("ARCH Lag[",gdf+3,"]",sep=""), paste("ARCH Lag[",gdf+5,"]",sep=""))
  
  return(alm)
}

## Extract table functions
extract_table <- function(models, ext_regressors, dep_var = garch_fut) {
  ext_reg_names = colnames(ext_regressors)
  dict_tb <- tibble(key = str_c('mxreg', 1:length(ext_reg_names)),
                    value = ext_reg_names)
  dict_garch <- {hashmap::hashmap(dict_tb$key, dict_tb$value)}
  
  table <- map(1:length(models), ~ {
    a1 <- models[[.x]]@fit$robust.matcoef %>%
      as_tibble(rownames = 'rowname') %>%
      set_names(c('rowname', 'estimate', 'sd', 't', 'pr')) %>%
      mutate(rowname = case_when(rowname %in% dict_tb$key ~ dict_garch[[rowname]], 
                                 rowname %!in% dict_tb$key ~ rowname))
    
    mr2 <- get_r2(models[[.x]], dep_var)
    r2 <- pull(mr2, r2)
    r2a <- pull(mr2, r2a)
    
    lik = models[[.x]]@fit$LLH
    n = length(ext_regressors[,1])
    aic = infocriteria(models[[.x]])[1]
    bic = infocriteria(models[[.x]])[2]
    gof = c(r2, r2a, aic, bic, lik, n)
    gof.names = c('R2', 'Adjusted R2', 'AIC', 'BIC', 'Log Likelihood', 'Num. obs.')
    decimal.places <- c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE)
    tr <- texreg::createTexreg(
      coef.names = a1$rowname, 
      coef = a1$estimate, 
      se = a1$sd, 
      pvalues = a1$pr, 
      gof.names = gof.names, 
      gof = gof, 
      gof.decimal = decimal.places)
    
    return(tr)
  })
  
  return(table)
}