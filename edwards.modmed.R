############################################
# Edwards & Lambert Method
############################################
# bootstrap function
bootstrap <- function(formula.m,formula.y,data) {
  no.terms <- length(attributes(terms(formula.m))$term.labels)
  coefs <- as.data.frame(t(sapply(1:1000,function(i) {
    temp.data <- data[sample(1:nrow(data),(10*no.terms*(no.terms+1)/2),replace=T),]
    lm1 <- lm(formula.m,temp.data)
    lm2 <- lm(formula.y,temp.data)
    c(lm1$coefficients,0,lm2$coefficients)
  })))
  names(coefs) <- c('Intercept.1',paste(attributes(terms(formula.m))$term.labels,"1",sep="."),'gap','Intercept.2',paste(attributes(terms(formula.y))$term.labels,"2",sep="."))
  return(coefs)
}

# basically do the edwards spreadsheet automatically in R. Working on it.
edwards.modmed <- function(coefs, lmm, lmy, model = c("x","xz","m","mz","y","z"),zlevels=c(-1,1),test='first',p=.05) 
{
  compute.effects <- function(lmm,lmy,model,zlevels,test=test)
  {
    output <- list(
      Low.Z  = list(
        First.Stage = lmm$coefficients[model[1]] + lmm$coefficients[model[2]]*zlevels[1],
        Second.Stage = lmy$coefficients[model[3]] + ifelse(test == "first",0,lmy$coefficients[model[4]]*zlevels[1]),
        Direct = lmy$coefficients[model[1]] + ifelse(test == "first",0,lmy$coefficients[model[2]]*zlevels[1])
      ),
      High.Z  = list(
        First.Stage = lmm$coefficients[model[1]] + lmm$coefficients[model[2]]*zlevels[2],
        Second.Stage = lmy$coefficients[model[3]] + ifelse(test == "first",0,lmy$coefficients[model[4]]*zlevels[2]),
        Direct = lmy$coefficients[model[1]] + ifelse(test == "first",0,lmy$coefficients[model[2]]*zlevels[2])
      )
    )
    output[['Low.Z']][['Indirect']] <- output[['Low.Z']][['First.Stage']]*output[['Low.Z']][['Second.Stage']]
    output[['Low.Z']][['Total']] <- output[['Low.Z']][['Direct']] + output[['Low.Z']][['Indirect']]
    output[['High.Z']][['Indirect']] <- output[['High.Z']][['First.Stage']]*output[['High.Z']][['Second.Stage']]
    output[['High.Z']][['Total']] <- output[['High.Z']][['Direct']] + output[['High.Z']][['Indirect']]
    output[['Difference']] <- lapply(1:5,function(i) output[['High.Z']][[i]] - output[['Low.Z']][[i]])
    names(output[['Difference']]) <- names(output[['High.Z']])
    return(unlist(output))
  }
  
  effects <- compute.effects(lmm,lmy,model,zlevels,test=test)
  effect.dist <- sapply(1:nrow(coefs),function(i) {
    temp.lmm <- list()
    temp.lmy <- list()
    temp.lmm$coefficients <- coefs[i,paste(model[c(1,2,6)],'.1',sep='')]
    if (test == "first") {
      include <- c(1,3)
    }else{
      include <- c(1:4,6)
    }
    temp.lmy$coefficients <- coefs[i,paste(model[include],'.2',sep='')]
    names(temp.lmm$coefficients) <- model[c(1,2,6)]
    names(temp.lmy$coefficients) <- model[include]
    compute.effects(temp.lmm,temp.lmy,model,zlevels,test=test)
  })
  
  ci <- t(sapply(1:15,function(i) quantile(t(effect.dist)[,i],probs=c(p,1-p))))
  temp <-data.frame(effect = c('First','Second','Direct','Indirect','Total'),low=effects[1:5],high=effects[6:10],difference=effects[11:15],low.lb=ci[1:5,1],low.ub=ci[1:5,2],high.lb=ci[6:10,1],high.ub=ci[6:10,2],diff.lb=ci[11:15,1],diff.ub=ci[11:15,2]) 
  row.names(temp) <- NULL
  
  return(temp)
}