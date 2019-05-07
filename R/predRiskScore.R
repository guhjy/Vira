#' @title Score trajectory cluster model 
#' @description 
#' Pedict risk scores (decsion values) and  trajectory clusters for a patient (goups of patients)
#' with  variables: 
#'  
#' @param age patient age 
#' @param sex patient gender
#' @param time follow up time from surgery  
#' @param acei ace inhibitor(drug). ACE inhibitors treat a variety of conditions, 
#' such as high blood pressure, scleroderma and migraines. 
#' @param dm  preoperative diabetes
#' @param creat preoperative serum creatinine
#' @param hc preoperative high cholesterol
#' @param prenyha preoperative New York Heart Association (NYHA): 1 = I/II; 3 = III/IV 
#' @param bsa preoperative body surface area.

#' @importFrom plyr dlply 
#' @importFrom plyr ddply 
#
#' @export
predRiskScore <- function(input = NULL){
  #input can either be, NULL, data frame, or csv file 	
 
# for NULL input, we restun predictions on the training set  
  if(is.null(input)){
    
    return(model$data)
    
  } else {
     newdata <- if(is.character(input) && file.exists(input)){
                    read.csv(input)
                } else {
                   as.data.frame(input)
                }
  
  id <- model$groups
  dat <- model$data
  
  ## check if patient id or grouping variable has been provided 
  if(!id %in% names(newdata)) stop("please provide patient id/grouping variable")
  
  nme <- c("age", "sex", "time","acei", "dm", "creat", "hc", "prenyha", "bsa")
  n <- nme%in%names(newdata)
  if(!all(n)) stop("some variables in the test data are not in the model!")
  
  n <- sum(sapply(newdata[, nme], function(xx) sum(is.na(xx)))) 
  
  if(n != 0) stop("no missing values in test data")
  
  newdata$age <- as.numeric(newdata$age)
  newdata$time <- as.numeric(newdata$time)
  newdata$bsa <- as.numeric(newdata$bsa)
  newdata$creat <- as.numeric(newdata$creat)
  newdata$sex <- factor(newdata$sex, levels = levels(dat$sex))
  newdata$acei <- factor(newdata$acei, levels = levels(dat$acei))
  newdata$dm <-  factor(newdata$dm, levels = levels(dat$dm))
  newdata$hc <- factor(newdata$hc, levels = levels(dat$hc))
  newdata$prenyha <- factor(newdata$prenyha, levels = levels(dat$prenyha))
  
#  if(!all(colclass%in%model$col.class[nme])) stop("training and test data differ in some variable types")
 
## get data for patient(s) in the training data and replace new features in newdata 
  dd <- dat[dat$id%in%newdata$id, ] 

  ##  for each patient in the newdata, follow up time should be more than follow up time used for training   
  tme <- unlist(plyr::dlply(dd, .variables = id, .fun = function(xx) {
       max(xx$time)  <= max(newdata$time[newdata$id%in%unique(xx$id)])
  }))
  
  if(!all(tme)) stop("follow up times for each patient in test data must be greater than or equal to that in training data") ## not sure if we'll need this test 
  
  newdata$time <- round(newdata$time)
  
  newdata <- plyr::ddply(dd,  .variables = "id", .fun = function(xx){
    xx <- xx[order(xx$time, decreasing = TRUE), ]
    yy <- tail(xx, 1)
    tab <- subset(newdata, id%in%unique(xx$id))
    yy <- do.call(rbind.data.frame, lapply(1:nrow(tab), function(zz) yy))
    yy[, nme] <- tab[, nme] 
    pred <- predict(model, newdata= yy) 
    yy$risk <- pred$Y.star 
    yy$meanRisk <- (yy$meanRisk +  yy$risk)/2
    k <- unique(yy$cluster)
    sm <- model$smooth[[k]]$smooth 
    yy$smoothRisk <- (yy$smoothRisk + predict(sm, x= yy$time)$y)/2  
    yy 
  })
  
 return(newdata)
  }
  

}
