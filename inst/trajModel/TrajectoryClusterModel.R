library(Formula)
library(gbm)
library(plyr)
library(caret)
library(lme4)
library(inTrees)
library(ggplot2)
library(parallel)
library(class)
library(Hmisc)
library(kernlab)
require(flexmix)

require(gplots)
require(bayou)

library(devtools)
library(roxygen2)
Rcode <- as.package("C:/Users/m133937/Dropbox/Research/VirtualReality/computation/Rpackages/Vira")
load_all(Rcode)
document(Rcode)

### predict longidutinal profile of left ventricular mass index (increase or normal) 
### in  patients undergoing aortic valve surgery.
#### LVM is considered increased if >134 g/m(2) in male patients and >110 g/m(2) in female patients 
data(heart.valve)
dat <- heart.valve

levels(dat$sex) <- c("Male", "Female")
levels(dat$lvh) <- c("No", "Yes")
levels(dat$acei) <- c("No", "Yes")
levels(dat$hc) <- c("Absent", "Treated", "Untreated")
levels(dat$prenyha) <- c("I-II", "III-IV")
levels(dat$dm) <- c("No", "Yes")

## potential modifiable variables 
## acei = ace inhibitor(drug). ACE inhibitors treat a variety of conditions, such as high blood pressure, scleroderma and migraines. 
## dm = preoperative diabetes
## creat = preoperative serum creatinine
## hc = preoperative high cholesterol
## prenyha= preoperative New York Heart Association (NYHA): 1 = I/II; 3 = III/IV 
## bsa = preoperative body surface area.
## fuyrs= maximum follow up time, with surgery date as the time origin (years)



#### train mixed effect machine learning 

seed = 123 
para <- list(
  method = "cv",
  tuneLength=3,
  number = 3,
  n.trees=100,
  ntree = 50, 
  interaction.depth=4,
  shrinkage=0.01,
  n.minobsinnode=10,
  opt.para= TRUE, 
  include.RE = FALSE,
  con.tree = FALSE, 
  max.iter = 10, alpha=0.05, minsize=20,maxdepth=30,  
  K = 3, 
  krange = 2:5,
  tol= 1e-5,
  seed = seed
)

dat$id <- as.numeric(dat$id)  ## random effect grouping variable
resp.vars <- "inc.lvmi"
id <- "id"

## fixed effect variables 
rhs.vars <- c("sex", 
              "age", 
              "time", 
              "grad", 
              "log.grad", 
              "bsa", 
              "lvh", 
              "prenyha", 
              "redo", 
              "size",
              "con.cabg", 
              "creat", 
              "dm", 
              "acei", 
              "lv", 
              "emergenc", 
              "hc", 
              "sten.reg.mix", 
              "hs")

rand.vars= "time"  ## random effect variables 
order.vars = "time"

document(Rcode)

######
form <- as.formula(paste0(paste0(resp.vars, " ~"), paste0(rhs.vars, collapse = "+"))) 

document(Rcode)

model <- MEmixgbm(form = form, dat=dat, groups = id,  rand.vars= rand.vars,  para = para,   
                max.iter =20, include.RE =FALSE, maxdepth=5, k=3, krange = 2:5, decay = 0.05)


save(model, file = "C:/Users/m133937/Dropbox/Research/VirtualReality/computation/Rpackages/Vira/data/model.rda")


### plot 
d1 <- model$data[, c("id", "cluster", "time", "risk")]
d2 <- model$data[, c("id", "cluster", "time",  "smoothRisk")]
names(d2) <- c("id", "cluster", "time",  "risk")
d2$cluster <- d2$cluster + 3

d1$id <- factor(d1$id)
d1$cluster <- factor(d1$cluster)
d2$id <- factor(d2$id)
d2$cluster <- factor(d2$cluster)

d <- rbind(d1, d2)
d$id <- factor(d$id)
d$cluster <- factor(d$cluster)

col = col2hex(c("darkgreen", "darkblue", "darkred",  "purple"))
col1 <- c(makeTransparent(col[1], alpha = 20), makeTransparent(col[2], alpha=20), 
          makeTransparent(col[3], alpha = 20))
col2 <- c(col1, col)


png(filename ="C:/Users/m133937/Dropbox/Research/VirtualReality/computation/Rpackages/Vira/inst/traj.png", width = 1050, height = 850)
pp <- ggplot() + 
  geom_line(data = d1, aes(x = time, y = risk, group = id, color = cluster, size = cluster)) +  
  geom_line(data = d2, aes(x = time, y = risk, group = id, color = cluster, size = cluster )) +
  scale_size_manual(values = c(1, 1, 1, 1.5, 1.5, 1.5)) + 
  geom_jitter() + 
  guides(colour = FALSE, size = FALSE) +scale_x_continuous(name="Time (years from surgery)",breaks=0:11) + 
  scale_color_manual(values = col2 )  + ylab("Predicted Risk") + 
  annotate("text", x = c(9.2, 11.2, 9.2), y = c(-4, 1.3, 4.5), label = c("T1", "T2", "T3"), size = 5) + 
  theme(axis.title.x=element_text(size=18,face="bold"), 
        axis.title.y=element_text(size=18,face="bold"),
        legend.text = element_text(size=18,face="bold"), 
        axis.text.x = element_text(size = 18, face="bold",colour = "gray40"),
        legend.title = element_text(size=18,face="bold"),
        axis.text.y = element_text(size = 18, face="bold",colour = "gray40"))
print(pp)
dev.off()




### ass a highlight individual longitudinal profile of a single patient 
ix <- ddply(dat, .variables = "id",  NROW)
ip <- subset(d1, id==26)
ip$id <- paste0("patient", ip$id)
ip$cluster = "patient5"
col3 <- c(col2, makeTransparent(col2hex("darkblue"), 150))

png(filename ="C:/Users/m133937/Dropbox/Research/VirtualReality/computation/Rpackages/Vira/inst/traj.png", width = 1050, height = 850)
pp <- ggplot() + 
  geom_line(data = d1, aes(x = time, y = risk, group = id, color = cluster, size = cluster)) +  
  geom_line(data = d2, aes(x = time, y = risk, group = id, color = cluster, size = cluster )) +
  geom_line(data = ip, aes(x = time, y = risk, group = id, color = cluster, size = cluster)) +
  scale_size_manual(values = c(1, 1, 1, 1.5, 1.5, 1.5, 1.4)) + 
#  geom_jitter() + 
  guides(colour = FALSE, size = FALSE) +
  scale_color_manual(values = col3 ) + 
  xlab("Time (years from AVR)") + ylab("Predicted continous risk function") + 
  annotate("text", x = c(9.0, 10.0, 8.8, 5.5), y = c(-4.4, 0.2, 4.7, 2.3), 
           label = c("Improving course", "Stable/Slow progression", "Rapid progression", 
                     "Trajectory of PID = 26"), size = 8) + 
  theme(axis.title.x=element_text(size=22,face="bold"), 
        axis.title.y=element_text(size=22,face="bold"),
        legend.text = element_text(size=22,face="bold"), 
        axis.text.x = element_text(size = 22, face="bold",colour = "gray40"),
        legend.title = element_text(size=22,face="bold"),
        axis.text.y = element_text(size = 22, face="bold",colour = "gray40"))
print(pp)
dev.off()



### test predictions 

document(Rcode)

pred <- predRiskScore()


ix <- sample(unique(dat$id), 26)
newdata <- subset(dat, id ==26)
write.csv(newdata, file="C:/Users/m133937/Dropbox/Research/VirtualReality/computation/Rpackages/Vira/inst/trajModel/testdata.csv", row.names = FALSE)

### convert to json 
library(jsonlite)
nme <- c("id","age", "sex", "time","acei", "dm", "creat", "hc", "prenyha", "bsa")
x <- toJSON(tail(newdata[,nme],3))



#pred <- predRiskScore(input = newdata)





library(opencpu)

### push package to github and install with 

### first uninstall package - not sure if this step is required
remove_apps(repo="nguforche/Vira")

## install package from repository - using github - package must be public? 
install_apps(repo="nguforche/Vira")

### run 
## update, install and run
ocpu_start_app("nguforche/Vira")



curl http://localhost:5656/ocpu/apps/nguforche/Vira/R/predRiskScore/json \
-H "Content-Type: application/json" \
-d '{"input" : [ {"id":190,"age":68.726,"sex":"Male","time":2.9123,"acei":"Yes","dm":"No","creat":114,"hc":"Absent","prenyha":"I-II","bsa":2.02},{"id":190,"age":69.7233,"sex":"Male","time":3.9096,"acei":"Yes","dm":"No","creat":130,"hc":"Treated","prenyha":"I-II","bsa":2.02} ]}'









