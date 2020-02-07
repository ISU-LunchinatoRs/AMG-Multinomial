####################################################
#   multinom and mlogit
#        Ann Marie Gawel
#  LunchinatoRs 07FEB2020
#####################################################

######################################################
#          load packages                             #

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# Load libraries

packages <- c("tidyverse", "ggplot2",  "nnet", "foreign", "lme4", "gmnl", "foreach", "mlogit")
ipak(packages)

####################################################
#        Example using "multinom" function
#           "nnet" package
###################################################

ml <- read.dta("https://stats.idre.ucla.edu/stat/data/hsbdemo.dta")
str(ml)

table(ml$ses, ml$prog)

with(ml, do.call(rbind, tapply(write, prog, function(x) c(M = mean(x), SD = sd(x)))))

ml$prog2 <- relevel(ml$prog, ref = "academic")
test <- multinom(prog2 ~ ses + write, data = ml)
summary(test)

#####################################################
#        Example with mlogit package
##################################################

data("Heating", package = "mlogit")
H <- mlogit.data(Heating, shape = "wide", choice = "depvar", varying = c(3:12))
m <- mlogit(depvar ~ ic + oc | 0, H)
summary(m)

mi <- mlogit(depvar ~ oc + I(ic / income), H)
summary(mi)



#########################################################

#Run model with seedfate data

##############################
#   load data                #

fate <- read.csv("data/tidy/manipulated for summaries/fate_perseed.csv")


fate$seedfate <- as.factor(fate$seedfate)
fate$seedfate <- relevel(fate$seedfate, ref = "ignored")
seed_mnom <- multinom(seedfate ~ species, data = fate)
summary(seed_mnom)

##########################
# to reshape for mlogit:
#########################

sd_mdat <- mlogit.data(fate, shape = "wide", choice = "seedfate")
sd_mlg <- mlogit(seedfate ~ 0|species, data = sd_mdat, reflevel = "ignored")
summary(sd_mlg)
#this works, but it is unclear what is specified as a random or a fixed effect, info on the model shows that rpar is null, so assuming they are not 


#when I add rpar, it gives me an error message
sd_mlg2 <- mlogit(seedfate ~ 0|species + ratID, data = sd_mdat, rpar = (ratID = 'n'))


