aff<-read.csv("/Users/jeesh/OneDrive/Desktop/Econometrics Data/Wooldridge Data 2011/affairs.csv", header=FALSE, na.strings = ".")
colnames(aff) <- c("id",     "male",       "age",      "yrsmarr",      "kids",       "relig",
                   "educ",    "occup", "ratemarr",      "naffairs",     "affair",   "vryhap",
                   "hapavg",  "avgmarr",  "unhap",  "vryrel",   "smerel",    "slghtrel",
                   "notrel")

aff_lm1<-lm(naffairs~yrsmarr+kids+ratemarr+unhap+relig,data=aff)
summary(aff_lm1)
