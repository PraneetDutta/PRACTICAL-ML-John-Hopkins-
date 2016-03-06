Urltrain = "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
training = read.csv(url(Urltrain), na.strings=c("NA","#DIV/0!",""))
Urltest = "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
testing <- read.csv(url(testUrl), na.strings=c("NA","#DIV/0!",""))
