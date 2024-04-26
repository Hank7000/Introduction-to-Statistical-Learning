###Chapter 2 ---- Statistical Learning 

### Ex. 8
head(College) ###first look at the dataset

###(c)
summary(College) ###  i - summary of the main variables 
pairs(College[,1:10]) ### ii - pairs plot of the first 10 variables 
boxplot (College$Outstate, College$Private) ### iii - boxplots of Outstate vs Private 

### iv - creating 'Elite' variable by binning 'Top10perc' variable based on top 50% high schools of provenience 
Elite = rep('No', nrow(College)) 
Elite[College$Top10perc > 50] = 'Yes'
sum(Elite == 'Yes') ###how many Elite colleges? 78
Elite = as.factor(Elite) ### converting 'Elite' into qualitative variable 
levels(Elite) ### checking 'Elite' levels 
College = data.frame(College, Elite) ### integrating Elite into College dataset 
### checking relationship between elite colleges and out of state tuition(OST): no surprises, 
### elite colleges' OST seems to be way higher 
plot(College$Elite, College$Outstate)

### v - produce some histograms to illustrate the connection between some of the quantitative variables 

###creating some vectors to group variables by category 
coll.costs = c('Oustate', 'Room.board', 'Books', 'Personal', 'Expend') ###general costs 
coll.stats = c('Apps', 'Accept', 'Enroll', 'Grad.Rate', 'Top10Perc', 'Top25Perc',
               'F.Undergrad', 'P.Undergrad', 'S.F.Ratio') ###stats about the colleges 
coll.students = c('PhD', 'Terminal', 'perc.alumni') ###parameters regarding individual students 

hist.coll = function(b) {hist(College[[b]])} ###creating a function that produces histograms for every element of College 
lapply(coll.costs, hist.coll) ###using lapply to run the function for every element of our list automatically
lapply(coll.stats, hist.coll)
lapply(coll.students, hist.coll)
is.numeric(College$Accept)