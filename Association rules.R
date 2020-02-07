
install.packages("arules")
library(arules)
data("Groceries")
data <- Groceries
data
View(data)

# Type of data 
class(data)
summary(data)

# No of nonempty cells
prod(dim(data))*0.02691

#INSPECT
#==========================================================================

inspect(Groceries[1:3])
inspect(Groceries[1000])

?itemFrequencyPlot
itemFrequencyPlot(Groceries, support=0.1)    # Minimal support
itemFrequencyPlot(Groceries, topN = 10)      # Top product by frequency


# FINDING RULES:
#==========================================================================
# Finding a rules with minimum support 0.007 and minimum confidence 0.25

rules1 <- apriori(data=Groceries, parameter = list(supp=0.01,conf=0.15,minlen=2))

summary(rules1)
inspect(rules1[1:5])

# Sorting the rules 
rules <- sort(rules1,descending= TRUE,by="lift")

inspect((rules[1:5]))


# Rule 2
#========
# Finding all the rules such that the LHS learn to {whole milk} in RHS with a
# support and confidence thresold is 0.01 and 0.5

rules2 <- apriori(data=Groceries, parameter = list(supp=0.01,conf=0.5),
                  appearance = list(default = "lhs",rhs="whole milk"))
                  
summary(rules2)
inspect(rules2[1:5])

rules <- sort(rules2,descending= TRUE,by="lift")

inspect(rules[1:5])

# Rule 3
#========

rules3 <- apriori(data=Groceries, parameter = list(supp=0.01,conf=0.5),
                  appearance = list(default = "lhs",rhs="other vegetables"))

summary(rules3)
inspect(rules3[1:4])

rules <- sort(rules3,descending= TRUE,by="lift")

inspect(rules[1:4])
























