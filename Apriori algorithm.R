#### APRIORI ALGORITHM ####
install.packages('arules')
library('arules')

load("titanic.raw.rdata")
View(titanic.raw)

## draw a sample of 10 
idx = sample(1:nrow(titanic.raw),10)
titanic.raw[idx,]

# Summary of titanic data
summary(titanic.raw)

## Apply apriori algorithm to extract rules with
# minimum support : supp = 0.1
# minimum confidence : conf = 0.8
# Maximum length of rules : maxlen

rules.all = apriori(titanic.raw)

## Check to see rules
inspect(rules.all)

## Rules with rhs containing "Survived" only
rules = apriori(titanic.raw,
                control = list(verbose=F),
                parameter = list(minlen=2, supp=0.005, conf=0.8),
                appearance = list(rhs = c("Survived=No", "Survived=Yes"),
                                  default = "lhs"))

## Keep the decimal places
quality(rules) = round(quality(rules), digits = 3)

## Order rules by lift
rules.sorted = sort(rules, by='lift')

## View sorted rules
inspect(rules.sorted)

## inspect 1st two rules
inspect(rules.sorted[1:2])

## Remove the Redundant rules
subset.matrix = is.subset(rules.sorted, rules.sorted, sparse = FALSE)
subset.matrix[lower.tri(subset.matrix, diag = T)] = NA

redundant = colSums(subset.matrix, na.rm = T) >= 1

# Which rules are redundant
which(redundant)

# Remove redundant rules
rules.pruned = rules.sorted[!redundant]
rules.pruned

# Inspecting the pruned rules
inspect(rules.pruned[1])

# Inspect the rules for children in other classes who have survived to interpret rule

rules = apriori(titanic.raw, control = list(verbose=F),
                parameter = list(minlen=3, supp = 0.002,  conf=0.2),
                appearance = list(default = "none", rhs = c("Survived=Yes"),
                                  lhs = c("Class=1st","Class=2nd","Class=3rd","Class=Crew",
                                          "Age=Child","Age=Adult","Sex=Male")))

rules.sorted = sort(rules, by="confidence")
inspect(rules.sorted)[1]


##Plot the rules
library('arulesViz')
plot(rules)

plot(rules.all, method = "grouped")

plot(rules.all, method = "graph")

plot(rules.all, method = "graph", control = list(type="items"))

plot(rules, method="paracoord", control=list(recorder=TRUE))
