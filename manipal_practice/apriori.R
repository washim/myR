library(arules)
library(arulesViz)
rules <- apriori(titanic.raw)
rules <- apriori(titanic.raw,
                 control=list(verbose=F),
                 parameter = list(minlen=3, supp=0.0002, conf = 0.2),
                 appearance = list(
                   rhs=c("Survived=Yes","Survived=No"),
                   lhs=c("Class=1st","Class=2nd","Class=3rd","Class=Crew","Sex=Male"),
                   default="none")
)
rules.sorted <- sort(rules, by="confidence")
inspect(rules.sorted)

plot(rules.sorted)
plot(rules.sorted, method = "grouped")
plot(rules.sorted, method = "graph")
plot(rules.sorted, method = "graph", control = list(type="items"))
plot(rules.sorted, method = "paracoord", control = list(reorder=TRUE))
