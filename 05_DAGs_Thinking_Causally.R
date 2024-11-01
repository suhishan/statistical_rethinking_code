library(rethinking)
library(dagitty)
library(tidyverse)
library(conflicted)

data("WaffleDivorce")
d <- WaffleDivorce
# Making DAGs. ------------------------------------------------------------

dag5.1 <- dagitty( "dag{ A -> D; A -> M; M -> D }" )
coordinates(dag5.1) <- list( x=c(A=0,D=1,M=2) , y=c(A=0,D=1,M=0) )
drawdag( dag5.1 )


# Looking for conditional independences:

cor(d$Divorce, d$MedianAgeMarriage)
cor(d$Divorce, d$Marriage)
cor(d$MedianAgeMarriage, d$Marriage)


DMA_dag2 <- dagitty('dag{ D <- A -> M }')
impliedConditionalIndependencies( DMA_dag2 )

