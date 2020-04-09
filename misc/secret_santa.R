closeAllConnections()
graphics.off()
rm(list = ls())

library(openxlsx)

people = read.xlsx("~/Desktop/quarantine_christmas.xlsx")
D = people

out = as.list(D$person)
names(out) = D$person
out = lapply(out, function(x) {c()})


l = as.numeric(lapply(out, function(x) {length(x) == 0}))

while (sum(l) > 0) {
  
  # Get person to give gift
  n = ceiling(runif(1,min = 0,max = sum(l)))
  picker = (names(out)[l == 1])[n]
  
  # Get potential giftees
  potential = D[D$address != people$address[people$person == picker],]
  
  # Select giftee
  if (nrow(potential) >= 1) {
    s = ceiling(runif(1,min = 0,max = nrow(potential)))
    selected = potential[s,]
    D = D[-which(D$person == selected$person),]
    out[[picker]] = selected
  } else {
    d = which(unlist(lapply(out, function(x) {length(x) > 0})))
    b = ceiling(runif(1,0,length(d)))
    p = out[[d[b]]]
    D = rbind(D,p)
  }
  
  l = as.numeric(lapply(out, function(x) {length(x) == 0}))
  
}


for (n in names(out)) {
  write.csv(out[[n]],paste0("~/Desktop/",n,".csv"),row.names = FALSE)
}