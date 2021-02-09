newLine <- function () {
  blank <- matrix()
  rownames(blank) <- ""
  colnames(blank) <- ""
  print(blank, na.print = "")
}
justTheString <- function (str) {
  blank <- matrix()
  rownames(blank) <- str
  colnames(blank) <- ""
  print(blank, na.print = "")
}

solve4 <- function () { 
  
  justTheString ("Datasetul: CO2")
  newLine()
  
  # Media si Varianta
  tab <- matrix (rep(0, 2*2), nrow = 2, ncol = 2)
  rownames(tab) <- c("Media", "Varianta")
  colnames(tab) <- names(CO2)[4:5]
  for (i in 1:2) {
    tab[1, i] <- floor(mean(CO2[,i+3]) * 100) / 100
    tab[2, i] <- floor(var (CO2[,i+3]) * 100) / 100
  }
  print (tab)
  newLine()
  
  justTheString ("Quantile")
  
  tab <- matrix (rep(0, 2*11), nrow = 2, ncol = 11)
  rownames(tab) <- names(CO2)[4:5]
  colnames(tab) <- strsplit(paste (strsplit(toString(0:11 * 10), ", ")[[1]], collapse = "% "), " ")[[1]][1:11]

  for (i in 1:2) {
    tab[i,] <- quantile(CO2[,i+3], probs = (0:10 / 10))
  }
  print (tab)
  newLine()
  
  #boxplot-uri pentru comparatie
  boxplot(CO2, main="Complete boxplot", ylab="Value", ylim=c(0,800), las=1)
  boxplot(CO2$conc[CO2$Type=="Quebec"], main = "Concentratia de co2 a plantelor din Quebec", ylab="Conc")
  }
solve4()
