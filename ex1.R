#1a

frepcomgen <- function(m, n) {
  x <- matrix(0, 2, n)
  y <- matrix(0, 2, m)
  v <- 1
  x[1, 1] <- sample(2:5, 1)
  x[2, 1] <- round(runif(1, 0, v), digits = 2)
  v <- v - x[2, 1]
  for (i in 2:(n - 1)) {
    x[1, i] <- sample(2:5, 1) + x[1, i - 1]
    x[2, i] <- round(runif(1, 0, v), digits = 2)
    v <- v - x[2, i]
    
  }
  x[2, n] = v
  x <- x[, order(x[1, ])]    #Ordonam matricea dupa prima linie
  
  v <- 1
  y[1, 1] <- sample(2:5, 1)
  y[2, 1] <- round(runif(1, 0, v), digits = 2)
  v <- v - y[2, 1]
  for (i in 2:(m - 1)) {
    y[1, i] <- sample(2:5, 1) + y[1, i - 1]
    y[2, i] <- round(runif(1, 0, v), digits = 2)
    v <- v - y[2, i]
  }
  y[2, m] = v
  
  y <- y[, order(y[1, ])]
  
  matrice <- matrix(0, n, m)
 
  
  auxx <- x
  auxy <- y
  
  for (i in 1:n) {
    nr <- sample(1:m, 1)
    for (j in 1:m)
    {
      if (j != nr)
      {
        matrice[i, j] <-
          round(runif(1, 0, min(auxx[2, i], auxy[2, j])), digits = 2)
        auxx[2, i] <- auxx[2, i] - matrice[i, j]
        auxy[2, j] <- auxy[2, j] - matrice[i, j]
      }
    }
  }
  
  TheMatrix <- list(x, y, matrice)
  return(TheMatrix)
  
}

m=3
n=5
matri <- frepcomgen(m, n)
matri

warnings()

#1b
fcomplrepcom <- function(x,y,matri) {
  while (1) {
    okey <- 0 #verific daca s-au mai facut modificari
    n = length(matri[, 1])
    m = length(matri[1, ])
    auxi <- 1
    auxj <- 1
    while (auxi <= n && auxj <= m)
    {
      ok <- 0
      poz <- 0
      for (count in 1:m)
        if (matri[auxi, count] == 0) {
          ok <- ok + 1 #numar valorile de 0 de pe linie
          poz <- count
          
        }
      if (ok == 1)
      {
        matri[auxi, poz] = x[2, auxi] - sum(matri[auxi, ])
        okey <- 1
      }
      
      ok <- 0
      poz <- 0
      for (count in 1:n)
        if (matri[count, auxj] == 0) {
          ok <- ok + 1 #numar valorile de 0 de pe coloana
          poz <- count
          
        }
      if (ok == 1) 
      {
        matri[poz, auxj] = y[2, auxj] - sum(matri[, auxj])
        okey <- 1
      }
      auxi <- auxi + 1
      auxj <- auxj + 1

    }
    while (auxi <= n) #daca avem mai multe linii le verificam separat
    {
      ok <- 0
      poz <- 0
      for (count in 1:m)
        if (matri[auxi, count] == 0) {
          ok <- ok + 1
          poz <- count
        }
      if (ok == 1)
      {
        matri[auxi, poz] = x[2, auxi] - sum(matri[auxi, ])
        okey <- 1
      }
      auxi <- auxi + 1
    }
    
    
    while (auxj <= m) #daca avem mai multe coloane le verificam separat
    {
      ok <- 0
      poz <- 0
      for (count in 1:n)
        if (matri[count, auxj] == 0) {
          ok <- ok + 1
          poz <- count
          
        }
      if (ok == 1)
      {
        matri[poz, auxj] = y[2, auxj] - sum(matri[, auxj])
        okey <- 1
      }
      auxj <- auxj + 1
      
    }
    
    if (okey == 0)
      return(matri)
  }
}


x <- matri[[1]]
print(x)

y <- matri[[2]]
print(y)

tabel <- matri[[3]]
print(tabel)
tabel <- fcomplrepcom(x,y,tabel)
print(tabel)


#1d
fvernecor <- function(auxx, auxy) {
  #necorelate<=> Cov(X,Y)=0
  ex <- 0
  ey <- 0
  for (i in 1:length(auxx[1, ]))
    ex <- ex + auxx[1, i] * auxx[2, i]
  for (i in 1:length(y[1, ]))
    ey <- ey + auxy[1, i] * auxy[2, i]
  exy <- 0
  
  for (i in 1:length(auxx[1, ]))
    for (j in 1:length(auxy[1, ]))
      exy <- exy + ((auxx[1, i] * auxy[1, j]) * (auxx[2, i] * auxy[2, j]))
  covxy <- exy - ex * ey
  if (covxy == 0)
    print("Sunt necorelate")
  else
    print("Sunt corelate")
}

fverind <- function(x, y, matri) {
  ok <- 1
  for (i in 1:length(matri[, 1]))
    for (j in 1:length(matri[1, ]))
      if (x[2, i] * y[2, j] != matri[i, j])
        ok <- 0
      if (ok == 1)
        print("Sunt independente")
      else
        print("Nu sunt independente")
      
}


fverind(x, y, tabel)
fvernecor(x, y)

#1c
#Cov(5X+9, -3Y-2)
auxx <- 5 * x + 9
auxx[2,] <- x[2,]
auxx
auxy <- -3 * y - 2
auxy[2,] <- y[2,]
auxy
ex <- 0
ey <- 0
for (i in 1:length(auxx[1, ]))
  ex <- ex + auxx[1, i] * auxx[2, i]  #E[X]
for (i in 1:length(y[1, ]))
  ey <- ey + auxy[1, i] * auxy[2, i] #E[Y]

exy <- 0
for (i in 1:length(auxx[1, ]))
  for (j in 1:length(auxy[1, ]))
    exy <- exy + ((auxx[1, i] * auxy[1, j]) * (auxx[2, i] * auxy[2, j])) #E[XY]
covxy <- exy - ex * ey
covxy

#P(0<X<0.8/Y>0.3)
#P(0<X<0.8/Y>0.3)=P(0<X<0.8 n Y>0.3)/P(Y>0.3)=P(0<X<0.8)*P(Y>0.3)/P(Y>0.3)=P(0<X<0.8)

#P(0<X<0.8)
sumx <- 0
for (i in 1:length(x[1, ]))
  if (x[1, i] > 0 && x[1, i] < 0.8)
    sumx <- sumx + x[2, i]
raspuns2 = sumx
raspuns2


#P[X>0.2,Y<1.7]
sumx <- 0
sumy <- 0
for (i in 1:length(x[1, ]))

  if (x[1, i] > 0.2)
    sumx <- sumx + x[2, i]

for (i in 1:length(y[1, ]))
  if (y[1, i] < 1.7)
    sumy <- sumy + y[2, i]

raspuns3 <- sumx * sumy   
raspuns3
