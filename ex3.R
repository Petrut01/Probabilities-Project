

solve3 <- function(n){
  
  ok <- 0
  cnt <- 0
  x <- rep(c(0), times=n)
  while (ok == 0) {
    timp <- Sys.time()
    ora <- hour(timp)
    min <- minute(timp)
    sec <- round(second(timp),0)
    t1 <- min*100 + sec
    if (t1 %% 17 ==0)
      {x[1] <- rnorm(1,min,sec)
      cnt <- 0
      ok <- 1}
      else if(t1 %% 17 == 3)
        {x[1] <- rpois(1,min) + runif(1,-1,1)
        cnt <- 0
        ok <- 1}
        else if(t1 %% 17 == 5)
          {x[1] <- rexp(1,ora)
          cnt <- 0
          ok <- 1}
          else if(t1 %% 17 == 7)
            {x[1] <- rbinom(1, ora, 1/min) + runif(1, 0, 5)
            cnt <- 0
            ok <- 1}
              else if(t1 %% 17 == 8)
                {x[1] <- runif(1,-5,7)
                cnt <- 0
                ok <- 1}
                else if(t1 %% 17 == 11)
                  {x[1] <- rgamma(1) - rhyper(1)
                  cnt <- 0
                  ok <- 1}
                    else cnt <- cnt + 1
    
      if(cnt == 3) #daca procesul este reluat de inca 2 ori ne oprim
        ok <- 1
  }
  if (cnt == 3)
    x[1] <- rnorm(1,0,1)
  for (i in 2:n)
    x[i] <- rexp(1,5)*x[i-1]+ rnorm(1,2,1) #formula de recurenta
  
  print(x)  
  
  hist(x, main = "Histograma vectorului x", col = "blue")
  }
solve3(6)

data("CO2")
