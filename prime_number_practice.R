
######
### Prime Numbers Practice
######

### Create Integer detecting function

eval.integer<-function(x){
  if (floor(x)-x == 0){
    return(TRUE)
  }else{
    return(FALSE)
  }
}
eval.integer(2.1)

### Create a prime detectiing Function

is.prime<-function(x){
  if(x==2){
    return(TRUE)
  }else{
  if (eval.integer(x)==F){
    return(FALSE)
  }else{
    z<-c()
    for(i in c(2:ceiling(x/2))){
      z[i]<-eval.integer(x/i)
    }
    return(ifelse(sum(z,na.rm = T)>0,FALSE,TRUE))
  }
  }
}

is.prime(11)

### Evaluate primes to 1000

y<-c()
for (j in c(2:1000)){
  y[j]<-is.prime(j)
}

### Function to count number of primes

count.primes<-function(x){
  a<-c()
  for (i in c(2:x)){
    a[i]<-is.prime(i)
  }
  return(sum(a,na.rm = T))
}

y<-c()
for(i in c(2:1000)){
  y[i]<-count.primes(i)
}

plot(c(1:1000),y)


### Plot system times

time.passed<-function(code){
  start.time <- Sys.time()
  code
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  time.taken  
}
time.passed(is.prime(10^6))

for(i in c(1:10)){
  print(time.passed(is.prime(10^i)))
} # once you get to 8 it takes significantly longer (from 3.8s to 38s from 7 to 8)


### Plot primes

z<-c()
for (i in c(2:1000)){
  z[i]<-is.prime(i)
}

plot(c(1:1000),z)


time.passed(for (i in c(2:1000)){
  z[i]<-is.prime(i)
})


######
### Make a better function
######

is.even<-function(x){
  z<-c()
  for(i in c(1:NROW(x))){
    z[i]<-eval.integer(x[i]/2)
  }
  return(z)
}
is.even(c(2:7))

is.prime2<-function(x){
 if(x==2){
   return(TRUE)
 } else{
   if (eval.integer(x)==F){
     return(FALSE)
   }else{
     if(is.even(x)==T){
       return(FALSE)
     }else{
       a<-c(2:ceiling(x/2))
       a<-a[is.even(a)==F]
       z<-c()
       for(i in seq_along(1:NROW(a))){
         z[i]<-eval.integer(x/a[i])
       }
       return(ifelse(sum(z,na.rm = T)>0,FALSE,TRUE))
     }
   }
 }
}
is.prime2(21)

### Evaluate; slower...

time.passed(is.prime(10^7+1))
time.passed(is.prime2(10^7+1))

### For vectors

is.prime2.1<-function(x){
  z<-c()
  for (i in seq_along(1:NROW(x))){
    z[i]<-is.prime2(x[i])
  }
  return(z)
}
x<-c(3:11)
is.prime2.1(c(5:11))


######
### Make a better function
######

### Try #3

is.prime3<-function(x){
  if(x==2){
    return(TRUE)
  }else{
    if (eval.integer(x)==F){
      return(FALSE)
    }else{
      if(is.even(x)==T){
        return(FALSE)
      }else{
        
      
      z<-c()
      for(i in c(2:ceiling(x/2))){
        z[i]<-eval.integer(x/i)
      }
      return(ifelse(sum(z,na.rm = T)>0,FALSE,TRUE))
      }
    }
  }
}

is.prime3(8)

### Evaluate; sig faster for doing vectors

time.passed(is.prime(10^7+1))
time.passed(is.prime3(10^7+1))

z<-c()
time.passed(for (i in c(2:1000)){
  z[i]<-is.prime(i)
})
time.passed(for (i in seq_along(1:NROW(1000))){
          z[i]<-is.prime3(i)
        })

### Percent Prime

percent.primes<-function(x){
  a<-c()
  for (i in c(1:x)){
    a[i]<-is.prime3(i)
  }
  b<-sum(a)/x
  return(b)
}
percent.primes(9)

z<-c()
for (i in c(1:1000)){
  z[i]<-percent.primes(i)
}

plot(c(1:1000),z,xlab="Number",ylab="Percent",title("Percent of Numbers Below X that are Prime"))



