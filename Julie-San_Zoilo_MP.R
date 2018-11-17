# To give you more choices, I've added another item in the assignment. You need to answer and submit just 5 questions among these:
# 
# 
# 1.	Define an R function that removes NA values from a vector.

x<-c(1,NA,3,4,5,NA)
remove_na=function(d){d[!is.na(d)]}
remove_na(x)



# 2.	Define an R function that computes the factorial of given an integer argument. 
# The output should be a vector of length 1.


#version 1

Factorial_loop<-function(x){
  result<-1
  if(x==0)return(1)
  for(i in 1:x) {
    result<-result*i
  }
  return(result)
}  

occurance<-100
resultaat<-Factorial_loop(occurance)
cat("The result is:", resultaat)



# 3.	Define an R function that computes the determinant of a given matrix. The output should be a vector of length 1.





# 4.	Define an R function that sorts a given vector in decreasing order. The output should be a vector of the same length. It should accept both numeric or character vectors.

vector_sort<-function(A){
  for(j in 2:length(A)){
    key=A[j] 
    # insert A[j] into sorted sequence A[1,...,j-1] 
    i=j-1 
    while(i>0&&A[i]>key){
      A[(i + 1)] = A[i]
      i=i-1 
    }
    A[(i+1)]=key
  }
  A
}
vector_sort(c(6,5,6,7,3,2,1,17,12))

vector_sort(c('z','s','e','u','n','y'))


# 5.	Define an R function that accepts a Date (POSIXct) as argument and outputs the day of the week as characters. Use modulo operator.




date_nw<-as.POSIXct(as.Date("01/01/1970",format="%d/%m/%Y"))


which_weekday<-function(date_nw) 
{c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")[((unclass(date_nw)/86400)%%7)+1]}


which_weekday(date_nw)


# 6.	Create a function to compute for your net pay at work.


Salary<-function(monthly.pay){
  
  pay<-(monthly.pay*12)
  
  Bracket_1<-((pay-250000)*0.20)/12
  Bracket_2<-(((pay-400000)*0.25)+30000)/12
  Bracket_3<-(((pay-800000)*0.30)+130000)/12
  Bracket_4<-(((pay-2000000)*0.32)+490000)/12
  Bracket_5<-(((pay-8000000)*0.35)+2410000)/12
  
  
  
  
  
  if(pay<=250000){return(monthly.pay)
  }else if(pay<=400000)return(monthly.pay-Bracket_1)
  else if(pay<= 800000)return(monthly.pay-Bracket_2)
  else if(pay<=2000000)return(monthly.pay-Bracket_3)
  else if(pay<=8000000)return(monthly.pay-Bracket_4)
  else if(pay>8000000)return(monthly.pay-Bracket_5)
  
  
}

Salary(65000)
 
# 7.	Create a function that accepts a vector and and integer n and returns nth highest number


library(dbplyr)


nth<-function(x,n,order_by=NULL,default=default_missing(x)){
  stopifnot(length(n)==1,is.numeric(n))
  n<-trunc(n)
  
  if(n==0||n>length(x)||n< -length(x)){
    return(default)
  }
  
  # Negative values index from RHS
  if(n<0){
    n<-length(x)+n+1
  }
  
  if(is.null(order_by)){
    x[[n]]
  }else{
    x[[order(order_by)[[n]]]]
  }
}

nth(x,15)

nth(x,-2)

nth(x,52)

first<-function(x,order_by=NULL,default=default_missing(x)){
  nth(x,1L,order_by=order_by,default=default)
}


last<-function(x,order_by = NULL,default=default_missing(x)){
  nth(x,-1L,order_by=order_by,default=default)
}

default_missing<-function(x){
  UseMethod("default_missing")
}



default_missing.default<-function(x){
  if (!is.object(x)&&is.list(x)){
    NULL
  } else {
    x[NA_real_]
  }
}


default_missing.data.frame<-function(x) {
  rep(NA,nrow(x))
}


# 8.	Create a function that computes the compound interest of an investment given the rate, time, and initial amount or principal.

compound_int.rate<-function(principal,int.rate=0.01,timeframe=1){
  return(principal*((1+int.rate)**timeframe-1))} 

compound_int.rate(2000000)


# 9.	Create a function isPrime(n) that accepts an integer and outputs a Boolean value (TRUE or FALSE) depending whether the integer is a prime number or not.


isPrime<-function(x){
  if(sum(x/1:x==x%/%1:x)==2)
  {print("TRUE")}
  else{print("FALSE")
  }  
}


isPrime(7)

isPrime(10)



# I'll give bonus points to the one whose choice of items is not very common with the others'. May the odds be ever in your favor.



# Regards,
# Elmer

