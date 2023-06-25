#program created by Ethan Ferneyhough analysing LPPL curve model on TSLA stock
Stock <-read.csv(file.choose(),header=TRUE)
x1<-matrix(,length(Stock[,2]),4,TRUE)#creates a matrix of x values
y1<-matrix(,length(Stock[,2]),1,TRUE)#creates a matrix of y values
m<-.6# variable that can change between .01,1.2
t<--1#shift function 
w<-4#amount of periods between 2-25
for(i in 1:length(Stock[,2])){
  y1[c(i),]<-log(Stock[i,2])
  tep<-(i-t)^m  #second parameter of matrix
  tep2<-tep*cos(w*log((i-t))) #third parameter of matrix
  tep3<-tep*sin(w*log((i-t))) #fourth parameter of matrix
  x1[c(i),]<- c(1,tep,tep2,tep3)
}
plot(1:length(Stock[,2]),y=Stock[,2],main= 'Stock over time', xlab='time(days)', ylab='Stock')
  tepeq<-t(x1)%*%x1 #temporaryrily holding variables.
  matinv<-solve(tepeq) #calculating the inverse
  eq<-matinv%*%t(x1)%*%y #this is the equation for the line.
  A<-eq[1,1]
  B<-eq[2,1]
  C<-eq[3,1]
  D<-eq[4,1]
  curve(exp(A+B*(x-t)^m+C*(x-t)^m*cos(w*log(x-t))+D*(x-t)^m*sin(w*log(x-t))),from=-1, to=length(Stock[,2]),add=TRUE,type="l",col='blue')
  
  