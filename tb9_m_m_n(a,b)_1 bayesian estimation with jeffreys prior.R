library(cubature)
n=10;rho=0.1;b=16
rhoSELF=c()
riskSELF=c()
rhoPLF=c()
riskPLF=c()
for(t in 1:1000){
for(i in 1:n){
  x[i]=rgeom(1,1/(1+(b*rho)))
}
a<-1:n
y=sum(x[a])
y
f<-function(x){
  x^(y+(1/2))*(1-x)^(n-2)
}
e1=adaptIntegrate(f,0,b/(b+1))
numerator[t]=e1$integral
g<-function(x){
  x^(y-(1/2))*(1-x)^(n-1)
}
e2=adaptIntegrate(g,0,b/(b+1) )
denomenator[t]=e2$integral
h<-function(x){
  x^(y+(3/2))*(1-x)^(n-3)
}
e3=adaptIntegrate(h,0,b/(b+1))
numerator1[t]=e3$integral
rhoSELF[t]=numerator[t]/(b*denomenator[t])
print(rhoSELF[t])
riskSELF[t]=(numerator1[t]/(b^(2)*denomenator[t]))-(rhoSELF[t])^(2)
print(riskSELF[t])
rhoPLF[t]=(numerator1[t]/(b^(2)*denomenator[t]))^(1/2)
print(rhoPLF[t])
riskPLF[t]=2*(rhoPLF[t]-rhoSELF[t])
print(riskPLF[t])
}
b<-1:1000
mean(rhoSELF[b])
mean(riskSELF[b])
mean(rhoPLF[b])
mean(riskPLF[b])

