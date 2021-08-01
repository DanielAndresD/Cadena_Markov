#Declaraci�n de las condiciones iniciales
#Vector inicial Po
Po<- matrix(c(0.1,0.2,0.5,0.2),nrow =1 )
print (Po) #visualizar el Vector Po
#Matriz de transici�n A
A<- matrix(c(0.1,0.7,0.2,0.4,0.2,0.1,0.6,0.1,0.2,0.1,0.1,0.3,0.5,0.1,0.1,0.2),nrow=4)
print(A)#Visualizar la matriz de transici�n A

i<- NULL #Contador para el ciclo for
n<-5 #potencia de la matriz A
B<-A # Matriz acumulada A(i)

#ciclo para el c�lculo de la matriz A(5)
for (i in 2:n)
  B=B%*%A
print(B)
# C�lculo de P5
P5=Po%*%B #Producto matricial Po*A(5)
#a) Calcular P5
print(t(P5)) #imprimir el resultado de P5
#-------------------------------------------------------------------------
#Capturar P(X5)=30
#vector para X=30
v1<-matrix(c(1,0,0,0),ncol =1 )
Px5=P5%*%v1
#Probabilidad de P(X5=30)
print(Px5)
n<-11
C<-A
for (i in 2:n)
  C=C%*%A
# C�lculo de P11 X=-12 dado X=30 en P5
#Capturar P(X5)=-12
#vector para X=-12
P11=Po%*%C
v2<-matrix(c(0,0,1,0),ncol =1 )
Px11=P11%*%v2
#Probabilidad de P(X11=-12)

#b) Probabilidad condicional de P(X11=-12|X5=30)
PX11_PX5=Px11%*%Px5
print(PX11_PX5)

#----------------------------------------------------------
#Valor esperado en x7
n<-7
D<-A
for (i in 2:n)
  D=D%*%A
print(D)
E=Po%*%D
print(E)
VP<-matrix(c(30,40,-12,-26),nrow =1 )
U=VP%*%t(E)
print(U)
#-----------------------------------------------------------------
# valor esperado en E(X2|X1=-26)
#Valor esperado en x7
n<-2
D<-A
Poo<- matrix(c(0,0,0,1),nrow =1 )
for (i in 2:n)
  D=D%*%A
print(D)
E=Poo%*%D
print(E)
VP<-matrix(c(30,40,-12,-26),nrow =1 )
U=VP%*%t(E)
print(U)
#-----------------------------------------------------------------

#e) C�lculo de P6 X=40 dado X=-26 en P4
#Capturar P(X5)=-12
n<-4
C<-A
for (i in 2:n)
  C=C%*%A
P4=Po%*%C
v2<-matrix(c(0,0,0,1),ncol =1 )
Px4=P4%*%v2

n<-6
C<-A
for (i in 2:n)
  C=C%*%A
P6=Po%*%C
v2<-matrix(c(0,1,0,0),ncol =1 )
Px6=P6%*%v2
#d) Probabilidad condicional de P(X11=-12|X5=30)
PX6_PX4=Px4%*%Px6
print(PX6_PX4)


#----------------------------------------------------------
#f) Valor esperado a la larga
n<-100
D<-A
for (i in 1:n)
  D=D%*%A
print(D)
E=Po%*%D
print(E)
VP<-matrix(c(30,40,-12,-26),nrow =1 )
U=VP%*%t(E)
print(U)



