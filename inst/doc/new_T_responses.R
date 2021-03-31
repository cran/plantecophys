## ---- echo=FALSE, fig.width=8, fig.height=4-----------------------------------
.Rgas <- function()8.314
Tk <- function(x)x+273.15
TJmax <- function(Tleaf, EaJ, delsJ, EdVJ){
  J1 <- 1+exp((298.15*delsJ-EdVJ)/.Rgas()/298.15)
  J2 <- 1+exp((Tk(Tleaf)*delsJ-EdVJ)/.Rgas()/Tk(Tleaf))
  exp(EaJ/.Rgas()*(1/298.15 - 1/Tk(Tleaf)))*J1/J2
}

# Vcmax temperature response (Arrhenius)
TVcmax <- function(Tleaf, EaV, delsC, EdVC){
  
  if(EdVC > 0){
    V1 <- 1+exp((delsC*(25 + 273.15)-EdVC)/(.Rgas()*(25 + 273.15)))
    V2 <- 1+exp((delsC*(Tleaf+273.15)-EdVC)/(.Rgas()*(Tk(Tleaf))))
    f <- V1/V2
  } else f <- 1
  
  exp((Tleaf-25)*EaV/(.Rgas()*Tk(Tleaf)*Tk(25))) * f
}

par(mfrow=c(1,2))
curve(TVcmax(x, 82620.87,645.1013,0), from=10, to=40, ylim=c(0,4),
      xlab=expression(T[leaf]~~(degree*C)), 
      ylab=expression(V[cmax] / V[cmax25]))
curve(TVcmax(x, 58550,629.26,200000), from=10, to=40, lty=5, add=T)
legend("topleft", c("old","new"), lty=c(1,5))

curve(TJmax(x, 39676.89, 641.3615, 200000), from=10, to=40, ylim=c(0,1.4),
      xlab=expression(T[leaf]~~(degree*C)), 
      ylab=expression(J[max] / J[max25]))
curve(TJmax(x, 29680, 631.88, 200000), from=10, to=40, add=T, lty=5)


