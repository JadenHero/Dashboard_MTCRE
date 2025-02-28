#' Filtro de Beveridge-Nelson
#'
#' Filtro_BN realiza el filtro de Descomposición estacional por Loess a las variables solicitadas y regresa una lista con los valores de la variable y su suavizamiento generada por el filtro.
#'
#' @param datos Es un data frame en el cual se encuentran las series de tiempo.
#' @param variables Es un vector con los nombres de las variables a las cuales se le quiere realizar el suavizamiento.
#' @param inicio Es la fecha en la cual inicia la serie de tiempo su estructura es c(año,periodo).
#' @param freq Es la frecuencia de las series de tiempo,
#' Anual=1
#' Trimestral=4
#' Mensual=12
#' @param cor_col Booleano, si es TRUE realiza la corrección de colas ya sea usando nuevos datos o usando el ultimo valor como constante, si es FALSE usa solo los datos ingresados.
#' @param dat_cor Es un data frame con datos nuevos para realizar la correccion de colas, si es NULL se toma el ultimo valor como una constante.
#' @param c Es un entero el cual se usa cuando dat_cor=NULL y es el numero de periodos extra donde el ultimo valor de datos sera constante
#' @param nlag Es un entero y corresponde al numero de rezagos a tener en cuenta.
#'
#' @return Una lista que contiene un dataframe por cada variable, en dicho data frame esta la fecha, el valor observado y el suavizamiento por el filtro de Beveridge-Nelson
#' @export Filtro_BN
#'
#' @examples

DashFiltrosest_Filtro_BN_v1.0<-function(datos, variables=names(datos)[-1],inicio,freq=4,cor_col=FALSE,dat_cor=NULL, c=freq, nlag=8){
  # Manejar datos.
  library(dplyr)
  library(tidyverse)
  library(readxl) # Abrir excel
  library(zoo); library(lubridate) # Fechas
  library(reshape2)
  library(writexl)
  library(readxl)
  library(lubridate)
  # library(plyr)

  # Graficas
  library(ggplot2)
  library(gridExtra)
  library(coefplot)


  # Series de tiempo
  library(forecast)
  library(mFilter)
  library(seasonal)
  library(fpp)

  # Test
  library(lmtest)
  library(tseries)
  library(stats)
  library(fBasics)
  library(neverhpfilter)

  ######Corrección de Colas de los datos #####
  if(cor_col==TRUE){
    if(is.null(dat_cor)==TRUE){
      if(freq==1){f=12}# anual
      if(freq==4){f=3}# trimestral
      if(freq==12){f=1}#mensual


      for (i in 1:c) {
        add<-cbind(max(datos[,1])%m+%months(f),tail(datos,1)[-1])
        names(add)<-names(datos)
        datos<-rbind(datos,add)
      }
    }else{
      datos<-rbind(datos,dat_cor)
    }
  }

  ######Funcion para el filtro Beveridge-Nelson#########




  bnd  <- function(data,nlag){

    y=matrix(data,ncol=1)
    #nlag=8

    yd=diff(y,lag=1)
    yl=matrix(rep(0,length(y)*nlag),ncol=nlag)
    yl[,1] = c(0,yd)
    for (i in 2:nlag){
      yl[,i] = c(0,yl[1:(length(y)-1),i-1])
    }
    x=yl[(nlag+1):(length(y)-1),1:nlag]
    yy=matrix(yd[(1+nlag):length(yd),],ncol=1)

    # OLS
    beta <- lm(yy ~ x)$coefficients

    # Companion form of matrix
    eye=diag(1,nlag)
    coef.tmp=matrix(beta[2:length(beta)],nrow=1)
    betac.tmp=rbind(coef.tmp,eye)
    betac=betac.tmp[1:nlag,]


    c1=betac %*% solve(diag(nlag)-betac)
    ydd=c(rep(0,1+nlag),yd)-beta[1]
    ydd.len=length(ydd)

    # Construct matrix of historical lags
    yD = matrix(rep(0,nlag*(ydd.len-nlag)),nrow=nlag)

    for (i in 1:nlag){
      yD[i,]=matrix(ydd[(1+i):(ydd.len-(nlag)+i)],nrow=1)
    }

    yD.tmp=apply(yD,2,rev)
    yD=yD.tmp[nrow(yD.tmp):1,]

    # Selection vector
    sel=rep(0,nlag)
    sel[1]=1

    # Compute trend and cycle
    ytr=y+t(sel%*%c1%*%yD)
    yc=t(sel%*%c1%*%yD)
    out=cbind(ytr,yc)
    colnames(out) = c("trend","cycle")
    out=out
  }

  ex_BN<-vector("list",length = length(variables))
  names(ex_BN)<-variables
  for (i in variables) {
    data<-datos[c("Fecha",i)]



    ####################Filtro Beveridge-Nelson#######################


    dta.ts<-ts(data[i],start = inicio,frequency = freq)



    data_bn<-bnd(dta.ts,nlag=nlag)[,"trend"] #Valores suavizados por el filtro Beveridge-Nelson


    data<-cbind(data,as.data.frame(data_bn))

    colnames(data)[3]<-"Filtro Beveridge-Nelson"

    ex_BN[[i]]<-data
    
    print('--Filtro de BN se estimo de forma correcta--')

  }
  return(ex_BN)
}


#' datos<-as.data.frame(read_excel("C:/Users/usr_practicantegt52/OneDrive - Banco de la República/Desktop/PRACTICA SEGUNDO SEMESTRE 2023/MARLON/FILTROS TCRE/Datos/202308_Filtros 1970.xlsx",range = "A1:B215"))
#' inicio=c(1970,1)
#' freq=4
#' variables<-names(datos)[-1]
#' cor_col<-TRUE
#' dat_cor<-as.data.frame(read_excel("C:/Users/usr_practicantegt52/OneDrive - Banco de la República/Desktop/PRACTICA SEGUNDO SEMESTRE 2023/MARLON/FILTROS TCRE/Datos/202308_Filtros 1970.xlsx",range = "A216:B223",col_names = FALSE))
#'
#' Filtro_BN(datos = datos,
#'           variables=variables,
#'           inicio = inicio,
#'           freq = freq,
#'           cor_col=cor_col,
#'           dat_cor=dat_cor,
#'           c=NULL,
#'           nlag=8)
