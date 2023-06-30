#' EvaluaciÓn Y Determinación de la  Condición Corporal en Bovinos.
#'
#' Realiza la evaluación y determina la Composicion Corporal en Bovinos (TA Y Masa Muscular)
#'
#' @param PESO1 (vector) variable que representa el peso inicial
#' @param PDESV (vector) nombre de la variable que represesta el peso desvastado.
#' @param SEXO (vector) variable idependiente que representa el sexo del animal.
#' @param FSEXO (vector) variable idependiente que representa el Factor del sexo del animal,
#' es decir sies Hembra(0.8), Macho Entero (1.2) y Macho Castrado(1).
#' @param ALTURA (vector) variable independiente que representa el frame y/o altura
#' @param GD (vector) Variable independiente que representa la ganancia diaria
#'  de peso.
#' @param PESOF (vector ) variable que representa el peso final de la engorda.
#' @param  DAL (vector) variable que hace referencia a los dias de engorda.
#' @return Delvuelve una tabla con el promedio, desviacion estandar, minimo y
#' maximo de la Evaluacion de la Res, Tejido Adiposo(TA), Masa Muscular, Huesos,
#' Falacias, Tendones(HFT); tambien devuelve un grafico de kg predichos vs kg observados
#'  de Tejido Adiposo y Masa Muscular.
#'  @export
#'
#'  @examples
#'  \dontrun(
#'  # DIRECTORIO DE TRABAJO
#'RUTA <- "C:/Users/yasuv/OneDrive/Documentos/PAQ  R/BD1.csv"
#'
#' #--------------------------------------------------------------
#' #EJEMPLO 1
#' #DATOS <- read.csv(RUTA)
#' # CARGO DE LIBRERIA
#' library(CCOMBOY)
#' CCBOVin(PESO1=DATOS$PESO1,PDESV=DATOS$PDESV,SEXO=DATOS$SEXO,ALTURA=DATOS$ALTURA,
#'PESOF=DATOS$PESO.FIN, GD=DATOS$GD, DAL=DATOS$DAL, Fsex=DATOS$F.SEXO)
#' #--------------------------------------------------------------
#'  #EJEMPLO 2
#'  PESO1	<-c(250,	140,	115,	154,	193,	120,	153,	178,	117,	129,	187,	165,	209,	150,	277,	315,	354,	395)
#' SEXO<- c("Macho",	"Hembra", "Hembra","Hembra","Hembra","Hembra","Hembra","Hembra","Hembra","Hembra","Hembra","Hembra","Hembra","Hembra","Macho","Machoc","Machoc","Machoc")
#' Fsex <- c(1.2,.8,.8,.8,.8,.8,.8,.8,.8,.8,.8,.8,.8,.8,1.2,1,1,1)
#' PDESV	<-c(145,130,112,150,190,110,150,173,112,127,180,160,204,147,274,310,350,390)
#' ALTURA  <-c(6,6,6,6,6,6,5,5,5,5,4,9,9,9,9,9,7,7)
#' PESOF	<- c(550,560,600,580,597,509,530,590,542,529,603,587,537,568,1050,950,940,970)
#' GD	<- c(1.5,1.6,1.7,1.4,1.3,1.1,1,1.2,1.5,1.9,1.1,1.3,1.2,1.4,1.2,1.3,1.6,1.7)
#' DA	<- c(135,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135)
#'
#' CCBOVin(PESO1=PESO1, PDESV=PDESV, SEXO=SEXO, ALTURA=ALTURA, PESOF=PESOF, GD=GD, DAL=DA, Fsex=Fsex)
#'  )
 CCBOVin <-function (PESO1,PDESV, SEXO, ALTURA, PESOF, GD, DAL,Fsex){
  AG  <- 0.725
  PRO <- 0.225
  CEN <- 0.050

  TE <- (367 + ALTURA )* 33

  Peq <- ((PESO1 * 478)/(TE))
  print(Peq)
  Pvac <- PDESV* 0.891

  CC <- 90
  PvacEq <- ((PESO1 * 478)/(TE * Fsex))* 0.891
  GrEqKg <- (0.00013 * (PvacEq ^2) + 0.159 * PvacEq - 10.76) * (1 + ((CC - 5) * 0.16))

  GrVacEq <- (GrEqKg / PvacEq) * 100

  FGpesoinicial <-  PESO1 * GrVacEq

  FLGpesoinicial <- PESO1 - FGpesoinicial
  Ag1 <- FLGpesoinicial * AG
  Pro1 <-FLGpesoinicial * PRO
  Cen1  <- FLGpesoinicial * CEN
  FLG1<-sum((Ag1+Pro1)+Cen1)


  TAEn<- (0.122*(1.25-146))*(1+((GD-1)*0.119))

  FGganancia <- ((GD * 0.891) * (TAEn))

  print(FGganancia)
  FLGganancia <- ((GD * 0.891) * (FGganancia))
  FGPFin  <-  ((FGpesoinicial + FGganancia) * DAL)
  FGPfin8  <- (FGPFin / PESOF) / 100
  FLGPfin <-  (PESOF - FGPFin)
  FGres <- ((FGPFin  * 1.0815) + 0.7)
  FGresk <- PESOF * FGres
  FLGRes <- PESOF - FGresk
  TApR  <-  ((0.982 * FGres )+ 0.986)

  TAPPf <- ((1.062 + FGPfin8 ) / 2.361)

  TAKg <-  PESOF * TApR

  MaMusk <- (PESOF - TAKg) * 0.790 - 5.780
  Res_sinTA_Huesosk <-(PESOF - TAKg) * 0.164 + 8.164
  Rc1 <- 0.92
  Res_Hk <- ((PESOF * 0.101) + 14.39)
  Rc2<- 0.88
  Fas_y_Tenk <-((PESOF * 0.031)- 0.201)
  ppv <- mean(PESO1)
  Desvesp <- sd(PESO1)
  minp <- min(PESO1)
  maxp <- max (PESO1)

  ERP <- mean(PESOF)
  DVSR <- sd(PESOF)
  MinR <- min(PESOF)
  MaxR <- max(PESOF)
  P <- mean(TAKg)
  dv <- sd(TAKg)
  Mint <- min(TAKg)
  maxt <- max(TAKg)

  pm <- mean(MaMusk)
  dvm <- sd(MaMusk)
  minm <- min(MaMusk)
  maxm <- max(MaMusk)

  a <- Res_Hk
  b <- Fas_y_Tenk
  HFT <- sum(a + b)
  print(HFT)
  PH <- mean(HFT)
  Dvdh <- sd(HFT)
  minh <- min(HFT)
  maxh <- max(HFT)

  T1 <- data.frame( na = c("PROMEDIO", "DES.EST", "MIN", "MAX"),
                    PVivo = c(ppv, Desvesp,minp,maxp),
                    Eva_Res = c(ERP,DVSR,MinR,MaxR),
                    Tej_AdiposoKgs =c(P,dv,Mint,maxt),
                    Musculo_Kgs = c(pm,dvm,minm,maxm),
                    HFT_Kgs =c(PH,Dvdh,minh,maxh))
  print(T1)

  #grafico de regresion
  X <- TAPPf
  Y <- PESOF

  gs <- plot(X,Y,
             main = "REGRESION DE KG TejidoA EN RES",
             xlab = "Pre", ylab = "Obs")
  abline(lm(Y~X),col= "red", lwd=4)

  H <- MaMusk
  E <- PESOF

  g2 <-plot(H,E,
            main = "REGRESION DE KG Masa Muscular EN RES",
            Xlab = "Pre", ylab = "Obs")
  abline(lm(E~H),col= "black", ldw=3)

}
