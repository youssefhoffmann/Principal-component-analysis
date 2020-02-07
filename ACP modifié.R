donnees1<-read.csv("crime_data_pca.csv", sep=";")
#On crée la fonction ACP avec comme paramètres, le choix de la méthode (centrée ou normée) et le nombre
#de dimension(s) retenue(s) :

ACP <- function(methode, nbdim){
  boxplot(donnees1,main="Première visualisation des données")
  if (methode == 0) #méthode 0 = données centrées
    d<-scale(donnees1, center = TRUE, scale = FALSE)
  if (methode == 1)
    d<-scale(donnees1) #méthode 1 = données normées
  MatCov<-cov(d, y = d, use = "everything", method = c("pearson", "kendall", "spearman"))
  r<-eigen(MatCov)
  VecteursPropres<-r$vectors
  ValeursPropres<-as.vector(r$values)
  barplot(ValeursPropres/sum(ValeursPropres),main="Poids de chaque valeur propre dans le spectre",
          xlab="Valeur Propre i",
          ylab="Valeur Propre i/Somme des valeurs propres")
  TauxInertie=NULL
  TauxInertie[1]<-ValeursPropres[1]/sum(ValeursPropres)
  for (i in 2:14){
    TauxInertie[i]<-TauxInertie[i-1]+ValeursPropres[i]/sum(ValeursPropres)
  }
  plot(0:14,c(0,TauxInertie[1:14]),xlim=c(0,14),ylim=c(0,1),
       type="b",main="Taux d'inertie expliquée selon le nombre de valeurs propres retenues",
       xlab="Nombre de valeurs propres retenues",
       ylab="Taux d'inertie expliquée")
  donnees3<-t(VecteursPropres%*%t(d))
  boxplot(donnees3,main="Visualisation des données projetées", xlab="Axes de projections")
  abline(v= 0.5 + nbdim, col="red")
  #A gauche de la droite verticale rouge se trouvent les composantes dans l'espace de dimension,
  #le nombre de dimension retenues, et �  droite, le reste des composantes dans l'espace de dimension 14.
  Q<-(rowSums(donnees3[,1:nbdim]^2)/rowSums(donnees3[,1:14]^2))
  plot(Q,main = "Qualité de projection des individus dans        1
         l'espace de projection de dimension 2", xlab = "Individus")
  Y=matrix(data = NA,nrow = 47,ncol = 14)
  for (j in 1:14){
    Y[,j]<-(1/47)*(donnees3[,j]^2)/(ValeursPropres[j])
  }
  plot(1:47,Y[1:47,1],xlab="Individus",ylab="Contribution de l'individu sur l'axe principal",
       main="Contribution des individus sur l'axe principal")
  R=matrix(data = NA,nrow = 14,ncol = 14)
  if (methode==1){
    for (i in 1:14){
      R[i,]<-sqrt(ValeursPropres[i])*VecteursPropres[i,]
    }
  }
  if (methode==0){
    for (i in 1:14){
      R[i,]<-sqrt(ValeursPropres[i]/MatCov[i,i])*VecteursPropres[i,]
    }
  }
  plot(R[1,],R[2,],xlab="Composantes c",ylab="Variables initiales j",
       main="Correlation linéaire entre une composante c et une variable j")
  return(donnees3[,1:nbdim])
}

