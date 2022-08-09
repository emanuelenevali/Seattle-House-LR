source("/Users/matteom/Library/CloudStorage/OneDrive-PolitecnicodiMilano/Progetto statistica/Funzioni.R")
case_originale = importa_case()
librerie()
case = selezione_covariate(case_originale)
case=case[which(case$anni<60),]
case$floors=NULL
case$bathrooms=NULL
case$anni=NULL
# case$anni=NULL
#case_con_zipcode_per_localita = modifica_covariate_zipcode_localita(case_originale,nrow(case_originale))
#case_con_zipcode_a_zone=modifica_covariate_zipcode_zone(case_originale,nrow(case_originale))
head(case)
#head(case_con_zipcode_per_localita)
#head(case_con_zipcode_a_zone)


# Prima overview dei dati
ggpairs(data = case, title ="Relationships between predictors & response",
        lower = list(continuous=wrap("points", alpha = 0.5, size=0.1)))
hist( case$price, breaks = sqrt( length( case$price ) ), probability = TRUE,main = 'Distribuzione dei prezzi') 

# Primo modello di regressione, non Omoschedastico
g = lm( price ~ ., data = case )
summary(g)

controllo_omoschedasticita(g)
controllo_normalita(g)

# Leverages
osservazioni_sane=leverages(case,g)
# Residui standardizzati
osservazioni_sane=standardized_residuals(case,g,osservazioni_sane)
# Residui Studentizzati
osservazioni_sane=studentized_residuals(case,g,osservazioni_sane)
# Distanza di Cook
osservazioni_sane=distanza_di_Cook(case,g,osservazioni_sane)


# Nuovo Modello di regressione
{
case$logic=osservazioni_sane
G=lm(price~bedrooms+sqft_living+view+grade,case,subset=(case$logic==1))
case$logic=NULL
rm(list= ls()[!(ls() %in% c('case','case_originale','G','g','osservazioni_sane'))])
source("/Users/matteom/Library/CloudStorage/OneDrive-PolitecnicodiMilano/Progetto statistica/Funzioni.R")
librerie
summary(G)
}

controllo_omoschedasticita(G)
controllo_normalita(G)

# DIfferenza % beta fra vecchio e nuovo modello
{
abs((G$coefficients-g$coefficients)/g$coefficients)
}




# BOX COX
b = boxcox(price ~ ., data = subset(case,osservazioni_sane==1))
names(b)
best_lambda_ind = which.max( b$y )
best_lambda = b$x[ best_lambda_ind ]
best_lambda
lambda=1/4



# Nuovo modello di regressione
{
  # Radice terza
  case$radprezzo=((case$price)^(lambda)-1)/lambda
  case$price=NULL
  g2 = lm( radprezzo ~ ., data = subset(case,osservazioni_sane==1) )
  summary(g)
}
# Nonostante tutto le bedrooms rimangono negative

abs((g2$coefficients-g$coefficients)/g$coefficients)


# Controllo omoschedasticità      SUPERATO
{
  plot( g$fit, g$res, xlab = "Fitted", ylab = "Residuals", 
        main = "Residuals vs Fitted Values", pch = 16 )
  abline( h = 0, lwd = 2, lty = 2, col = 'red' ) 
  
  plot(g, which=1 )
  
  n=dim(model.matrix(g))[1];
  for(i in 1:4){
    s=sample(1:n)
    shapiro_set_ids=s[seq(1,500)]
    case_shapiro=case[shapiro_set_ids,]
    g_shapiro=lm(radprezzo~.,case_shapiro) 
    print(shapiro.test( residuals( g_shapiro ) ) )
  }
  

  
}
# Controllo normalità       SUPERATO
{
  qqnorm( g$res, ylab = "Raw Residuals", pch = 16 )
  qqline( g$res )
}

# Corss Validation
{

# Suddivisione Training set vs testset
case_epurate=case[which(osservazioni_sane==1),]
n=dim(model.matrix(G))[1];
MSE=0;
for(i in 1:40){
s=sample(seq(1,n))
training_set_ids=s[seq(1,ceiling(0.8*n))]
test_set_ids=s[seq(ceiling(0.8*n)+1,n)]
# Cross validation
case_training=case_epurate[training_set_ids,]
case_test=case_epurate[test_set_ids,]
g_training=lm(radprezzo~.,case_training) 
beta_training=g_training$coefficients
g_test=lm(radprezzo~.,case_test)
Z=model.matrix(g_test)

y_hat=Z%*%beta_training
y_hat=(y_hat*lambda+1)^(1/lambda)
y=case_test$radprezzo
y=(y*lambda+1)^(1/lambda)
MSE=c(MSE,sum((y-y_hat)^2)/(floor(0.2*n)))
}
MSE=MSE[seq(2,41)]
Average_MSE=mean(MSE)
print(sqrt(Average_MSE))
}

# Fare con Predict
{
  case_epurate=case[which(osservazioni_sane==1),]
  n=dim(model.matrix(G))[1];
  for(i in 1:40){
    s=sample(seq(1,n))
    training_set_ids=s[seq(1,ceiling(0.8*n))]
    test_set_ids=s[seq(ceiling(0.8*n)+1,n)]
    # Cross validation
    case_training=case_epurate[training_set_ids,]
    case_test=case_epurate[test_set_ids,]
    #y_ex=case_test$radprezzo
    #y_ex=(y_ex*lambda+1)^(1/lambda)
    y.pred=predict(g,case_test,interval="confidence",se=T)
    y.pred$fit
    mean(y.pred$se)
    y.pred2=predict(g,case_test,interval='prediction',se=T)
    y.pred2$fit
    mean(y.pred2$se)
    
    matplot( y_ex, y.pred2$fit, lty = c( 1, 2, 2 ), col = c( 1, 2, 2 ), type = "l", xlab = "altezza", ylab = "peso", 
             main = "IC per la media e IP per singole osservazioni" )
    lines( y_ex, y.pred$fit[ , 2 ] , col = "blue", lty = 2, xlab = "altezza", ylab = "peso" )
    lines( y_ex, y.pred$fit[ , 3 ] , col = "blue", lty = 2, xlab = "altezza", ylab = "peso" )
    #points( altezza, peso, col = "black", pch = 16 )
  }
 
}






# VIF          Ellisse da lasciar stare
{
vif(G)
# Test ellise su bagni e sqft_living
plot( ellipse( G, c( 2, 3 ) ), type = "l", xlim = c( -0.05, 0.001 ),ylim=c(-0.01,.12) )
points( 0, 0 )
}








# Media Prezzi a Zone
case_epurate=case_originale[which(osservazioni_sane==1),]
case_con_zipcode_a_zone=modifica_covariate_zipcode_zone(case_epurate,nrow(case_epurate))
media_prezzi_a_zona=prezzi_a_zone(case_con_zipcode_a_zone,length(levels(case_con_zipcode_a_zone$location)),levels(case_con_zipcode_a_zone$location))
# Fare con tapply(sjdnjsdbas, unique(jsndjsb))
media_prezzi_a_zona

case_con_zipcode_a_zone=subset(case_con_zipcode_a_zone,!case_con_zipcode_a_zone$location=='Vashon Island')
case_con_zipcode_a_zone$location=droplevels(case_con_zipcode_a_zone$location)

boxplot(price~location,data=case_con_zipcode_a_zone)

# ----------------

Punti_leva=seq(0,0,length.out(nrow(case)))
Punti:leva[whatchout.ids-lev]=1;


# Istogramma LOGARITMO dei prezzi senza leverages
osservazioni_sane=seq( 1, 1, length.out = nrow(case) )
osservazioni_sane[watchout_ids_lev]=0
#case$logprezzi=log(case$price)
hist( log(case$price[which(osservazioni_sane==1)]), breaks = sqrt( length( case$price ) ), probability = TRUE,main = 'Distribuzione dei prezzi')
case$radprezzo2=case$radprezzo[which(osservazioni_sane==1)]





# ---------

# relevel
# Usare riferimento logico per categoria