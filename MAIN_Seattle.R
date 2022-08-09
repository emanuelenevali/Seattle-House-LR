source("/Users/matteom/Library/CloudStorage/OneDrive-PolitecnicodiMilano/Progetto statistica/Funzioni.R")
case_originale = importa_case()
librerie()
case = selezione_covariate(case_originale)
case=case[which(case$location=='Seattle'),]
#case=case[which(case$anni<60),]
case$location=NULL
head(case)



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
case_epurate=case[which(osservazioni_sane==1),]
G=lm(price~.,case_epurate)

controllo_omoschedasticita(G)
controllo_normalita(G)
shapiro.test(residuals(G))

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
lambda=1/3


# Nuovo modello di regressione
{
  # Radice terza
  case$radprezzo=((case$price)^(lambda)-1)/lambda
  case$price=NULL
  g2 = lm( radprezzo ~ ., data = subset(case,osservazioni_sane==1) )
  summary(g2)
}
case_epurate=case[which(osservazioni_sane==1),]

abs((g2$coefficients-g$coefficients)/g$coefficients)
plot(g2, which=1 )
shapiro2(case[which(osservazioni_sane==1),],G)
shapiro.test(residuals(g2))

qqnorm( g2$res, ylab = "Raw Residuals", pch = 16 )
qqline( g2$res )


# g4
case_epurate$bathrooms=NULL
g4=lm(radprezzo~.,case_epurate)
summary(g4)
plot(g4, which=1 )
qqnorm( g4$res, ylab = "Raw Residuals", pch = 16 )
qqline( g4$res )
vif(g4)

# Corss Validation
{
# Suddivisione Training set vs testset
n=dim(model.matrix(G))[1];
MSE=0;
MAE=0;
MAPE=0;
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
#MSE=c(MSE,sum((y-y_hat)^2)/(floor(0.2*n)))
MAE=c(MAE,sum(abs(y-y_hat))/(floor(0.2*n)))
MAPE=c(MAPE,(sum(abs(y-y_hat)/y)/n)*100)
}
# MSE=MSE[seq(2,41)]
MAE=MAE[seq(2,41)]
MAPE=MAPE[seq(2,41)]
# Average_MSE=mean(MSE)
Average_MAE=mean(MAE)
# print(sqrt(Average_MSE))
print(Average_MAE)
}
min((case_epurate$radprezzo*lambda+1)^(1/lambda))
max((case_epurate$radprezzo*lambda+1)^(1/lambda))
Media_prezzi=mean((case_epurate$radprezzo*lambda+1)^(1/lambda))
Average_MAE/Media_prezzi*100
boxplot(MAE)
sd(MAE)


# Tabella Bedrooms-Sqft_living
case_epurate$bedrooms=as.factor(case_epurate$bedrooms)
levels(case_epurate$bedrooms)
#case_epurate$sqft_living=cut.default(case_epurate$sqft_living, 5, labels = NULL, include.lowest = FALSE, right = TRUE, dig.lab = 3)
case_epurate$sqft_living=cut_number(case_epurate$sqft_living,n=6)
levels(case_epurate$sqft_living)
case_epurate$price=(case_epurate$radprezzo*lambda+1)^(1/lambda)

Price_matrix=matrix( data = 0:0, nrow = 6, ncol =6, byrow = FALSE )
n=1;
k=1;
for(i in levels(case_epurate$bedrooms)){
  for(j in levels(case_epurate$sqft_living)){
    Price_matrix[n,k]=mean(case_epurate$price[which(case_epurate$bedrooms==i & case_epurate$sqft_living==j)])
    if(k==6){
      k=1
    }else{
      k=k+1;
    }
  }
  n=n+1
} 
Price_matrix


# Barplot Prezzo_anni
case_epurate$price=(case_epurate$radprezzo*lambda+1)^(1/lambda)
case_epurate$anni=cut_number(case_epurate$anni,n=15)
l_anni=levels(case_epurate$anni)
l_anni
Media_prezzo_anni=seq(0,0,length.out=length(l_anni));
n=1
for(i in l_anni){
  Media_prezzo_anni[n]=mean(case_epurate$price[which(case_epurate$anni==i)])
  n=n+1
}
Media_prezzo_anni
barplot(Media_prezzo_anni)




# Istogramma prezzi case_epurate
hist(case_epurate$price)
hist(case_epurate$price, breaks = sqrt( length( case_epurate$price ) ), probability = TRUE,main = 'Distribuzione dei prezzi') 



