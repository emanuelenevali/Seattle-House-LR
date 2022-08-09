# 1
importa_case = function (){
  case =read.csv(file='kc_house_data.csv', header = TRUE, sep = ",", quote = "\"",
                 dec = ".", fill = TRUE, comment.char = "")
  return(case)
}

# 2
librerie= function(){
  library( car )
  library( ellipse )
  library( leaps )
  library(MASS)
  library( GGally)
  library(rgl)
  return()
}

# 3
selezione_covariate=function(case){
  case$date=NULL
  case$sqft_lot15=NULL
  case$sqft_living15=NULL
  case$floors=NULL
  case$long=NULL
  case$lat=NULL
  case$id=NULL
  case$sqft_lot=NULL
  case$condition=NULL
  case$sqft_basement=NULL
  case$sqft_above=NULL
  case$waterfront=NULL
  for(i in 1:nrow(case)) {
    case$anni[i]=2015-max(case$yr_built[i],case$yr_renovated[i])
  }
  case$yr_built=NULL
  case$yr_renovated=NULL
  
  ZIP=as.factor(case$zipcode)
  lZIP=levels(ZIP)
  case$location='bla'
  for (k in 1:dim(case)[1])
  {
    if (any(c(98033,98039,98004,98040,98006)/case$zipcode[k]==1))
    {
      case$location[k]='Lungolago'
    }
    else if (any(c(98125,98117,98107,98103,98115,98105,98199,98119,98109,98102,98112,98121,98101,98104,98122,98134,98144,98116,98136,98126)/case$zipcode[k]==1))
    {
      case$location[k]='Seattle'
    }else{
      case$location[k]='Provincia'
    }
  }
  case$location=as.factor(case$location)
  case$zipcode=NULL
  return(case)
}

# 4
modifica_covariate_zipcode_localita=function(case,numerosita){
  case$date=NULL
  case$sqft_lot15=NULL
  case$sqft_living15=NULL
  case$long=NULL
  case$lat=NULL
  case$id=NULL
  case$sqft_lot=NULL
  case$condition=NULL
  case$sqft_basement=NULL
  case$sqft_above=NULL
  case$waterfront=NULL
  for(i in 1:numerosita) {
    case$anni[i]=2015-max(case$yr_built[i],case$yr_renovated[i])
  }
  case$yr_built=NULL
  case$yr_renovated=NULL
  ZIP=as.factor(case$zipcode)
  lZIP=levels(ZIP)
  case$location=''
  for (k in 1:dim(case)[1])
  {
    if (any(c(98001)/case$zipcode[k]==1))
    {
      case$location[k]='Algona'
    }
    if (any(c(98002,98003,98023,98092)/case$zipcode[k]==1))
    {
      case$location[k]='Auburn'
    }
    if (any(c(98004,98005,98006,98007,98008)/case$zipcode[k]==1))
    {
      case$location[k]='Bellevue'
    }
    if (any(c(98010)/case$zipcode[k]==1))
    {
      case$location[k]='Black Diamond'
    }
    if (any(c(98011)/case$zipcode[k]==1))
    {
      case$location[k]='Bothel'
    }
    if (any(c(98014)/case$zipcode[k]==1))
    {
      case$location[k]='Carnation'
    }
    if (any(c(98019)/case$zipcode[k]==1))
    {
      case$location[k]='Duvall'
    }
    if (any(c(98022)/case$zipcode[k]==1))
    {
      case$location[k]='Enumclaw'
    }
    if (any(c(98024)/case$zipcode[k]==1))
    {
      case$location[k]='Fall City'
    }
    if (any(c(98027,98029)/case$zipcode[k]==1))
    {
      case$location[k]='Issaquah'
    }
    if (any(c(98028)/case$zipcode[k]==1))
    {
      case$location[k]='Kenmore'
    }
    if (any(c(98030,98031,98032)/case$zipcode[k]==1))
    {
      case$location[k]='Kent'
    }
    if (any(c(98033,98034)/case$zipcode[k]==1))
    {
      case$location[k]='Kirkland'
    }
    if (any(c(98038)/case$zipcode[k]==1))
    {
      case$location[k]='Maple Valley'
    }
    if (any(c(98039)/case$zipcode[k]==1))
    {
      case$location[k]='Medina'
    }
    if (any(c(98040)/case$zipcode[k]==1))
    {
      case$location[k]='Marcer Island'
    }
    if (any(c(98042)/case$zipcode[k]==1))
    {
      case$location[k]='Covington'
    }
    if (any(c(98045)/case$zipcode[k]==1))
    {
      case$location[k]='North Bend'
    }
    if (any(c(98052,98053)/case$zipcode[k]==1))
    {
      case$location[k]='Redmond'
    }
    if (any(c(98055,98058)/case$zipcode[k]==1))
    {
      case$location[k]='Renton'
    }
    if (any(c(98056,98059)/case$zipcode[k]==1))
    {
      case$location[k]='Newcastle'
    }
    if (any(c(98065)/case$zipcode[k]==1))
    {
      case$location[k]='Snoqualmie'
    }
    if (any(c(98070)/case$zipcode[k]==1))
    {
      case$location[k]='Vashon'
    }
    if (any(c(98072,98077)/case$zipcode[k]==1))
    {
      case$location[k]='Woodinville'
    }
    if (any(c(98074,98075)/case$zipcode[k]==1))
    {
      case$location[k]='Sammamish'
    }
    if (any(c(98102,98103,98105,98106,98107,98108,98109,98112,98115,98116,98117,98118,98119,98122,98125,98126,98133,98136,98144,98146,98155,98168,98177,98178,98188,98198,98199)/case$zipcode[k]==1))
    {
      case$location[k]='Seattle'
    }
    if (any(c(98148,98166)/case$zipcode[k]==1))
    {
      case$location[k]='Normandy Park'
    }
  }
  case$location=as.factor(case$location)
  return(case)
}

# 5.1
controllo_omoschedasticita=function(g){
  plot( g$fit, g$res, xlab = "Fitted", ylab = "Residuals", 
        main = "Residuals vs Fitted Values", pch = 16 )
  abline( h = 0, lwd = 2, lty = 2, col = 'red' ) 
  plot(g, which=1 )
}
# 5.2
controllo_normalita=function(g){
  qqnorm( g$res, ylab = "Raw Residuals", pch = 16 )
  qqline( g$res )
}
# 5.3
shapiro1=function(case,g){
  n=dim(model.matrix(g))[1];
  for(i in 1:4){
    s=sample(1:n)
    shapiro_set_ids=s[seq(1,500)]
    case_shapiro=case[shapiro_set_ids,]
    g_shapiro=lm(price~.,case_shapiro) 
    print(shapiro.test( residuals( g_shapiro ) ) )
  }
}
# 5.4
shapiro2=function(case,g){
  n=dim(model.matrix(g))[1];
  for(i in 1:4){
    s=sample(1:n)
    shapiro_set_ids=s[seq(1,500)]
    case_shapiro=case[shapiro_set_ids,]
    g_shapiro=lm(radprezzo~.,case_shapiro) 
    print(shapiro.test( residuals( g_shapiro ) ) )
  }
}

# 5.5
modello_logaritmo=function(case){
  case$logprezzo=log(case$price)
  case$price=NULL
  g = lm( logprezzo ~ ., data = case )
  return(g)
}

# 6
modifica_covariate_zipcode_zone=function(case,numerosita){
  case$date=NULL
  case$sqft_lot15=NULL
  case$sqft_living15=NULL
  case$long=NULL
  case$lat=NULL
  case$id=NULL
  case$sqft_lot=NULL
  case$condition=NULL
  case$sqft_basement=NULL
  case$sqft_above=NULL
  case$waterfront=NULL
  for(i in 1:numerosita) {
    case$anni[i]=2015-max(case$yr_built[i],case$yr_renovated[i])
  }
  case$yr_built=NULL
  case$yr_renovated=NULL
  ZIP=as.factor(case$zipcode)
  lZIP=levels(ZIP)
  case$location='bla'
  for (k in 1:dim(case)[1])
  {
    if (any(c(98177,98133,98125,98155,98028,98034,98033)/case$zipcode[k]==1))
    {
      case$location[k]='Lago Nord'
    }
    if (any(c(98011,98072,98052,98074,98075,98029,98027)/case$zipcode[k]==1))
    {
      case$location[k]='Fascia orientale nord'
    }
    if (any(c(98038,98051,98010,98022)/case$zipcode[k]==1))
    {
      case$location[k]='Fascia orientale sud'
    }
    if (any(c(98077,98053,98014,98019,98024,98065,98045)/case$zipcode[k]==1))
    {
      case$location[k]='Provincia Nord'
    }
    if (any(c(98023,98003,98001,98002,98092)/case$zipcode[k]==1))
    {
      case$location[k]='Auburn e limitrofi'
    }
    if (any(c(98030,98031,98032,98041,98042)/case$zipcode[k]==1))
    {
      case$location[k]='Kent e limitrofi'
    }
    if (any(c(98055,98056,98057,98058,98059)/case$zipcode[k]==1))
    {
      case$location[k]='Renton e Newcastle'
    }
    if (any(c(98004,98005,98006,98007,98008,98039,98040)/case$zipcode[k]==1))
    {
      case$location[k]='Bellevue e limitrofi'
    }
    if (any(c(98033,98034,98028,98125,98155,98133,98177)/case$zipcode[k]==1))
    {
      case$location[k]='Lago Nord'
    }
    if (any(c(98117,98107,98103,98115,98105,98199,98119,98109,98102,98112,98121,98101,98104,98122,98134,98144,98116,98136,98126,98106,98108,98118)/case$zipcode[k]==1))
    {
      case$location[k]='Seattle'
    }
    if (any(c(98146,98168,98178,98166,98148,98158,98188,98198)/case$zipcode[k]==1))
    {
      case$location[k]='Penisola sud'
    }
    if (any(c(98070)/case$zipcode[k]==1))
    {
      case$location[k]='Vashon Island'
    }
    if(case$location[k]=='bla'){
      print(case$zipcode[k])
    }
  }
  case$location=as.factor(case$location)
  return(case)
}
  
# 7
prezzi_a_zone = function(case,numero_zone,livelli) {
  media_prezzi_a_zone=seq(0,0,length.out=numero_zone)
  sd_prezzi_a_zone=seq(0,0,length.out=numero_zone)
  for(k in 1:numero_zone){
    s=subset(case,case$location==livelli[k],1)
    media_prezzi_a_zone[k]=mean(s$price)
    sd_prezzi_a_zone[k]=sd(s$price)
  }
  print('Media')
  print(media_prezzi_a_zone)
  print('Deviazione standard')
  print(sd_prezzi_a_zone)
  return(media_prezzi_a_zone)
}
  
# 8
tre_zone=function(case,numerosita){
  case$date=NULL
  case$sqft_lot15=NULL
  case$sqft_living15=NULL
  case$long=NULL
  case$lat=NULL
  case$id=NULL
  case$sqft_lot=NULL
  case$condition=NULL
  case$sqft_basement=NULL
  case$sqft_above=NULL
  case$waterfront=NULL
  for(i in 1:numerosita) {
    case$anni[i]=2015-max(case$yr_built[i],case$yr_renovated[i])
  }
  case$yr_built=NULL
  case$yr_renovated=NULL
  ZIP=as.factor(case$zipcode)
  lZIP=levels(ZIP)
  case$location='bla'
  for (k in 1:dim(case)[1])
  {
    if (any(c(98033,98039,98004,98040,98006)/case$zipcode[k]==1))
    {
      case$location[k]='Lungolago'
    }
    else if (any(c(98125,98117,98107,98103,98115,98105,98199,98119,98109,98102,98112,98121,98101,98104,98122,98134,98144,98116,98136,98126)/case$zipcode[k]==1))
    {
      case$location[k]='Seattle'
    }else{
      case$location[k]='Provincia'
    }
  }
  case$location=as.factor(case$location)
  case$bedrooms=NULL
  case$bathrooms=NULL
  case$floors=NULL
  case$view=NULL
  case$grade=NULL
  #case$zipcode=NULL
  return(case)
  }

# 9
box_cox=function(case){
  library( car )
  library( ellipse )
  library( leaps )
  library(MASS)
  library( GGally)
  library(rgl)
  b = boxcox(price ~ ., data = case)
  names(b)
  best_lambda_ind = which.max( b$y )
  best_lambda = b$x[ best_lambda_ind ]
  return(best_lambda)
}

# 10
leverages=function(case,g){
    lev = hatvalues( g )
    p = g$rank
    n = dim(case)[1]
    plot( g$fitted.values, lev, ylab = "Leverages", main = "Plot of Leverages", 
          pch = 16, col = 'black' )
    abline( h = 2 * p/n, lty = 2, col = 'red' )
    watchout_points_lev = lev[ which( lev > 2 * p/n  ) ]
    watchout_ids_lev = seq_along( lev )[ which( lev > 2 * p/n ) ] ## identify the rows relative to leverage points
    points( g$fitted.values[ watchout_ids_lev ], watchout_points_lev, col = 'red', pch = 16)
  # Vettore Osservazioni_Sane
    osservazioni_sane=seq( 1, 1, length.out = nrow(case) )
    osservazioni_sane[watchout_ids_lev]=0
    print(sum(osservazioni_sane))
  return(osservazioni_sane)
}  

# 11
standardized_residuals=function(case,g,osservazioni_sane){
  sort( g$res )
  sort( g$res ) [ c( 1, dim(model.matrix(g))[1] ) ]  ## per vedere il primo e l'ultimo residuo
  # Regola del pollice
  res_std = g$res/summary(g)$sigma
  watchout_ids_rstd = which( abs( res_std ) > 2 )
  watchout_rstd = res_std[ watchout_ids_rstd ]
  # Plot
  plot( g$fitted.values, res_std, ylab = "Standardized Residuals", main = "Standardized Residuals" )
  abline( h = c(-2,2), lty = 2, col = 'orange' )
  points( g$fitted.values[watchout_ids_rstd], 
          res_std[watchout_ids_rstd], col = 'red', pch = 16 )
  # Agg. vettore Osservazioni_Sane
  osservazioni_sane[watchout_ids_rstd]=0;
  print(sum(osservazioni_sane))      #Verifica che si sia aggiornato
  return(osservazioni_sane)
}

# 12
studentized_residuals=function(case,g,osservazioni_sane){
  stud = rstandard( g )
  watchout_ids_stud = which( abs( stud ) > 2 )
  watchout_stud = stud[ watchout_ids_stud ]
  # Plot resiui studentizzati
  plot( g$fitted.values, stud, ylab = "Studentized Residuals", main = "Studentized Residuals", pch = 16 )
  points( g$fitted.values[watchout_ids_stud], 
          stud[watchout_ids_stud], col = 'brown', pch = 16 )
  abline( h = c(-2,2), lty = 2, col = 'orange' )
  # Agg. vettore Osservazioni_Sane
  osservazioni_sane[watchout_ids_stud]=0;
  print(sum(osservazioni_sane))      #Verifica che si sia aggiornato
  return(osservazioni_sane)
}

# 13
distanza_di_Cook=function(case,g,osservazioni_sane){
  # __Rule of thumb__ C_i > {4}/{n-p}
  Cdist = cooks.distance( g )
  n = dim(case)[1]
  p = g$rank
  watchout_ids_Cdist = which( Cdist > 4/(n-p) ) 
  watchout_Cdist = Cdist[ watchout_ids_Cdist ]
  # Plot
  plot( g$fitted.values, Cdist, pch = 16, xlab = 'Fitted values', 
        ylab = 'Cooks Distance', main = 'Cooks Distance' )
  points( g$fitted.values[ watchout_ids_Cdist ], Cdist[ watchout_ids_Cdist ], 
          col = 'blue', pch = 16 )
  # Agg. vettore Osservazioni_Sane
  osservazioni_sane[watchout_ids_Cdist]=0;
  print(sum(osservazioni_sane))      #Verifica che si sia aggiornato
  return(osservazioni_sane)
}

# 14
modello_aggiornato=function(case,osservazioni,sane){
  case$logic=osservazioni_sane
  G=lm(logprezzo~bedrooms+bathrooms+sqft_living+floors+view+grade+anni,case,subset=(case$logic==1))
  case$logic=NULL
  return(G)
}

# 15
previsione=function(case_epurate,n){
  MAE=0;
  for(i in 1:40){
    # Separazione Training Set e Test Set
    s=sample(seq(1,n))
    training_set_ids=s[seq(1,ceiling(0.8*n))]
    test_set_ids=s[seq(ceiling(0.8*n)+1,n)]
    case_training=case_epurate[training_set_ids,]
    case_test=case_epurate[test_set_ids,]
    # Cross Validation
    g_training=lm(radprezzo~.,case_training) 
    beta_training=g_training$coefficients
    g_test=lm(radprezzo~.,case_test)
    Z=model.matrix(g_test)
    # Stima
    y_hat=Z%*%beta_training
    y_hat=(y_hat*lambda+1)^(1/lambda)
    # Prezzo effettivo
    y=case_test$radprezzo
    y=(y*lambda+1)^(1/lambda)
    # Calcolo Mean Absolute Error
    MAE=c(MAE,sum(abs(y-y_hat))/(floor(0.2*n)))
  }
  MAE=MAE[seq(2,41)]
  Average_MAE=mean(MAE)
  print(c('Sqrt of the Average MAE',sqrt(Average_MSE)))
  
  return(Average_MAE)
}

