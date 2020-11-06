library(tidyverse)
library(broom)
library(mgcv)
library(mgcViz)
library(gamair)
library(rlang)

data("sole")

sole %>% 
  as_tibble() %>% 
  ggplot(aes(x = lo,y = la)) +
  geom_point()

data("bird")

bird %>% 
  as_tibble() %>% 
  ggplot(aes(x = x,y = y)) +
  geom_point(alpha=0.02)

ct2 <- gam(Volume ~ s(Height,bs="cr") + s(Girth,bs="cr"),
           family=Gamma(link=log),data=trees)
ct2 %>% summary()
# ct2 %>% plot(residuals=T)
vis.gam(x = ct2)
vis.gam(x = ct2,plot.type="contour")
gam.check(ct2)



# book chapter 7 ----------------------------------------------------------

library(gamair); library(mgcv)
## NOTE: Examples are marked 'Not run' to save CRAN check time  -------------------------------------------

## 7.1.1 using smooth constructors  -------------------------------------------

library(mgcv); library(MASS) ## load for mcycle data.  -------------------------------------------
## set up a smoother...  -------------------------------------------
sm <- smoothCon(s(times,k=10),data=mcycle,knots=NULL)[[1]]
## use it to fit a regression spline model...  -------------------------------------------
beta <- coef(lm(mcycle$accel~sm$X-1))
with(mcycle,plot(times,accel)) ## plot data  -------------------------------------------
times <- seq(0,60,length=200)  ## create prediction times  -------------------------------------------
## Get matrix mapping beta to spline prediction at 'times'  -------------------------------------------
Xp <- PredictMat(sm,data.frame(times=times))
lines(times,Xp%*%beta) ## add smooth to plot  -------------------------------------------

## Not run:   -------------------------------------------
## 7.2 Brain scan  -------------------------------------------
## 7.2.1 preliminary modelling  -------------------------------------------
require(gamair); require(mgcv); data(brain)
brain <- brain[brain$medFPQ>5e-3,] # exclude 2 outliers
brain %>% 
  as_tibble() %>% 
  ggplot(aes(x = X,y = Y)) +
  geom_point(aes(color=medFPQ))
m0 <- gam(medFPQ~s(Y,X,k=100),data=brain)
gam.check(m0)
vis.gam(x = m0,plot.type="contour")
getViz(m0) %>% sm(.,1) %>% plot()  

e <- residuals(m0); fv <- fitted(m0)
lm(log(e^2)~log(fv))

m1<-gam(medFPQ^.25~s(Y,X,k=100),data=brain)
gam.check(m1)
m2<-gam(medFPQ~s(Y,X,k=100),data=brain,family=Gamma(link=log))
gam.check(m2)
mean(fitted(m1)^4);mean(fitted(m2));mean(brain$medFPQ)
getViz(m1) %>% sm(.,1) %>% plot()  
getViz(m2) %>% sm(.,1) %>% plot()  

m2
vis.gam(m2,plot.type="contour",too.far=0.03,
        color="gray",n.grid=60,zlim=c(-1,2))

## 7.2.2 additive?  -------------------------------------------
m3 <- gam(medFPQ~s(Y,k=30)+s(X,k=30),data=brain,
          family=Gamma(link=log))
m3
AIC(m2,m3)

## 7.2.3 isotropic or tensor   -------------------------------------------
tm <- gam(medFPQ~te(Y,X,k=10),data=brain,family=Gamma(link=log))
tm1 <- gam(medFPQ ~ s(Y,k=10,bs="cr") + s(X,bs="cr",k=10) +
             ti(X,Y,k=10), data=brain, family=Gamma(link=log))
AIC(m2,tm,tm1)
anova(tm1)

## 7.2.4 Detecting symmetry  -------------------------------------------
brain$Xc <- abs(brain$X - 64.5)
brain$right <- as.numeric(brain$X<64.5)
m.sy <- gam(medFPQ~s(Y,Xc,k=100),data=brain,
            family=Gamma(link=log))
m.as <- gam(medFPQ~s(Y,Xc,k=100)+s(Y,Xc,k=100,by=right),
            data=brain,family=Gamma(link=log))
m.sy
m.as

anova(m.as)

vis.gam(m.sy,plot.type="contour",view=c("Xc","Y"),too.far=.03,
        color="gray",n.grid=60,zlim=c(-1,2),main="both sides")
vis.gam(m.as,plot.type="contour",view=c("Xc","Y"),
        cond=list(right=0),too.far=.03,color="gray",n.grid=60,
        zlim=c(-1,2),main="left side")
vis.gam(m.as,plot.type="contour",view=c("Xc","Y"),
        cond=list(right=1),too.far=.03,color="gray",n.grid=60,
        zlim=c(-1,2),main="right side")

## 7.2.5 Comparing surfaces  -------------------------------------------
brain1 <- brain
mu <- fitted(m2)
n<-length(mu)
ind <- brain1$X<60 & brain1$Y<20
mu[ind] <- mu[ind]/3
set.seed(1)
brain1$medFPQ <- rgamma(rep(1,n),mu/m2$sig2,scale=m2$sig2)

brain2=rbind(brain,brain1)
brain2$sample1 <- c(rep(1,n),rep(0,n))
brain2$sample0 <- 1 - brain2$sample1

m.same<-gam(medFPQ~s(Y,X,k=100),data=brain2,
            family=Gamma(link=log))
m.diff<-gam(medFPQ~s(Y,X,k=100)+s(Y,X,by=sample1,k=100),
            data=brain2,family=Gamma(link=log))
AIC(m.same,m.diff)
anova(m.diff)

## 7.2.6 Prediction  -------------------------------------------
glance(m2)
tidy(m2)
tidy(m2,parametric = T)
augment(m2) 
augment(m2,newdata = brain,type.predict = "response") #prediction to new dataset!
# augment(m2,newdata = brain,type.predict = "link") #prediction to new dataset!
# augment(m2,newdata = brain,type.predict = "term") #prediction to new dataset!
# for each observation
# fited values are the predicted
# se the standard error of fit
# residual observed minus fitted
predict(m2)[1:5]
pv <- predict(m2,se=TRUE)
pv$fit[1:5]
pv$se[1:5]

predict(m2,type="response")[1:5]
pv <- predict(m2,type="response",se=TRUE)
pv$se[1:5]

pd <- data.frame(X=c(80.1,68.3),Y=c(41.8,41.8))
predict(m2,newdata=pd)
predict(m2,newdata=pd,type="response",se=TRUE)

tidy(m3,parametric = T)
augment(m3,newdata=pd,type.predict="link")
augment(m3,newdata=pd,type.predict="response")
augment(m3,newdata=pd,type.predict="terms")
predict(m3,newdata=pd,type="terms",se=TRUE)

Xp <- predict(m2,newdata=pd,type="lpmatrix")
fv <- Xp%*%coef(m2)
fv
d <- t(c(1,-1))
d%*%fv
d%*%Xp%*%m2$Vp%*%t(Xp)%*%t(d)

## 7.2.7 Variance of non-linear function  -------------------------------------------

ind <- brain$region==1& ! is.na(brain$region)
Xp <- predict(m2,newdata=brain[ind,],type="lpmatrix")
set.seed(8) ## for repeatability  -------------------------------------------
br <- rmvn(n=1000,coef(m2),vcov(m2)) # simulate from posterior
mean.FPQ<-rep(0,1000)
for (i in 1:1000)
{ lp <- Xp%*%br[i,]  # replicate linear predictor
mean.FPQ[i] <- mean(exp(lp)) # replicate region 1 mean FPQ
}
mean.FPQ <- colMeans(exp(Xp%*%t(br)))

## 7.3 Retinopathy  -------------------------------------------
require(gamair); require(mgcv); data(wesdr)
wesdr %>% as_tibble()
wesdr %>% as_tibble() %>% 
  mutate(dur_cat=cut(dur,breaks = 3),
         gly_cat=cut(gly,breaks = 3),
         bmi_cat=cut(bmi,breaks = 3),
         ret=as.factor(ret)) %>% 
  ggplot(aes(x = dur_cat,fill = ret)) +
  # ggplot(aes(x = gly_cat,fill = ret)) +
  # ggplot(aes(x = bmi_cat,fill = ret)) +
  geom_bar(
    position = position_fill()
    )
k <- 7
b <- gam(ret ~ s(dur,k=k) + s(gly,k=k) + s(bmi,k=k) + 
           ti(dur,gly,k=k) + ti(dur,bmi,k=k) + ti(gly,bmi,k=k),
         select=TRUE, data=wesdr, family=binomial(), method="ML")
b
gam.check(b)
b %>% tidy() %>% mutate(p.value=as.character(p.value))
b %>% tidy(parametric=T)
b %>% glance()
b %>% augment(type.predict="link")
b %>% augment(type.predict="response")
b %>% augment(type.predict="term")
# b %>% plot()
# b %>% vis.gam()
# b %>% getViz() %>% sm(.,4) %>% plot()  
# b %>% getViz() %>% sm(.,5) %>% plot()  
# b %>% getViz() %>% sm(.,6) %>% plot()  

b <- gam(ret ~ s(dur,k=k) + te(gly,bmi,k=k),
         select=TRUE, data=wesdr, family=binomial(), method="ML")
b
gam.check(b)
b %>% tidy() %>% mutate(p.value=as.character(p.value))
b %>% tidy(parametric=T)
b %>% glance()
b %>% augment(type.predict="link") %>% skimr::skim(.fitted)
b %>% augment(type.predict="response") %>% skimr::skim(.fitted)
b %>% augment(type.predict="term")
b %>% getViz() %>% sm(.,2) %>% plot() #smoothed risk space
b %>% augment(type.predict="link") %>% 
  # naniar::miss_var_summary()
  ggplot(aes(x = gly,y = bmi, colour=.fitted)) +
  geom_point() +
  scale_color_viridis_c()

## 7.4 Air pollution  -------------------------------------------
data(chicago)
chicago %>% as_tibble()
ap0 <- gam(death~s(time,bs="cr",k=200)+pm10median+so2median+
             o3median+tmpd,data=chicago,family=poisson)
gam.check(ap0)

par(mfrow=c(2,1))
plot(ap0,n=1000)  # n increased to make plot smooth
plot(ap0,residuals=TRUE,n=1000)

chicago$death[3111:3125]

ap1<-gam(death~s(time,bs="cr",k=200)+s(pm10median,bs="cr")+
           s(so2median,bs="cr")+s(o3median,bs="cr")+s(tmpd,bs="cr"),
         data=chicago,family=poisson)

## 7.4.1 single index  -------------------------------------------

lagard <- function(x,n.lag=6) {
  n <- length(x); X <- matrix(NA,n,n.lag)
  for (i in 1:n.lag) X[i:n,i] <- x[i:n-i+1] 
  X
}
dat <- list(lag=matrix(0:5,nrow(chicago),6,byrow=TRUE),
            death=chicago$death,time=chicago$time)
dat$pm10 <- lagard(chicago$pm10median)
dat$tmp <- lagard(chicago$tmpd)
dat$o3 <- lagard(chicago$o3median)

si <- function(theta,dat,opt=TRUE) {
  ## Return ML if opt==TRUE or fitted gam otherwise.  -------------------------------------------
  alpha <- c(1,theta) ## alpha defined via unconstrained theta  -------------------------------------------
  kk <- sqrt(sum(alpha^2)); alpha <- alpha/kk  ## ||alpha||=1  -------------------------------------------
  o3 <- dat$o3%*%alpha; tmp <- dat$tmp%*%alpha
  pm10 <- dat$pm10%*%alpha ## re-weight lagged covariates  -------------------------------------------
  b<- bam(dat$death~s(dat$time,k=200,bs="cr")+s(pm10,bs="cr")+
            te(o3,tmp,k=8),family=poisson) ## fit model  -------------------------------------------
  cat(".") ## give user something to watch  -------------------------------------------
  if (opt) return(b$gcv.ubre) else {
    b$alpha <- alpha  ## add alpha to model object  -------------------------------------------
    b$J <- outer(alpha,-theta/kk^2) ## get dalpha_i/dtheta_j  -------------------------------------------
    for (j in 1:length(theta)) b$J[j+1,j] <- b$J[j+1,j] + 1/kk
    return(b)
  }
} ## si  -------------------------------------------

## WARNING: the next line takes around half an hour to run  -------------------------------------------

f1 <- optim(rep(1,5),si,method="BFGS",hessian=TRUE,dat=dat)

apsi <- si(f1$par,dat,opt=FALSE)
apsi$alpha

## 7.4.2 distributed lag...  -------------------------------------------

apl <- bam(death~s(time,bs="cr",k=200)+te(pm10,lag,k=c(10,5))+
             te(o3,tmp,lag,k=c(8,8,5)),family=poisson,data=dat)

## 7.5 Egg survey - less than a minute  -------------------------------------------
## 7.5.1 Model development  -------------------------------------------
data(mack)
mack$log.net.area <- log(mack$net.area)
mack %>% 
  as_tibble() %>% 
  ggplot(aes(x = lon,y = lat,color=egg.count)) +
  geom_point()

gmtw <- gam(egg.count ~ s(lon,lat,k=100) + s(I(b.depth^.5))+ 
              s(c.dist) + s(salinity) + s(temp.surf) + s(temp.20m)+
              offset(log.net.area),data=mack,family=tw,method="REML",
            select=TRUE)
gmtw %>% tidy() %>% mutate(p.value=as.character(p.value))

gm2 <- gam(egg.count ~ s(lon,lat,k=100) + s(I(b.depth^.5)) + 
             s(c.dist) + s(temp.20m) + offset(log.net.area),
           data=mack,family=tw,method="REML")
gm2
gam_humber <- gm2
gam_humber %>% broom::augment()
gam_humber %>% broom::augment(type.predict="link") %>% skimr::skim(.fitted)
gam_humber %>% broom::augment(type.predict="response") %>% skimr::skim(.fitted)
# gam_humber %>% broom::augment(type.predict="term")
gam_humber %>% plot()
# gam_humber %>% vis.gam()
gam_humber_viz <- getViz(gm2)
gam_humber_viz_raster <- 
  plot(sm(gam_humber_viz, 1)) + 
  l_fitRaster() +
  l_fitContour()
gam_humber_viz_raster + coord_fixed()
gam_humber_viz_raster$data$fit %>% 
  as_tibble() %>% 
  ggplot() +
  geom_contour_filled(aes(x = x, y = y, z = z)) + 
  coord_fixed()

## 7.5.2 model predictions  -------------------------------------------
par(mfrow=c(1,3))
data(mackp); data(coast)
mackp %>% as_tibble()
mackp$log.net.area <- rep(0,nrow(mackp))
lon <- seq(-15,-1,1/4); lat <- seq(44,58,1/4)
zz<-array(NA,57*57); zz[mackp$area.index]<-predict(gm2,mackp) 
image(lon,lat,matrix(zz,57,57),col=gray(0:32/32),
      cex.lab=1.5,cex.axis=1.4)
contour(lon,lat,matrix(zz,57,57),add=TRUE)
lines(coast$lon,coast$lat,col=1)

# predictions from the raster of observed values per area
augment(gm2,newdata=mackp,type.predict="link") %>% skimr::skim(.fitted)
augment(gm2,newdata=mackp,type.predict="response") %>% skimr::skim(.fitted)
augment(gm2,newdata=mackp,type.predict="link") %>% 
  ggplot() +
  geom_tile(aes(x = lon, y = lat, fill = .fitted)) +
  scale_fill_viridis_c() +
  # geom_contour(aes(x = lon, y = lat, z = .fitted)) + 
  # geom_contour_filled(aes(x = lon, y = lat, z = .fitted)) + 
  coord_fixed()
augment(gm2,newdata=mackp,type.predict="response") %>% 
  ggplot() +
  geom_contour_filled(aes(x = lon, y = lat, z = .fitted)) + 
  coord_fixed()

set.seed(4) ## make reproducable  -------------------------------------------
br1 <- rmvn(n=1000,coef(gm2),vcov(gm2))
Xp <- predict(gm2,newdata=mackp,type="lpmatrix")
mean.eggs1 <- colMeans(exp(Xp%*%t(br1)))
hist(mean.eggs1)

## 7.5.3 alternative  -------------------------------------------

gmgr <- gam(egg.count ~s(lon,lat,k=100)+s(lon,lat,by=temp.20m)
            +s(lon,lat,by=I(b.depth^.5)) +offset(log.net.area),
            data=mack,family=tw,method="REML")

## 7.6 Larks - about a minute  -------------------------------------------
library(gamair); data(bird)
bird$n <- bird$y/1000;bird$e <- bird$x/1000
bird %>% as_tibble()

m1 <- gam(crestlark~s(e,n,bs="tp",k=100),data=bird,family=binomial,
          method="REML")
m1
plot(m1,scheme = 3,contour.col=1,too.far=0.03,rug=FALSE)

m2 <- gam(crestlark ~ s(e,n,bs="ds",m=c(1,.5),k=100),data=bird,family=binomial,
          method="REML")
m2
plot(m2,scheme = 3,contour.col=1,too.far=0.03,rug=FALSE)

REML <- r <- 1:10*10 
for (i in 1:length(r)) { 
  mt <- gam(crestlark ~ s(e,n,bs="gp",m=c(3,r[i]),k=100),
            data=bird,family=binomial,method="REML")
  REML[i] <- mt$gcv.ubre
  if (i==1||REML[i]==REML0) { m3 <- mt; REML0 <- REML[i]}
}
AIC(m1,m2,m3)

bird %>% as_tibble()
bird$tet.n <- bird$N <- rep(1,nrow(bird))
bird$N[is.na(as.vector(bird$crestlark))] <- NA
ba <- aggregate(data.matrix(bird), by=list(bird$QUADRICULA),
                FUN=sum, na.rm=TRUE)
ba$e <- ba$e/ba$tet.n; ba$n <- ba$n/ba$tet.n
ba %>% as_tibble()

m10 <- gam(cbind(crestlark,N-crestlark) ~ s(e,n,k=100),
           data=ba, family=binomial, method="REML")
m10
plot(m10,scheme = 3,contour.col=1,too.far=0.03,rug=FALSE)

library(geoR)
coords<-matrix(0,nrow(ba),2)
coords[,1]<-ba$e
coords[,2]<-ba$n
gb<-list(data=residuals(m10,type="d"),coords=coords)
plot(variog(gb,max.dist=100))
plot(fitted(m10),residuals(m10))

augment(m10)
from_augment <- augment(m10,type.residuals="deviance")
gb<-list(data=from_augment %>% pull(.resid),
         coords=from_augment %>% select(e,n) %>% as.matrix())
plot(variog(gb,max.dist=100)) # variaogram of deviance residuals
plot(fitted(m10),residuals(m10)) #deviance residuals against fitted probabilities
augment(m10,type.residuals="deviance",type.predict="response") %>% 
  ggplot(aes(x = .fitted,y = .resid)) +
  geom_point() +
  geom_smooth()

## 7.7.1 Sole egg GAMM  -------------------------------------------
## Chapter 3 preliminaries...  -------------------------------------------
data(sole)
sole$off <- log(sole$a.1-sole$a.0)# model offset term
sole$a<-(sole$a.1+sole$a.0)/2     # mean stage age
solr<-sole                        # make copy for rescaling
solr$t<-solr$t-mean(sole$t)
solr$t<-solr$t/var(sole$t)^0.5
solr$la<-solr$la-mean(sole$la)
solr$lo<-solr$lo-mean(sole$lo)

## GAMM fit...  -------------------------------------------
solr$station <- factor(with(solr,paste(-la,-lo,-t,sep="")))     
solr %>% as_tibble() %>% 
  # count(t) # time collection
  # count(a) # mean stage
  ggplot(aes(x = lo,y = la)) +
  geom_point(aes(color=eggs,size=eggs)) +
  facet_wrap(~t)

som <- gamm(eggs~te(lo,la,t,bs=c("tp","tp"),k=c(25,5),d=c(2,1))
            +s(t,k=5,by=a)+offset(off), 
            family=quasipoisson,
            data=solr,
            random=list(station=~1))
som$gam
som1 <- bam(eggs~te(lo,la,t,bs=c("tp","tp"),k=c(25,5),d=c(2,1))
            + s(t,k=5,by=a)+offset(off)+s(station,bs="re"),
            family=quasipoisson,
            data=solr)
gam.vcomp(som1)
som$lme
## boundary and knots for soap...  -------------------------------------------
bnd <- list(list(lo=c(-6.74,-5.72,-5.7 ,-5.52,-5.37,-5.21,-5.09,-5.02,
                      -4.92,-4.76,-4.64,-4.56,-4.53,-4.3,-4.16,-3.8 ,-3.8,-5.04,-6.76,
                      -6.74),
                 la=c(50.01,50.02,50.13,50.21,50.24,50.32,50.41,50.54,50.59,50.64,
                      50.74,50.86,51.01,51  ,51.2,51.22,51.61,51.7,51.7,50.01)))

knt <- list(lo=c(-4.643,-5.172,-5.638,-6.159,-6.665,-6.158,-5.656,-5.149,
                 -4.652,-4.154,-3.901,-4.146,-4.381,-4.9,-5.149,-5.37,-5.866,-6.36,-6.635,
                 -6.12,-5.626,-5.117,-4.622,-4.695,-4.875,-5.102,-5.609,-5.652,-5.141,
                 -5.354,-5.843,-6.35,-6.628,-6.127,-5.63,-5.154,-5.356,-5.652,-5.853,
                 -6.123),
            la=c(51.626,51.61,51.639,51.638,51.376,51.377,51.373,51.374,51.374,
                 51.376,51.379,51.226,51.129,51.194,51.083,51.147,51.129,51.151,50.901,
                 50.891,50.959,50.958,50.942,50.728,50.676,50.818,50.825,50.684,50.693,
                 50.568,50.564,50.626,50.397,50.451,50.443,50.457,50.325,50.193,50.322,
                 50.177))

sole$station <- solr$station ## station to sole data  -------------------------------------------

som2 <- bam(eggs ~ te(lo,la,t,bs=c("sw","cr"),k=c(40,5),
                      d=c(2,1),xt=list(list(bnd=bnd),NULL)) +
              s(t,k=5,by=a) + offset(off) + s(station,bs="re"),
            knots=knt, family=quasipoisson, data=sole)

## 7.7.2 Cairo temperature  -------------------------------------------
data(cairo)
ctamm <- gamm(temp~s(day.of.year,bs="cc",k=20)+s(time,bs="cr"),
              data=cairo,correlation=corAR1(form=~1|year))
summary(ctamm$gam)
intervals(ctamm$lme,which="var-cov")
ctamm$gam$sig2/ctamm$gam$sp
plot(ctamm$gam,scale=0,pages=1)

REML <- rho <- 0.6+0:20/100
for (i in 1:length(rho)) {
  ctbam <- bam(temp~s(day.of.year,bs="cc",k=20)+s(time,bs="cr"),
               data=cairo,rho=rho[i])
  REML[i] <- ctbam$gcv.ubre
}
rho[REML==min(REML)]

## 7.7.3 Fully Bayesian  -------------------------------------------
## Not currently included (requires editing of JAGS file)  -------------------------------------------

## 7.7.4 Random wiggly curves  -------------------------------------------
data(sitka)
sitka$id.num <- as.factor(sitka$id.num)
b <- gamm(log.size~s(days) + ozone + ozone:days +
            s(days,id.num,bs="fs",k=5),data=sitka)
plot(b$gam,pages=1)


## 7.8 survival  -------------------------------------------
require(survival)
data(pbc) ## loads pbcseq also  -------------------------------------------
pbc$status1 <- as.numeric(pbc$status==2)
pbc$stage <- factor(pbc$stage)
b0 <- gam(time ~ trt+sex+stage+s(sqrt(protime))+s(platelet)+ 
            s(age)+s(bili)+s(albumin)+s(sqrt(ast))+s(alk.phos),
          weights=status1,family=cox.ph,data=pbc)

b <- gam(time ~ trt+sex+s(sqrt(protime))+s(platelet)+ 
           s(age)+s(bili)+s(albumin),
         weights=status1,family=cox.ph,data=pbc)

anova(b)
par(mfrow=c(2,3))
plot(b); plot(b$linear.predictors,residuals(b))

par(mfrow=c(1,1))
## create prediction data frame...  -------------------------------------------
np <- 300
newd <- data.frame(matrix(0,np,0))
for (n in names(pbc)) newd[[n]] <- rep(pbc[[n]][25],np)
newd$time <- seq(0,4500,length=np)
## predict and plot the survival function...   -------------------------------------------
fv <- predict(b,newdata=newd,type="response",se=TRUE)
plot(newd$time,fv$fit,type="l",ylim=c(0.,1),xlab="time",
     ylab="survival",lwd=2)
## add crude one s.e. intervals...  -------------------------------------------
lines(newd$time,fv$fit+fv$se.fit,col="grey")
lines(newd$time,fv$fit-fv$se.fit,col="grey")
## and intervals based on cumulative hazard s.e...  -------------------------------------------
se <- fv$se.fit/fv$fit
lines(newd$time,exp(log(fv$fit)+se))
lines(newd$time,exp(log(fv$fit)-se))

## 7.8.1 time dependent  -------------------------------------------
## copy functions from ?cox.pht in mgcv...  -------------------------------------------

app <- function(x,t,to) {
  ## wrapper to approx for calling from apply...  -------------------------------------------
  y <- if (sum(!is.na(x))<1) rep(NA,length(to)) else
    approx(t,x,to,method="constant",rule=2)$y
  if (is.factor(x)) factor(levels(x)[y],levels=levels(x)) else y
} ## app  -------------------------------------------

tdpois <- function(dat,event="z",et="futime",t="day",
                   status="status1",id="id") {
  ## dat is data frame. id is patient id; et is event time; t is  -------------------------------------------
  ## observation time; status is 1 for death 0 otherwise;  -------------------------------------------
  ## event is name for Poisson response.  -------------------------------------------
  if (event %in% names(dat)) warning("event name in use")
  require(utils) ## for progress bar  -------------------------------------------
  te <- sort(unique(dat[[et]][dat[[status]]==1])) ## event times  -------------------------------------------
  sid <- unique(dat[[id]])
  prg <- txtProgressBar(min = 0, max = length(sid), initial = 0,
                        char = "=",width = NA, title="Progress", style = 3)
  ## create dataframe for poisson model data  -------------------------------------------
  dat[[event]] <- 0; start <- 1
  dap <- dat[rep(1:length(sid),length(te)),]
  for (i in 1:length(sid)) { ## work through patients  -------------------------------------------
    di <- dat[dat[[id]]==sid[i],] ## ith patient's data  -------------------------------------------
    tr <- te[te <= di[[et]][1]] ## times required for this patient  -------------------------------------------
    ## Now do the interpolation of covariates to event times...  -------------------------------------------
    um <- data.frame(lapply(X=di,FUN=app,t=di[[t]],to=tr))
    ## Mark the actual event...  -------------------------------------------
    if (um[[et]][1]==max(tr)&&um[[status]]==1) um[[event]][nrow(um)] <- 1 
    um[[et]] <- tr ## reset time to relevant event times  -------------------------------------------
    dap[start:(start-1+nrow(um)),] <- um ## copy to dap  -------------------------------------------
    start <- start + nrow(um)
    setTxtProgressBar(prg, i)
  }
  close(prg)
  dap[1:(start-1),]
} ## tdpois  -------------------------------------------

## model fitting...  -------------------------------------------

data(pbc)
pbcseq$status1 <- as.numeric(pbcseq$status==2) ## deaths  -------------------------------------------
pb <- tdpois(pbcseq) ## conversion  -------------------------------------------
pb$tf <- factor(pb$futime) ## add factor for event time  -------------------------------------------

b <- bam(z ~ tf - 1  +  trt + s(sqrt(protime)) + s(platelet) + 
           s(age) + s(bili) + s(albumin) + s(sqrt(ast)),
         family=poisson,data=pb,discrete=TRUE,nthreads=2)

chaz <- tapply(fitted(b),pb$id,sum) ## cum. hazard by subject  -------------------------------------------
d <- tapply(pb$z,pb$id,sum) ## censoring indicator  -------------------------------------------
mrsd <- d - chaz ## Martingale residuals  -------------------------------------------
drsd <- sign(mrsd)*sqrt(-2*(mrsd + d*log(chaz))) ## deviance  -------------------------------------------

te <- sort(unique(pb$futime)) ## event times  -------------------------------------------
di <- pbcseq[pbcseq$id==25,] ## data for subject 25  -------------------------------------------
## interpolate to te using app from ?cox.pht...  -------------------------------------------
pd <- data.frame(lapply(X=di,FUN=app,t=di$day,to=te)) 
pd$tf <- factor(te)
X <- predict(b,newdata=pd,type="lpmatrix")
eta <- drop(X%*%coef(b)); H <- cumsum(exp(eta))
J <- apply(exp(eta)*X,2,cumsum)
se <- diag(J%*%vcov(b)%*%t(J))^.5
par(mfrow=c(1,2))
plot(stepfun(te,c(1,exp(-H))),do.points=FALSE,ylim=c(0.7,1),
     ylab="S(t)",xlab="t (days)",main="",lwd=2)
lines(stepfun(te,c(1,exp(-H+se))),do.points=FALSE)
lines(stepfun(te,c(1,exp(-H-se))),do.points=FALSE)
rug(pbcseq$day[pbcseq$id==25]) ## measurement times  -------------------------------------------

er <- pbcseq[pbcseq$id==25,]
plot(er$day,er$ast);lines(te,pd$ast)

## 7.9 Location scale  -------------------------------------------

library(MASS);library(mgcv)
b <- gam(list(accel~s(times,bs="ad"),~s(times,bs="ad")),
         family=gaulss,data=mcycle)

## 7.9.1 Extreme rainfall  -------------------------------------------
library(mgcv);library(gamair);data(swer)
swer %>% as_tibble()

b0 <- gam(list(exra ~ s(nao)+ s(elevation)+ climate.region+
                 te(N,E,year,d=c(2,1),k=c(20,5)),
               ~ s(year)+ s(nao)+ s(elevation)+ climate.region+ s(N,E),
               ~ s(elevation)+ climate.region),family=gevlss,data=swer)

b <- gam(list(exra~ s(nao)+s(elevation)+climate.region+s(N,E),
              ~ s(year)+ s(elevation)+ climate.region+ s(N,E),
              ~ climate.region),family=gevlss,data=swer)
plot(b,scale=0,scheme=c(1,1,3,1,1,3),contour.col="white",pages=1)

mu <- fitted(b)[,1];rho <- fitted(b)[,2]; xi <- fitted(b)[,3]
fv <- mu + exp(rho)*(gamma(1-xi)-1)/xi

Fi.gev <- function(z,mu,sigma,xi) { ## GEV inverse cdf.  -------------------------------------------
  xi[abs(xi)<1e-8] <- 1e-8 ## approximate xi=0, by small xi  -------------------------------------------
  x <- mu + ((-log(z))^-xi-1)*sigma/xi
}
mb <- coef(b);Vb <- vcov(b) ## posterior mean and cov  -------------------------------------------
b1 <- b ## copy fitted model object to modify  -------------------------------------------
n.rep <- 1000; br <- rmvn(n.rep,mb,Vb) ## posterior sim  -------------------------------------------
n <- length(fitted(b))
sim.dat <- cbind(data.frame(rep(0,n*n.rep)),swer$code)
for (i in 1:n.rep) {
  b1$coefficients <- br[i,] ## copy sim coefs to gam object  -------------------------------------------
  X <- predict(b1,type="response");ii <- 1:n + (i-1)*n
  sim.dat[ii,1] <- Fi.gev(runif(n),X[,1],exp(X[,2]),X[,3])
}

stm <- tapply(sim.dat[,1],sim.dat[,2],mean)
st98 <- tapply(sim.dat[,1],sim.dat[,2],quantile,probs=0.98)

## 7.10 Multivariate  -------------------------------------------
library(mgcv); library(gamair); data(mpg)
b <- gam(list(city.mpg ~ fuel +style +drive +s(weight) +s(hp)
              + s(make,bs="re"),
              hw.mpg ~ fuel +style +drive +s(weight) +s(hp)
              + s(make,bs="re")),
         family = mvn(d=2) , data = mpg)

b1 <- gam(list(city.mpg ~ fuel +style +drive +s(hp) +s(weight)
               + s(make,bs="re"),
               hw.mpg ~ fuel +style +drive +s(make,bs="re"),
               1+2 ~ s(weight) +s(hp) -1),
          family = mvn(d=2) , data = mpg)

## 7.11 FDA  -------------------------------------------
## 7.11.1 scalar-on-function  -------------------------------------------
data(gas)
b <- gam(octane~s(nm,by=NIR,k=50),data=gas)
par(mfrow=c(1,2))
plot(b,scheme=1,col=1)
plot(fitted(b),gas$octane)

## Prostate...  -------------------------------------------
data(prostate)
b <- gam(type ~ s(MZ,by=intensity,k=100),family=ocat(R=3),
         data=prostate,method="ML")
par(mfrow=c(1,3))
plot(b,rug=FALSE,scheme=1,xlab="Daltons",ylab="f(D)",
     cex.lab=1.6,cex.axis=1.4)
pb <- predict(b,type="response") ## matrix of class probs  -------------------------------------------
plot(factor(prostate$type),pb[,3])
qq.gam(b,rep=100,lev=.95)

prostate$type1 <- prostate$type - 1 ## recode for multinom  -------------------------------------------
b1 <- gam(list(type1 ~ s(MZ,by=intensity,k=100),
               ~ s(MZ,by=intensity,k=100)),
          family=multinom(K=2),data=prostate)
plot(b1,pages=1,scheme=1,rug=FALSE)

## 7.11.2 Canadian weather  -------------------------------------------
require(gamair);require(lattice);data(canWeather)
xyplot(T~time|region,data=CanWeather,type="l",groups=place)

aic <- reml <- rho <- seq(0.9,0.99,by=.01)
for (i in 1:length(rho)) {
  b <- bam(T ~ region + s(time,k=20,bs="cr",by=region) +
             s(time,k=40,bs="cr",by=latitude),
           data=CanWeather,AR.start=time==1,rho=rho[i])
  aic[i] <- AIC(b); reml[i] <- b$gcv.ubre
}

## End(Not run)  -------------------------------------------
