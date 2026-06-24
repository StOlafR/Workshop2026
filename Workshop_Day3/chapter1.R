# chapter1.R

# Example 1.1: Porsche price vs. mileage

Porsche.df = read.csv(file=file.choose())  # Get the data (PorschePrice.csv)
dim(Porsche.df)   # Do some basic checks that we have the right data.
names(Porsche.df) # The variable names should agree with the book.

attach(Porsche.df)  # Remember, we use attach to refer more easily to
                    # variable names.
plot(Mileage, Price)  # The basic scatterplot of interest.

Porsche.lm1 = lm(Price ~ Mileage)
abline(Porsche.lm1)
abline(80, -.7, lty=2)  # Add a second, dashed line, which fits less well.
abline(h=mean(Price), lty=3)  # Add a horizontal, dotted line, at the 
                              # mean Price level.

is.list(Porsche.lm1)
length(Porsche.lm1)
is.vector(Porsche.lm1)
names(Porsche.lm1)
Porsche.lm1[[1]]
Porsche.lm1$coefficients
Porsche.lm1$coef
is.vector(Porsche.lm1$coef)
length(Porsche.lm1$coef)

cbind(Mileage,Price,Porsche.lm1$fit,Porsche.lm1$resid)[1:5,]
  # print first 5 rows
Porsche.lm1.table = data.frame(cbind(Mileage,Price,Porsche.lm1$fit,
  Porsche.lm1$resid))
names(Porsche.lm1.table)
names(Porsche.lm1.table)[c(3,4)] = c("fits", "residuals")
head(Porsche.lm1.table)
summary(Porsche.lm1)
summary(c(1,2,3,3,4,5,10))  # Here the input is a vector of length 7.
Porsche.lm1.table

plot(Porsche.lm1$resid~Porsche.lm1$fit) # plot of residuals vs fitted values
abline(h=0)                             # horizontal line at 0

hist(Porsche.lm1$resid)
qqnorm(Porsche.lm1$resid)  # Plot residuals vs theoretical normal quantiles
qqline(Porsche.lm1$resid)  # Fits a straight line to "aid the eye."

# After deleting the above graph, try reproducing it using your own function:
myqqnorm = function(x){
  qqnorm(x)
  qqline(x)
}
myqqnorm(Porsche.lm1$resid)

par(mfrow=c(2,2))
plot(Porsche.lm1)   # alternative set of residual plots
par(mfrow=c(1,1))


# Example 1.6: number of MDs in a metro area vs. number of hospitals

MetroHealth83.df = read.csv(file=file.choose())  # Get the data.
MetroHealth.df = MetroHealth83.df[,c(1,2,4)]  # extract the 3 variables
                                              # used in Ch 1 of text.
detach()
attach(MetroHealth.df)

# generate plots on page 39 (Figures 1.12 and 1.13)
par(mfrow=c(2,2))  
plot(NumHospitals, NumMDs, main="Scatterplot of MDs vs. Hospitals")
MetroHealth.lm1 = lm(NumMDs ~ NumHospitals)  # fit model
abline(MetroHealth.lm1)  # Add line to scatterplot.

plot(MetroHealth.lm1$fit, MetroHealth.lm1$resid, main="Residuals vs. Fits")
abline(h=0)  # horizontal line at 0
hist(MetroHealth.lm1$resid, main="Histogram of Residuals")
qqnorm(MetroHealth.lm1$resid)  # Plot residuals vs theoretical normal quantiles
qqline(MetroHealth.lm1$resid)  # Fits a straight line to "aid the eye."

par(mfrow=c(2,2))
plot(MetroHealth.lm1)   # Another helpful set of 4 diagnostic plots
par(mfrow=c(1,1))   # return to single graph window

SqrtMDs = sqrt(NumMDs)  # Create the square-rooted variable
MetroHealth.df = data.frame(MetroHealth.df, SqrtMDs)  # Add to data frame
dim(MetroHealth.df)   # Check that it got added.
names(MetroHealth.df)

MetroHealth.lm2 = lm(SqrtMDs ~ NumHospitals)

# Generate plots in Figures 1.14 and 1.15
par(mfrow=c(2,2))
plot(NumHospitals, SqrtMDs)
abline(MetroHealth.lm2)
plot(MetroHealth.lm2$fit, MetroHealth.lm2$resid)
abline(h=0)
hist(MetroHealth.lm2$resid)
qqnorm(MetroHealth.lm2$resid) 
qqline(MetroHealth.lm2$resid)  
par(mfrow=c(1,1))

# Find predicted NumMDs when there are 18 and 6 hospitals under both models
new.data = data.frame(NumHospitals=c(18,6)) 
predict(MetroHealth.lm1,new=new.data)
predict(MetroHealth.lm2,new=new.data)^2

# Generate Figure 1.16
plot(NumHospitals, NumMDs, main="Doctors vs. Hospitals with Quadratic fit")
min(NumHospitals)
max(NumHospitals)
xx = seq(2,32,length=101)
yy = (14.033 + 2.915*xx)^2
points(xx,yy,type="l")


# Example 1.9: Butterfly ballot - Buchanan votes vs. Bush votes

PalmBeach.df = read.csv(file=file.choose())  # Get the data.
detach()
attach(PalmBeach.df)
head(PalmBeach.df)
str(PalmBeach.df)

model.with = lm(Buchanan ~ Bush)

df1 = PalmBeach.df[!County=="PALM BEACH",]
  # create data frame, df1, removing Palm Beach County
dim(df1)
model.without = lm(df1$Buchanan ~ df1$Bush)

# Now recreate Figure 1.25.
plot(Bush, Buchanan)
abline(model.with)
abline(model.without, lty=2)
