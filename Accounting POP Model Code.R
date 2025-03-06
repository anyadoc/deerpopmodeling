##ACOUNTING STYLE POPULATION MODEL CODE 
##Used for Ani's excel called "AUDeerPopModel" 

##setting up matrices##
Npost=matrix(nrow=6,ncol=21)
Nmid=matrix(nrow=6,ncol=21)
Born=matrix(nrow=2,ncol=21)
Npre=matrix(nrow=6,ncol=21)

##harvest parameters##
harvest=c(0.06,0.03,0.13,0.07,0.25,0.08)
survharvest=1-harvest
##births/preg rates##
##yearling=0.2, adults=1.6
preg.rate=c(0.2,1.6)
##year one##
Npre[,1]=c(1000,1500,1200,1800,1800,2700)

##begins data with postharvest
##going from year 1 preharvest to year 2 post harvest


##nonharvest mortality
nhm=c(0.285857157,0.285857157,0.059255614,0,0.059255614,0.116823913)
survnhm=1-nhm
nhm6mfawn=(0.28936648)
survnhm6mfawn=1-nhm6mfawn

#######################actual loop#######################
##i=year, i-1=previous year
##Ani's year is 1 less than my year
for(i in 2:21){
  Npost[,i]=round(Npre[,(i-1)]*survharvest,0)
  
  
##Getting to Mid numbers for all cohorts
Nmid[,i]=round(Npost[,i]*survnhm,0)


Born[,i]=round((Nmid[4,i]*preg.rate[1]+Nmid[6,i]*preg.rate[2])/2,0)
##gives male and female category

##Getting to Preharvest numbers for fawns
Npre[1:2,i]=round(Born[1:2,i]*survnhm6mfawn,0)

##Getting to Preharvest numbers for yearlings
Npre[3:4,i]=round(Nmid[1:2,i]*survnhm[3:4],0)

##Getting to Preharvest numbers for adults
Npre[5:6,i]=round((Nmid[3:4,i]+Nmid[5:6,i])*survnhm[5:6],0)
  
}

##gives actual estimates for each cohort
Npost
Nmid
Born
Npre
