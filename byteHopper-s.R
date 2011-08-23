#############################
##byteHopper s-1.0 simulation for Slush's pool
##Note that in this sim change in hop point is linear instead of random
##since there's only one x var.
##
##fill in constant vars below with current details
##c<-300 works atm, but you may wish to check it every now and then.

currentSlushScore<-14779687.8996
currentSlushSecs<-1432
slushShares<-609729
c<-300
diff<-1805700

slushSpeed<-slushShares*2^32/currentSlushSecs/10^9
slushSharesPerSec<-slushShares/currentSlushSecs

#score lookup table - only need to run once per session
score<-vector(10*diff,mode="numeric")
sharesToFind<-1:(10*diff)
score[1]<-1
for(i in 2:(10*diff)) score[i]<-score[(i-1)]+exp((i/slushSharesPerSec)/c)

#simulation starts here
loops<-100  #model variation use loops=1000, model accurate efficiency use loops = 100
maxRounds<-1000  	#model variation use rounds~100 (eg. at current difficulty and pool hashrate, rounds<-168 == 1 week)
			#model accurate efficiency use rounds = 100000 (about 11 years)
maxHop<-1         #up to how large a hop point do you want to model? 1.0 is a good start

#define outside loop vars
scoreEfficiencyPerShare<-matrix(0,loops,5)
hopoff<-1/loops*maxHop*diff

for(m in 1:loops){
sharesToFind<-vector(maxRounds,mode="numeric");hopShares<-vector(maxRounds,mode="numeric")
scoreSharesToFind<-vector(maxRounds,mode="numeric");scoreHopShares<-vector(maxRounds,mode="numeric")
propScore<-vector(maxRounds,mode="numeric");propShares<-vector(maxRounds,mode="numeric")
pps<-vector(maxRounds,mode="numeric")

###hopping simulation here:
while(min(sharesToFind)==0||max(sharesToFind)>(10*diff)){sharesToFind<-rgeom(maxRounds,1/diff)+1}
for(i in 1:maxRounds){
	if(sharesToFind[i]<hopoff) hopShares[i]<-sharesToFind[i] else hopShares[i]<-hopoff
	pps[i]<-sharesToFind[i]-hopShares[i]
	scoreSharesToFind[i]<-score[sharesToFind[i]]
	scoreHopShares[i]<-score[hopShares[i]]
	propScore[i]<-scoreHopShares[i]/scoreSharesToFind[i]
	propShares[i]<-hopShares[i]/sharesToFind[i]
}

scoreEfficiencyPerShare[m,1]<-hopoff
scoreEfficiencyPerShare[m,2]<-sum(propScore)/sum(hopShares)/(maxRounds/sum(sharesToFind)) #score eff. per hopped round
scoreEfficiencyPerShare[m,3]<-sum(pps+propScore*diff)/(maxRounds*diff)                    #score+pps eff.
scoreEfficiencyPerShare[m,4]<-sum(propShares)/sum(hopShares)/(maxRounds/sum(sharesToFind))#prop eff. per hopped round
scoreEfficiencyPerShare[m,5]<-sum(pps+propShares*diff)/(maxRounds*diff)                   #prop+pps eff.

print(m)

hopoff<-hopoff+(maxHop*diff)/loops

}

##get data from efficiency matrix:
scoreEfficiencyPerShare

##use the matrix to create charts, eg:
plot(scoreEfficiencyPerShare[,1],scoreEfficiencyPerShare[,2],pch=19)





