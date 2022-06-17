#function to create case control logistic regression, including mixing, or mathcing variable

#data=ego network data, of form chara charac1, charac2, etc.; also have ties alter info, weights and possibly other info
#var.name.degree=name of variable in data that has the degree of the respondents
#var.name.characs= vector of names of characterisictics for homophily model-case control logistic
#case.control.type='simulated.data', when just running case.control.data.function, other possible options
#var.name.characs.alter=list of variable names for alter characteristics, same order as var.name.charac
#sim.net=simulated network from which the control part will be drawn
#formula=ergm formula used to run logistic regression 
#resp.weights=vector of weights to create representative population 
#output.weights=T or F, should the weights of the respondent be outputed?
#max.alter=max number of alters the respondents were allowed to name
#max.control.data.N=if not null, number of dyads in control part of regression 

which.func=function(x,y){which(x==y)}

case.control.data.create.vars=function(data=NULL,var.name.degree=NULL,var.name.characs=NULL,case.control.type=NULL
,var.name.characs.alter=NULL,sim.net=NULL
,formula=NULL
,resp.weights=NULL,output.weights=F,max.alter=NULL,max.control.data.N=NULL,var.name.characs.person2=NULL,control.type=NULL
,tie.data=NULL,remove.isolates.control=TRUE
,output.ids=F,id.names=NULL,one_one_match_withreplace=T,resp.control.weights=NULL
,N.type="dyads",case.type="none"){

var.type=NA
for (i in 1:length(var.name.characs)){

ids=grep(var.name.characs[i],attr(terms.formula(formula),"variables"))
abs=grep("absdiff",attr(terms.formula(formula),"variables"))
match=grep("nodematch",attr(terms.formula(formula),"variables"))
mix=grep("nodemix",attr(terms.formula(formula),"variables"))
id.type=max(c(1*as.numeric(sum(ids %in%abs)>0)
,2*as.numeric(sum(ids %in%match)>0)
,3*as.numeric(sum(ids %in%mix)>0)
))

var.type[i]=switch(paste(id.type),"0"=NA,"1"="absdiff","2"="nodematch","3"="nodemix")
}


base.name=list()

for (i in 1:length(var.name.characs)){
if (var.type[i]%in% c("absdiff", "nodematch")){base.name[[i]]=NULL}
if (var.type[i]%in% c("nodemix")){
ids=grep(var.name.characs[i],attr(terms.formula(formula),"variables"))
mix=grep("nodemix",attr(terms.formula(formula),"variables"))

ids=ids[ids %in% mix]

charac.mix=as.character(attr(terms.formula(formula),"variables")[[ids]])
if(length(charac.mix)==3){base.name[[i]]=charac.mix[[3]]}
#if(length(charac.mix)!=3){base.name[[i]]=NULL}

 }
}

if (length(base.name)<i ){base.name[[i+1]]="hold"}


var.nodecov=NA
for ( i in 1:length(var.name.characs)){
ids=grep(var.name.characs[i],attr(terms.formula(formula),"variables"))
node=grep("nodecov",attr(terms.formula(formula),"variables"))
ids=ids[ids %in% node]

var.nodecov[i]=ifelse(length(ids)>0,TRUE,FALSE)
}


var.nodefactor=NA
for ( i in 1:length(var.name.characs)){
ids=grep(var.name.characs[i],attr(terms.formula(formula),"variables"))
node=grep("nodefactor",attr(terms.formula(formula),"variables"))
ids=ids[ids %in% node]

var.nodefactor[i]=ifelse(length(ids)>0,TRUE,FALSE)
}


base.name.factor=list()

for (i in 1:length(var.name.characs)){
if (var.nodefactor[i] %in% FALSE){base.name.factor[[i]]=NULL}
if (var.nodefactor[i]%in% c(TRUE)){
ids=grep(var.name.characs[i],attr(terms.formula(formula),"variables"))
node=grep("nodefactor",attr(terms.formula(formula),"variables"))

ids=ids[ids %in% node]

charac.mix=as.character(attr(terms.formula(formula),"variables")[[ids]])
if(length(charac.mix)==3){base.name.factor[[i]]=charac.mix[[3]]}

 }
}

if (length(base.name.factor)<i ){base.name.factor[[i+1]]="hold"}



#getting if nodemtach is differential diff
node.diff=NA

for ( i in 1:length(var.name.characs)){
ids=grep(var.name.characs[i],attr(terms.formula(formula),"variables"))
node=grep("diff",attr(terms.formula(formula),"variables"))
ids=ids[ids %in% node]
node.diff[i]=ifelse(length(ids)>0,TRUE,FALSE)
}




if (is.null(max.alter)){max.alter=max(unlist(lapply(var.name.characs.alter,length)))}

miss.ids=unlist(lapply(lapply(var.name.characs.alter,is.na),sum))
miss.ids=which(miss.ids==1)

for (i in miss.ids){

var.name.characs.alter[[i]]=rep(var.name.characs[i],max.alter)
}

add.trans=F
case.control.data=case.control.data.function(data=data,var.name.degree=var.name.degree,var.name.characs=var.name.characs,
case.control.type=case.control.type,var.name.characs.alter=var.name.characs.alter,add.trans=add.trans,resp.weights=resp.weights
,output.weights=output.weights,max.control.data.N=max.control.data.N,var.name.characs.person2=var.name.characs.person2
,control.type=control.type,tie.data=tie.data,remove.isolates.control=remove.isolates.control
,output.ids=output.ids,id.names=id.names,one_one_match_withreplace=one_one_match_withreplace
,resp.control.weights=resp.control.weights,N.type=N.type,case.type=case.type,sim.net=sim.net)


deg=NA
for ( i in 1:length(var.name.characs)){

deg[i]=all.equal(data[,var.name.characs[i]],data[,var.name.degree])[1]
}


id.deg=which(deg==TRUE)
if (length(id.deg)>0){

max.deg=max(data[,var.name.characs[id.deg]],na.rm=T)

case.control.data[case.control.data[,paste(var.name.characs[id.deg],1,sep="")]>max.deg,paste(var.name.characs[id.deg],1,sep="")]=max.deg
case.control.data[case.control.data[,paste(var.name.characs[id.deg],2,sep="")]>max.deg,paste(var.name.characs[id.deg],2,sep="")]=max.deg

}


for (i in 1:length(var.name.characs)){
if (!(is.na(var.type[i]))){

if (var.type[i]=="absdiff") {
case.control.data[,paste("absdiff",".",var.name.characs[i],sep="")]=abs(case.control.data[,paste(var.name.characs[i],"1",sep="")]-case.control.data[,paste(var.name.characs[i],"2",sep="")])
     }

if (var.type[i]=="nodematch") {

 if (node.diff[i]==F){
case.control.data[,paste("nodematch.",var.name.characs[i],sep="")]=as.numeric(as.character(case.control.data[,paste(var.name.characs[i],"1",sep="")])==as.character(case.control.data[,paste(var.name.characs[i],"2",sep="")]))
                 }

 if (node.diff[i]==T){#creating factor appropriate for differential nodematch 

match=as.numeric(as.character(case.control.data[,paste(var.name.characs[i],"1",sep="")])==as.character(case.control.data[,paste(var.name.characs[i],"2",sep="")]))

case.control.data[,paste("nodematch.",var.name.characs[i],".",sep="")]=as.character(case.control.data[,paste(var.name.characs[i],"1",sep="")])

case.control.data[match==0&is.na(match)==F&is.na(case.control.data[,paste("nodematch.",var.name.characs[i],".",sep="")])==F,
paste("nodematch.",var.name.characs[i],".",sep="")]="no match"


case.control.data[is.na(case.control.data[,paste(var.name.characs[i],"1",sep="")])|is.na(case.control.data[,paste(var.name.characs[i],"2",sep="")]),paste("nodematch.",var.name.characs[i],".",sep="")]=NA

case.control.data[,paste("nodematch.",var.name.characs[i],".",sep="")]=as.factor(case.control.data[,paste("nodematch.",var.name.characs[i],".",sep="")])
case.control.data[,paste("nodematch.",var.name.characs[i],".",sep="")]=relevel(case.control.data[,paste("nodematch.",var.name.characs[i],".",sep="")],"no match")
#relevling to make "no match" the base
                 }
   }#nodematch

if (var.type[i]=="nodemix"){
temp.dat1=(data.frame(case.control.data[,paste(var.name.characs[i],"1",sep="")],case.control.data[,paste(var.name.characs[i],"2",sep="")]))
#id=temp.dat1[,1]==temp.dat1[,2]
#id.not=!id

n.mix=unique(c(names(table(temp.dat1[,1])),names(table(temp.dat1[,2]))))
r.mix=rank(n.mix)

temp.dat=temp.dat1
temp.dat[,1]=as.numeric(as.character(factor(temp.dat1[,1],levels=n.mix,labels=r.mix)))
temp.dat[,2]=as.numeric(as.character(factor(temp.dat1[,2],levels=n.mix,labels=r.mix)))


id2=temp.dat[,1]>temp.dat[,2]
#id1=temp.dat[,1]<temp.dat[,2]
rm(temp.dat);gc()

id2[is.na(id2)]=FALSE

t2=temp.dat1[id2,2]
t1=temp.dat1[id2,1]


keep2=as.character(temp.dat1[,2])
keep2[id2]=as.character(t1)

keep1=as.character(temp.dat1[,1])
keep1[id2]=as.character(t2)

#temp.dat1[id2,2]=t1
#temp.dat1[id2,1]=t2

var=paste(keep1,keep2,sep=".")



case.control.data[,paste("mix.",var.name.characs[i],".",sep="")]= var
rm(temp.dat1);gc();gc()
#taking out NA's

case.control.data[is.na(case.control.data[,paste(var.name.characs[i],"1",sep="")])|is.na(case.control.data[,paste(var.name.characs[i],"2",sep="")]),paste("mix.",var.name.characs[i],".",sep="")]=NA
case.control.data[,paste("mix.",var.name.characs[i],".",sep="")]=as.factor(case.control.data[,paste("mix.",var.name.characs[i],".",sep="")])

if (!is.null(base.name[[i]])){
case.control.data[,paste("mix.",var.name.characs[i],".",sep="")]=relevel(case.control.data[,paste("mix.",var.name.characs[i],".",sep="")],base.name[[i]])}
    }#mix
  }
}#i


#dealing with node factors
for (i in 1:length(var.nodefactor)){

if (var.nodefactor[i]==FALSE){}

if (var.nodefactor[i]==TRUE){

case.control.data[,paste(var.name.characs[i],1,sep="")]=factor(case.control.data[,paste(var.name.characs[i],1,sep="")])

if (!is.null(base.name.factor[[i]])){
case.control.data[,paste(var.name.characs[i],1,sep="")]=relevel(case.control.data[,paste(var.name.characs[i],1,sep="")],ref=base.name.factor[[i]])
  }
}

}


if (length(miss.ids)>0){

var.miss=paste(var.name.characs[miss.ids],2,sep="")
for (i in 1:length(var.miss)){
col.id=-which(colnames(case.control.data)==var.miss[i])
 if (length(col.id)>0){
 case.control.data=case.control.data[,col.id]}
  }

var.miss=paste("absdiff",".",var.name.characs[miss.ids],sep="")
for (i in 1:length(var.miss)){
col.id=-which(colnames(case.control.data)==var.miss[i])
 if (length(col.id)>0){
 case.control.data=case.control.data[,col.id]}
  }

var.miss=paste("nodematch",".",var.name.characs[miss.ids],sep="")
for (i in 1:length(var.miss)){
col.id=-which(colnames(case.control.data)==var.miss[i])
 if (length(col.id)>0){
 case.control.data=case.control.data[,col.id]}
  }

var.miss=paste("mix",".",var.name.characs[miss.ids],".",sep="")
for (i in 1:length(var.miss)){
col.id=-which(colnames(case.control.data)==var.miss[i])
 if (length(col.id)>0){
 case.control.data=case.control.data[,col.id]}
  }

}


var.nodecov.factor=var.nodecov+var.nodefactor


form.list=list()
for (i in 1:length(var.name.characs)){

if (is.na(var.type[[i]])) {form.list[[i]]=paste(var.name.characs[i],1,sep="")

}

if (!is.na(var.type[[i]]) & !(i%in%miss.ids)  ){
if (var.type[i]=="absdiff"){

form.list[[i]]=ifelse(var.nodecov.factor[i],paste(paste(var.type[i],".",var.name.characs[i],sep=""),paste(var.name.characs[i],1,sep=""),sep="+"),
paste(var.type[i],".",var.name.characs[i],sep="")  )
}

if (var.type[i]=="nodematch"){
   if (node.diff[i]==F){
form.list[[i]]=ifelse(var.nodecov.factor[i],paste(paste(var.type[i],".",var.name.characs[i],sep=""),paste(var.name.characs[i],1,sep=""),sep="+"),
paste(var.type[i],".",var.name.characs[i],sep=""))
                 }

   if (node.diff[i]==T){
form.list[[i]]=ifelse(var.nodecov.factor[i],paste(paste(var.type[i],".",var.name.characs[i],".",sep=""),paste(var.name.characs[i],1,sep=""),sep="+"),
paste(var.type[i],".",var.name.characs[i],".",sep=""))
                 }

         }#nodematch

if (var.type[i]=="nodemix"){
form.list[[i]]=ifelse(var.nodecov.factor[i],paste(paste("mix.",var.name.characs[i],".",sep=""),paste(var.name.characs[i],1,sep=""),sep="+"),
paste("mix.",var.name.characs[i],".",sep=""))
        }
}

if (!is.na(var.type[[i]]) & (i%in%miss.ids)  ){
form.list[[i]]=ifelse(var.nodecov.factor[i],paste(var.name.characs[i],1,sep=""),"none")

}

if (i ==1){ind.vars=form.list[[i]]}
if (i>1 &form.list[[i]]!="none"){ ind.vars=paste(ind.vars,form.list[[i]],sep="+")}
 
}

form<-formula(paste("y","~",ind.vars))

list(case.control.data=case.control.data,formula=form)

}
