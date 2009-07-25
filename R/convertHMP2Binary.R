#' Convert a HMDP model stored in a hmp (xml) file to binary file format.
#'
#' The function simply parse the hmp file and create binary files using
#' the \link{binaryMDPWriter}.
#'
#' @param file The name of the hmp file (e.g. mdp.hmp).
#' @param prefix A character string with the prefix which will be added to the binary files.
#' @return NULL (invisible).
#' @author Lars Relund \email{lars@@relund.dk}
#' @note Note all indexes are starting from zero (C/C++ style).
#' @seealso \link{binaryMDPWriter}.
#' @examples
#' f = system.file("inst/models", "mdp.xml", package = "MDP")
#' convertHMP2Binary("mdp.hmp")
#' stateIdxDf()
#' actionInfo()
convertHMP2Binary<-function(file, prefix="") {
	setWeights<-function(q) {
		labels<-unlist(lapply(q, function(x) xmlAttrs(x)))
		ctrW<<-length(labels)+1
		w$setWeights(c("Duration",labels))
	}

	stateCtr<-function(g) {
		length(xmlChildren(g))
	}

	process<-function(p) {
		w$process()
		states<-unlist(c(xmlApply(p,stateCtr),0))   # number of states in each stage
		for (i in 1:(length(states)-1)) stage(p[[i]],states[i+1])
		w$endProcess()
	}

	stage<-function(g,states) {
		w$stage()
		xmlApply(g,state,states=states)
		w$endStage()
	}

	state<-function(s,states) {
		w$state(label=xmlAttrs(s)['l'])
		xmlApply(s,action,states=states)
		w$endState()
	}

	# trim spaces in both ends
	trim<-function(x)
	{
		sub("[ \t\n\r]*$", "", sub("^[ \t\n\r]*", "", x))
	}

	action<-function(a,states) {
		if ("proc" %in% names(xmlChildren(a))) {
			w$action(label=xmlAttrs(a)['l'], weights=rep(0,ctrW), prob=c(2,0,1))
			xmlApply(a,process)
		} else {    # normal action
			v<-paste("c(", gsub(" +", ",", trim(xmlValue(a[['q']]))), ")",sep="")
			v<-eval(parse(text=v))
			d<-paste("c(", gsub(" +", ",", trim(xmlValue(a[['d']]))), ")",sep="")
			d<-eval(parse(text=d))
			if (length(d)>1) warning("More than one duration number in the action (see hmp file)! \nOnly one duration for each action is supported in the binary file format. \nUse the first one.", call.=FALSE)
			v<-c(d[1],v)
			type<-xmlAttrs(a[['p']])['t']
			pr<-paste("c(", gsub(" +", ",", trim(xmlValue(a[['p']]))), ")",sep="")
			pr<-eval(parse(text=pr))
			if (type=="s") {
				idx<-pr[1:length(pr)%%2==1]
				pr<-pr[1:length(pr)%%2==0]
				scp<-rep(1,ceiling(length(pr)/2))    # set scp to 1 (default)
			}
			if (type=="d") {
				idx<-pr[1]
				pr<-1
				scp<-1
			}
			if (type=="e") {
				idx<-1:length(pr)-1
				scp<-rep(1,length(pr))    # set scp to 1 (default)
			}
			for (i in 1:length(idx)) {
				if (idx[i]>=states) {
					scp[i]<-0
					idx[i]<-idx[i]-states
				}
			}
			i<-which(pr!=0)
			scp<-scp[i]
			idx<-idx[i]
			pr<-pr[i]
			pr<-as.numeric(rbind(scp,idx,pr))
			w$action(label=xmlAttrs(a)['l'], weights=v, prob=pr)
		}
		w$endAction()
	}

	ptm <- proc.time()
	ctrW<-0
	doc<-xmlTreeParse(file,useInternalNodes=T)
	r<-xmlRoot(doc)
	w<-binaryMDPWriter(prefix)
		setWeights(r['quantities',all=T])
		process(r[['proc']])
	w$closeWriter()
	free(doc)
	cat("Converted",file,"to binary format.\n\n")
	print(proc.time() - ptm)
	invisible(NULL)
}
