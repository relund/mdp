#include "hgfreader.hh"
#include "hypergf.hh"

// -----------------------------------------------------------------------------

void HgfReader::AllocateMem() {
	idx i;
	// Creates tempory arrays. Note store from entry 1
	pTmpTails  = new int[pH->ma+1];          // for storing arc tail numbers
	pTmpHeads  = new int[pH->ma+1];          // for storing arc head numbers
	pTmpHarcs  = new int[pH->d];             // used when reads one (h)arc
	pTmpW      = new flt[pH->sizeW];         // used to store weights of one (h)arc
	pTmpACosts = new fltPtr[pH->ma+1];     // for storing arc costs
	for (i=0;i<pH->ma+1;i++) pTmpACosts[i] = new flt[pH->sizeW];    // can now access using pTmpACosts[i][j]
	pTmpHACosts = new fltPtr[pH->mh+1]; // for storing harc costs
	for (i=0;i<pH->mh+1;i++) pTmpHACosts[i] = new flt[pH->sizeW];    // can now access using pTmpHACosts[i][j]
	pTmpTailSize = new int[pH->n+1];         // for storing the total tail size of hyperarcs in BS(i)
	pTmpHHeads = new int[pH->mh+1];          // for storing harc heads
	pTmpHTails = new int[pH->htailsize+1];       // for storing harc tail numbers
	pTmpHIndex = new int[pH->mh+1];          // for storing the index in pHTails containing the last tail number.
	pTmpHMult = new intPtr[pH->htailsize+1];;    // for storing multipliers
	for (i=0;i<pH->htailsize+1;i++) pTmpHMult[i] = new int[pH->sizeMult];    // can now access using pTmpHMult[i][j]

	// initilalization
	for (i=1;i<=pH->n;i++) pTmpTailSize[i]=0;
	narcs = 1;              // current number of arcs to be read
	arcIndex = 1;           // current arc index in temp arc arrays
	harcIndex = 1;          // current harc index in temp pTmpHHeads, pTmpHIndex
	htailsIndex = 1;        // current index in pTmpHTails
}

// -----------------------------------------------------------------------------

void HgfReader::DeallocateMem() {
	delete [] pTmpTails ;
	delete [] pTmpHeads;
	delete [] pTmpHarcs;
	delete [] pTmpW;
	for (idx i = 0; i < pH->ma+1; i++) delete [] pTmpACosts[i];
	delete [] pTmpACosts;
	for (idx i = 0; i < pH->mh+1; i++) delete [] pTmpHACosts[i];
	delete [] pTmpHACosts;
	delete [] pTmpTailSize;
	delete [] pTmpHHeads;
	delete [] pTmpHTails ;
	delete [] pTmpHIndex ;
	for (idx i = 0; i < pH->htailsize+1; i++) delete [] pTmpHMult[i];
	delete [] pTmpHMult;
}

// -----------------------------------------------------------------------------

char HgfReader::ReadSizes(char filename[12]) {
	bool more = true;
	bool recognized = false;
	char c;

	fstream = fopen(filename,"r");  // open the file for reading
	if ( fstream == NULL ) {
		cout << "The file could not be opened\n";
		exit(1);
	}
	cout << "Reading hypergraph from file " << filename << ":\n";
	cout << "Read hypergraph sizes (f 6 format) ... ";
	do {
		c=fgetc(fstream);
		if (c =='f') {
			for (;c!='\n';c=fgetc(fstream)) {
				if ( (c=='2') | (c=='3') | (c=='4') | (c=='5') | (c=='6') ) {
					recognized = true;
					break;
				}
			}
			if (recognized) {
				fscanf(fstream,"%d", &(pH->n));
				fscanf(fstream,"%d", &(pH->ma));
				fscanf(fstream,"%d", &(pH->mh));
				fscanf(fstream,"%d", &(pH->d));
				fscanf(fstream,"%d", &(pH->hsize));
				if (c=='6') {
					fscanf(fstream,"%u", &(pH->sizeW));
					fscanf(fstream,"%u", &(pH->sizeMult));
					pH->sizeWTmp = 0;
					pH->sizePred = 1;
				}
				more = false;
			} else {
				cout << "Wrong inputfile format.\n";
				exit(1);
			};
		} else
			if ( (c == 'c') | (c=='#') ) for (; fgetc(fstream) != '\n';);
			else {
				cout << "Error: Wrong comment syntax.\n";
				exit(1);
			};
	} while (more);
	cout << "done\n\n";

	pH->m = pH->mh + pH->ma;
	pH->htailsize = pH->hsize - pH->mh;
	pH->size = pH->hsize + (2*pH->ma);

	pH->PrintSizes();
	return c;
}

//-----------------------------------------------------------------------------

void HgfReader::ReadInHgf() {
	int head = 0;           // reads the head node number
	int name;               // reads node name/number
	idx i,j;                // counter

	cpuTime.StartTime(0);    // Start time
	AllocateMem();

	//cout << "Read (hyper)arcs ... ";

	//--- start reading (hyper)arcs  -----------------
	do {
		tailSizeIndex=0;    // Reset pTmpHarcs to put in entry 0
		do  // Reads the (hyper)arc. NOTE: does not assume the head node to be the
		{   // first index. However, if more than one positive entry is found, only
			// the last one becomes the head.
			fscanf(fstream,"%d", &name);
			if (name<0) {
				pTmpHarcs[tailSizeIndex++]=-name;   //if tail node add to tmp int array
				pH->itsNodes[-name].FSsize+=1;  // increse FS size
			} else if (name>0) head=name;        //if head node
		} while (name!=0);

		(pH->itsNodes[head].BSsize)+=1;  // increase BSsize

		for (i=0;i<pH->sizeW;i++) {
			fscanf(fstream,"%lf", &pTmpW[i]);       //reads the weight
		}

		if (tailSizeIndex==1) {     // an arc has been read: copy data into temporary arrays
			pTmpTails[arcIndex]=pTmpHarcs[0];
			pTmpHeads[arcIndex]=head;
			for (i=0;i<pH->sizeW;i++) pTmpACosts[arcIndex][i]=pTmpW[i];
			arcIndex++;
		} else { // a hyperarc has been read: modify temp harc arrays
			pTmpTailSize[head]=pTmpTailSize[head]+tailSizeIndex;    // add the size of the tail
			pTmpHHeads[harcIndex] = head;
			for (i=0;i<pH->sizeW;i++) pTmpHACosts[harcIndex][i]=pTmpW[i];
			for (i=0;i<tailSizeIndex;i++) { // insert tails numbers
				pTmpHTails[htailsIndex]=pTmpHarcs[i];
				for (j=0;j<pH->sizeMult;j++) fscanf(fstream,"%d", &pTmpHMult[htailsIndex][j]);     // get multipliers in tail
				htailsIndex++;
			}
			pTmpHIndex[harcIndex] = htailsIndex-1;  // last index containing tailnumber
			harcIndex++;
		}
		narcs++;
	} while (narcs <= pH->m);
	// end of hypergraph reading loop ---------------------------

	CheckDimensions();        // Check if reading ok

	// Build BS arc rep
	BuildArcRepF6();
	BuildFSARep();     // Build FS arc rep
	BuildHarcRepF6();
	BuildFSHRep();

	//cout << " done.\n";
	//cout << "Cpu time for reading and building (sec)  : " << cpuTime.StopAndGetTotalTimeDiff(0) << endl << endl;

	fclose(fstream);
	DeallocateMem();
}

//-----------------------------------------------------------------------------

void HgfReader::AddHyperarcs(const string &strHgf) {
	int head = 0;           // reads the head node number
	int name;               // reads node name/number
	idx i,j;                // counter
	//cout << "----\n" << strHgf;
	istringstream sHgf(strHgf,istringstream::in);    // stream to read from
	string tmp;

	while (getline(sHgf, tmp)) {
		istringstream sLine(tmp,istringstream::in);
		tailSizeIndex=0;    // Reset pTmpHarcs to put in entry 0
		do  // Reads the (hyper)arc. NOTE: does not assume the head node to be the
		{   // first index. However, if more than one positive entry is found, only
			// the last one becomes the head.
			sLine >> name; //fscanf(fstream,"%d", &name);
			if (name<0) {
				pTmpHarcs[tailSizeIndex++]=-name;   //if tail node add to tmp int array
				pH->itsNodes[-name].FSsize+=1;  // increse FS size
			} else if (name>0) head=name;        //if head node
		} while (name!=0); // TODO (LRE#1#): Her var indsat & name<1000???

		(pH->itsNodes[head].BSsize)+=1;  // increase BSsize

		for (i=0;i<pH->sizeW;i++) {
			sLine >> pTmpW[i]; //fscanf(fstream,"%lf", &pTmpW[i]);       //reads the weight
		}

		if (tailSizeIndex==1) {     // an arc has been read: copy data into temporary arrays
			pTmpTails[arcIndex]=pTmpHarcs[0];
			pTmpHeads[arcIndex]=head;
			for (i=0;i<pH->sizeW;i++) pTmpACosts[arcIndex][i]=pTmpW[i];
			arcIndex++;
		} else { // a hyperarc has been read: modify temp harc arrays
			pTmpTailSize[head]=pTmpTailSize[head]+tailSizeIndex;    // add the size of the tail
			pTmpHHeads[harcIndex] = head;
			for (i=0;i<pH->sizeW;i++) pTmpHACosts[harcIndex][i]=pTmpW[i];
			for (i=0;i<tailSizeIndex;i++) { // insert tails numbers
				pTmpHTails[htailsIndex]=pTmpHarcs[i];
				for (j=0;j<pH->sizeMult;j++) sLine >> pTmpHMult[htailsIndex][j]; //fscanf(fstream,"%d", &pTmpHMult[htailsIndex][j]);     // get multipliers in tail
				htailsIndex++;
			}
			pTmpHIndex[harcIndex] = htailsIndex-1;  // last index containing tailnumber
			harcIndex++;
		}
		narcs++;
	}
	//cout << "---" << endl;
}

// -----------------------------------------------------------------------------

void HgfReader::CheckDimensions() {
	if ((htailsIndex-1) != pH->htailsize) {
		printf("Error: Wrong number of hyperarc tail items: %1d (expected: %1d)\n",
			   htailsIndex-1, pH->htailsize );
		exit(1);
	}

	if ( arcIndex != pH->ma+1 )
		printf("Wrong number of arcs: %1d (expected: %1d)\n",
			   arcIndex-1, pH->ma );

	if ( harcIndex != pH->mh+1 )
		printf("Wrong number of hyperarcs: %1d (expected: %1d)\n",
			   harcIndex-1, pH->mh );

	if ((arcIndex != pH->ma+1) || (harcIndex != pH->mh+1)) exit(1);
}

// ----------------------------------------------------------------------------

void HgfReader::BuildArcRepF6() {
	ArcPtr pArcIndex;       // index pointer for itsArcs
	idx head,i,j;

	// scan heads to compute arc BS lengths.
	// Remember in all nodes temp = 0 (initialzation)
	for (i=1; i<=pH->ma; i++) (pH->itsNodes + pTmpHeads[i])->temp++;
	// now itsNodes[i].temp contains the length of BS(i)

	// scan nodes to set pAFirst
	pArcIndex = pH->itsArcs+1;    //points at first item
	for (i=1; i<=pH->n; i++) {
		pH->itsNodes[i].pAFirst=pArcIndex;      // pFirst points at pArcIndex
		pArcIndex+=pH->itsNodes[i].temp;        // increse pArcIndex with number
		// in backward star
	}
	// set "pAFirst" for fictitious node n+1
	pH->itsNodes[pH->n+1].pAFirst = pH->itsArcs+(pH->ma+1);

	// now  scan heads to fill array itsArcs. NOTE: modifies "pFirst"
	for (i=1; i<=pH->ma; i++) {
		head = pTmpHeads[i];    // node number to find tail for
		pArcIndex = pH->itsNodes[head].pAFirst++;
		pArcIndex->pHead = pH->itsNodes+head;
		pArcIndex->pTail = pH->itsNodes+pTmpTails[i];
		for (j=0;j<pH->sizeW;j++) pArcIndex->w[j] = pTmpACosts[i][j];
	}

	// now, restore "pFirst" (subtract lengths, i.e. "temp")
	for (i=1; i<=pH->n; i++) pH->itsNodes[i].pAFirst -= pH->itsNodes[i].temp;
}

// ----------------------------------------------------------------------------

void HgfReader::BuildFSARep() {
	idx i;
	ArcPtr* ppFSANow;
	ArcPtr pArcNow;

	for ( i = 1; i <= pH->n; i++)
		pH->itsNodes[i].temp = 0;

	// scan pTmpTails to compute hyperarc FS lengths
	for (i=1; i<=pH->ma; i++)
		(pH->itsNodes+pTmpTails[i])->temp++;
	// now itsNodes[i].temp contains the number of FS arcs

	// scan nodes to  set "ppAFirst"
	ppFSANow = pH->itsFSAs+1;
	for (i=1; i<=pH->n; i++) {
		pH->itsNodes[i].ppAFirst=ppFSANow;
		ppFSANow+=pH->itsNodes[i].temp;
	}
	pH->itsNodes[pH->n+1].ppAFirst=pH->itsFSAs+(pH->ma+1);   // set up dummy node

	// now scan itsArcs to fill array itsFSAs. NOTE: modifies "ppAfirst"
	for (i=1; i<=pH->ma; i++) {
		pArcNow = pH->itsArcs + i;
		*(pArcNow->pTail->ppAFirst++) = pArcNow;
	}

	// now, restore "ppAFirst" (subtract lengths, i.e. "temp")
	for (i=1; i<=pH->n; i++) pH->itsNodes[i].ppAFirst -= pH->itsNodes[i].temp;
}

// ----------------------------------------------------------------------------

void HgfReader::BuildHarcRepF6() {
	HArcPtr pHArcIndex;     // pointer index of itsHArcs
	TailPtr pHtailIndex;    // pointer index of itsTails
	TailPtr* TailIndexArray;
	idx i,j,head;
	idx tailIndex;

	// set pointers to tails
	TailIndexArray = new TailPtr[pH->n+1];
	pHtailIndex = pH->itsTails+1;
	for (i=1;i<=pH->n;i++) {
		TailIndexArray[i] = pHtailIndex;
		pHtailIndex += pTmpTailSize[i];
	}
	// now TailIndexArray[i] points to the entry of node i's first tail

	for ( i = 1; i <= pH->n; i++) { // reset nodes
		(pH->itsNodes+i)->temp = 0;
	};

	// scan heads to compute harc BS lengths.
	for (i=1; i<=pH->mh; i++) (pH->itsNodes + pTmpHHeads[i])->temp++;
	// now itsNodes[i].temp contains the length of BS(i)

	// scan nodes to set pHFirst
	pHArcIndex = pH->itsHArcs+1;    //points at first item
	for (i=1; i<=pH->n; i++) {
		pH->itsNodes[i].pHFirst=pHArcIndex;    // pHFirst points at pHArcIndex
		pHArcIndex+=pH->itsNodes[i].temp;      // increse pArcIndex with number in backward star
	}
	// set "pHFirst" for fictitious node n+1
	pH->itsNodes[pH->n+1].pHFirst = pH->itsHArcs+(pH->mh+1);

	// now scan heads to fill array itsHArcs and itsTails.
	// NOTE: modifies "pHFirst"
	tailIndex = 1;
	for (i=1; i<=pH->mh; i++) {
		head = pTmpHHeads[i];    // node number to find tail for
		pHArcIndex = pH->itsNodes[head].pHFirst++;
		pHArcIndex->pHead = pH->itsNodes+head;
		for (j=0;j<pH->sizeW;j++) pHArcIndex->w[j] = pTmpHACosts[i][j];
		pHArcIndex->pTail = pHtailIndex = TailIndexArray[head];
		pHArcIndex->tailSize = pTmpHIndex[i]-tailIndex+1;
		for (;tailIndex<=(idx)pTmpHIndex[i];tailIndex++) {
			pHtailIndex->pTail = pH->itsNodes + pTmpHTails[tailIndex];
			for (j=0;j<pH->sizeMult;j++) pHtailIndex->m[j] = (flt)pTmpHMult[tailIndex][j];
			pHtailIndex++;
		}
		TailIndexArray[head]=pHtailIndex; // ready for next tail
	}
	// Set up dummy node
	pH->itsHArcs[pH->mh+1].pTail=pH->itsTails+(pH->htailsize+1);

	// now, restore "pHFirst" (subtract lengths, i.e. "temp")
	for (i=1; i<=pH->n; i++) pH->itsNodes[i].pHFirst -= pH->itsNodes[i].temp;

	delete [] TailIndexArray;
}

// ----------------------------------------------------------------------------

void HgfReader::BuildFSHRep() {
	idx i;
	TailPtr ppNow;
	TailPtr ppLast;
	HArcPtr* ppFSHNow;
	HArcPtr pHArcNow;

	for ( i = 1; i <= pH->n; i++)
		pH->itsNodes[i].temp = 0;

	// scan itsTails to compute hyperarc FS lengths
	for (i=1; i<=pH->htailsize; i++)
		(pH->itsNodes+pTmpHTails[i])->temp++;
	// now itsNodes[i].temp contains the number of FS harcs

	// scan nodes to  set "ppHFirst"
	ppFSHNow = pH->itsFSHs+1;
	for (i=1; i<=pH->n; i++) {
		pH->itsNodes[i].ppHFirst=ppFSHNow;
		ppFSHNow+=pH->itsNodes[i].temp;
	}
	pH->itsNodes[pH->n+1].ppHFirst=pH->itsFSHs+(pH->htailsize+1);    // set up dummy node

	// now scan itsTails to fill array itsFSHs. NOTE: modifies "ppHfirst"
	for (i=1; i<=pH->mh; i++) {
		pHArcNow = pH->itsHArcs+i;
		for (ppNow=pH->itsHArcs[i].pTail,ppLast=pH->itsHArcs[i+1].pTail;ppNow!=ppLast;
				ppNow++)
			*((ppNow->pTail)->ppHFirst++)=pHArcNow;
	}

	// now, restore "ppHFirst" (subtract lengths, i.e. "temp")
	for (i=1; i<=pH->n; i++) pH->itsNodes[i].ppHFirst -= pH->itsNodes[i].temp;
}






