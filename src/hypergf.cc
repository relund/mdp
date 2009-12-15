#include "hypergf.hh"

// -----------------------------------------------------------------------------

Hypergraph::Hypergraph(char filename[12])
{
	memAllocated = false;
	pReader = new HgfReader(this);
	pReader->ReadSizes(filename);
	AllocateMem();
	pReader->ReadInHgf();
	delete pReader;
	ResetHgf();
	NormalizeMult();
}

// -----------------------------------------------------------------------------

void Hypergraph::AllocateMem()
{
	idx i;
	if (!memAllocated) {
		itsNodes = new Node[n+2];
		itsArcs = new Arc[ma+2];
		itsHArcs = new Hyperarc[mh+2];
		itsTails = new Tail[htailsize+2];
		itsFSAs = new ArcPtr[ma+2];
		itsFSHs = new HArcPtr[htailsize+2];
		for (i=0; i<n+2; i++) itsNodes[i].SetSize(sizeW,sizePred,sizeWTmp);
		for (i=0; i<ma+2; i++) itsArcs[i].SetSize(sizeW);
		for (i=0; i<mh+2; i++) itsHArcs[i].SetSize(sizeW);
		for (i=0; i<htailsize+2; i++) itsTails[i].SetSize(sizeMult);
		memAllocated = true;
	}
}

// -----------------------------------------------------------------------------

Hypergraph::~Hypergraph()
{
	if (memAllocated) {
		delete [] itsNodes;
		delete [] itsArcs;
		delete [] itsTails;
		delete [] itsHArcs;
		delete [] itsFSAs;
		delete [] itsFSHs;
	}
}

//-----------------------------------------------------------------------------

void Hypergraph::Initialize(uInt n, uInt ma, uInt mh, uInt d, uInt hSize,
	uInt sizeW, uInt sizeWTmp, uInt sizePred, uInt sizeMult) {
	this->n = n;
	this->ma = ma;
	this->mh = mh;
	this->d = d;
	this->hsize = hSize;
	this->sizeW = sizeW;
	this->sizeWTmp = sizeWTmp;
	this->sizePred = sizePred;
	this->sizeMult = sizeMult;
	this->htailsize = this->hsize - this->mh;
	this->size = this->hsize + (2*this->ma);
	//cout << this->n << " " << this->sizeMult << endl;
	pReader = new HgfReader(this);
	AllocateMem();
	pReader->Initialize();
}

//-----------------------------------------------------------------------------

void Hypergraph::ResetWeights(bool all)
{
	idx i,j;

	if (all) {
		for(i=1;i<=n;i++) {
			for(j=0; j<itsNodes[i].w.size(); j++) itsNodes[i].w[j] = INF;   // set weights to infinity
			//for(j=0; j<itsNodes[i].wTmp.size(); j++) itsNodes[i].wTmp[j] = INF;
		}
	}
	else {
		for(i=1;i<=n;i++) {
			if (itsNodes[i].BSsize > 0) {
				for(j=0; j<itsNodes[i].w.size(); j++) itsNodes[i].w[j] = INF;   // set weights to infinity
				//for(j=0; j<itsNodes[i].wTmp.size(); j++) itsNodes[i].wTmp[j] = INF;
			}
		}
	}
}

//-----------------------------------------------------------------------------

void Hypergraph::PrintSizes() {
	cout << "Number of nodes                          : " << n << endl;
	cout << "Number of arcs                           : " << ma << endl;
	cout << "Number of harcs                          : " << mh << endl;
	cout << "Max tail size of hyperarcs               : " << d << endl;
	cout << "Total size of hypergraph                 : " << size << endl;
	cout << "\n";
}

//-----------------------------------------------------------------------------

void Hypergraph::SwapW(idx i,idx j)
{
	idx k;
	flt wTmp;

	for(k=1;k<=mh;k++)
	{
		wTmp=itsHArcs[k].w[i];
		itsHArcs[k].w[i] = itsHArcs[k].w[j];
		itsHArcs[k].w[j] = wTmp;
	}

	for(k=1;k<=ma;k++)
	{
		wTmp=itsArcs[k].w[i];
		itsArcs[k].w[i] = itsArcs[k].w[j];
		itsArcs[k].w[j] = wTmp;
	}
}

//-----------------------------------------------------------------------------
/*
void Hypergraph::SetMultipliersToAverage()
{
	int i;
	flt size;
	TailPtr pNow,pLast;

	for(i=1;i<=mh;i++)
	{
		size = itsHArcs[i].tailSize;
		for (pNow=itsHArcs[i].pTail,pLast=itsHArcs[i+1].pTail;
			 pNow!=pLast;pNow++)
		{
			 pNow->m[0] = 1/size;
		}
	}
}

//-----------------------------------------------------------------------------

void Hypergraph::SetMultipliersToOne()
{
	int i;
	TailPtr pNow,pLast;

	for(i=1;i<=mh;i++)
	{
		for (pNow=itsHArcs[i].pTail,pLast=itsHArcs[i+1].pTail;
			 pNow!=pLast;pNow++)
		{
			 pNow->m[0] = 1;
		}
	}
}



//-----------------------------------------------------------------------------

void Hypergraph::SetWeightsToW2()
{
	int i;

	for(i=1;i<=mh;i++)
		itsHArcs[i].len = itsHArcs[i].w[1];

	for(i=1;i<=ma;i++)
		itsArcs[i].len = itsArcs[i].w[1];
}
*/
//-----------------------------------------------------------------------------
/*
void Hypergraph::CreateSubHypgf(idxPtr pArcIndexes,idxPtr pHArcIndexes)
{
	idx index;
	int i;
	NodePtr pNode;
	ArcPtr pANow, pALast;
	HArcPtr pHNow,pHLast;

	ResetSubHypgf();        // now the subhypergf contains all (h)arcs

	// scan arc indexes
	for(i=1,index=pArcIndexes[i];index!=0;index=pArcIndexes[++i])
	{
		if (index>0)    // i.e. fix the arc
		{
			pNode = itsArcs[index].pHead; // finds head node of the arc
			if (pNode->isFixed) printf("Error: Try to fix 2 different arcs!!!\n");
			for(pANow=pNode->pAFirst,pALast=(pNode+1)->pAFirst;     // remove arcs in BS
				pANow!=pALast;pANow++)
					pANow->inSubHgf = false;
			for(pHNow=pNode->pHFirst,pHLast=(pNode+1)->pHFirst;     // remove harcs in BS
				pHNow!=pHLast;pHNow++)
					pHNow->inSubHgf = false;

			itsArcs[index].inSubHgf = true;
			pNode->isFixed = true;
			pNode->BSsizeInSubHgf = 1;
		}
		else            // i.e. remove the arc
		{
			pNode = itsArcs[-index].pHead;      // finds head node of the arc
			itsArcs[-index].inSubHgf = false;   // remove the arc
			if (!(pNode->isFixed)) pNode->BSsizeInSubHgf -= 1;
		}
	}

	// scan harc indexes
	for(i=1,index=pHArcIndexes[i];index!=0;index=pHArcIndexes[++i])
	{
		if (index>0)    // i.e. fix the harc
		{
			pNode = itsHArcs[index].pHead;    // finds head node of the harc
			if (pNode->isFixed) printf("Error: Try to fix 2 different arcs!!!\n");

			for(pANow=pNode->pAFirst,pALast=(pNode+1)->pAFirst;     // remove arcs in BS
				pANow!=pALast;pANow++)
					pANow->inSubHgf = false;
			for(pHNow=pNode->pHFirst,pHLast=(pNode+1)->pHFirst;     // remove harcs in BS
				pHNow!=pHLast;pHNow++)
					pHNow->inSubHgf = false;

			itsHArcs[index].inSubHgf = true;
			pNode->isFixed = true;  // the BS have been fixed
			pNode->BSsizeInSubHgf = 1;
		}
		else            // i.e. remove the harc
		{
			pNode = itsHArcs[-index].pHead;
			itsHArcs[-index].inSubHgf = false;    //remove the harc
			if (!(pNode->isFixed)) pNode->BSsizeInSubHgf -= 1;
		}
	}
}


//-----------------------------------------------------------------------------

int Hypergraph::BuildAcyclicSubHypgf(idxPtr pArcIndexes,
		  idxPtr pHArcIndexes,idxPtr pPredIndexes)
{
	idx index;
	int i;
	int minC;   // the least number of a node where the BS is changed
	NodePtr pNode;
	ArcPtr pANow, pALast;
	HArcPtr pHNow,pHLast;

	ResetAcyclicSubHypgf();  // now the subhypergf contains all (h)arcs
							 // and all optimal labels

	minC = n;

	// scan arc indexes
	for(i=1,index=pArcIndexes[i];index!=0;index=pArcIndexes[++i])
	{
		if (index>0)    // i.e. fix the arc
		{
			pNode = itsArcs[index].pHead; // finds head node of the arc
			//if (pNode->isFixed) printf("Error: Try to fix 2 different arcs!!!\n");
			for(pANow=pNode->pAFirst,pALast=(pNode+1)->pAFirst;     // remove arcs in BS
				pANow!=pALast;pANow++)
					pANow->inSubHgf = false;
			for(pHNow=pNode->pHFirst,pHLast=(pNode+1)->pHFirst;     // remove harcs in BS
				pHNow!=pHLast;pHNow++)
					pHNow->inSubHgf = false;

			itsArcs[index].inSubHgf = true;
			pNode->isFixed = true;
			pNode->BSsizeInSubHgf = 1;
		}
		else            // i.e. remove the arc
		{
			pNode = itsArcs[-index].pHead;      // finds head node of the arc
			itsArcs[-index].inSubHgf = false;   // remove the arc
			if (!(pNode->isFixed)) pNode->BSsizeInSubHgf -= 1;
		}
		minC = MIN(minC,NodeIndex(pNode));
	}

	// scan harc indexes
	for(i=1,index=pHArcIndexes[i];index!=0;index=pHArcIndexes[++i])
	{
		if (index>0)    // i.e. fix the harc
		{
			pNode = itsHArcs[index].pHead;    // finds head node of the harc
			if (pNode->isFixed) printf("Error: Try to fix 2 different arcs!!!\n");

			for(pANow=pNode->pAFirst,pALast=(pNode+1)->pAFirst;     // remove arcs in BS
				pANow!=pALast;pANow++)
					pANow->inSubHgf = false;
			for(pHNow=pNode->pHFirst,pHLast=(pNode+1)->pHFirst;     // remove harcs in BS
				pHNow!=pHLast;pHNow++)
					pHNow->inSubHgf = false;

			itsHArcs[index].inSubHgf = true;
			pNode->isFixed = true;  // the BS have been fixed
			pNode->BSsizeInSubHgf = 1;
		}
		else            // i.e. remove the harc
		{
			pNode = itsHArcs[-index].pHead;
			itsHArcs[-index].inSubHgf = false;    //remove the harc
			if (!(pNode->isFixed)) pNode->BSsizeInSubHgf -= 1;
		}
		minC = MIN(minC,NodeIndex(pNode));
	}

	// scan pred indexes
	for(i=1,index=pPredIndexes[i];index!=0;index=pPredIndexes[++i])
	{
		if (index>0)    // i.e.  harc
		{
			pNode = itsHArcs[index].pHead;    // finds head node of the harc
			pNode->pred[0] = index;
		}
		else            // if arc
		{
			pNode = itsArcs[-index].pHead;
			pNode->pred[0]=index;
		}
	}

	return minC;
}

// -----------------------------------------------------------------------------

void Hypergraph::CreateSubHypgf(idx indexBefore,idx indexNow)
{
	NodePtr pNode;
	ArcPtr pANow, pALast;
	HArcPtr pHNow,pHLast;

	if (!gPathBranch) ResetSBP();     // reset so SBP can be called

	if (indexNow>0) // if harc
	{
		pNode = itsHArcs[indexNow].pHead;
		itsHArcs[indexNow].inSubHgf = false;    //remove the harc
		pNode->BSsizeInSubHgf -= 1;
	}
	if (indexNow<0)   // arc
	{
		pNode = itsArcs[-indexNow].pHead;
		itsArcs[-indexNow].inSubHgf = false;    //remove the arc
		pNode->BSsizeInSubHgf -= 1;
	}

	if (indexBefore>0)  // if harc we have to fix
	{
		pNode = itsHArcs[indexBefore].pHead;    // finds head node of the harc
		if (pNode->isFixed) printf("Error: Try to fix 2 different arcs!!!\n");
		// remove BS
		for(pANow=pNode->pAFirst,pALast=(pNode+1)->pAFirst;     // remove arcs in BS
			pANow!=pALast;pANow++)
				pANow->inSubHgf = false;
		for(pHNow=pNode->pHFirst,pHLast=(pNode+1)->pHFirst;     // remove harcs in BS
			pHNow!=pHLast;pHNow++)
				pHNow->inSubHgf = false;

		pNode->isFixed = true;
		pNode->BSsizeInSubHgf = 1;
		itsHArcs[indexBefore].inSubHgf = true;  //fix the harc
	}
	if (indexBefore<0)   // arc we have to fix
	{
		pNode = itsArcs[-indexBefore].pHead; // finds head node of the arc
		if (pNode->isFixed) printf("Error: Try to fix 2 different arcs!!!\n");
		for(pANow=pNode->pAFirst,pALast=(pNode+1)->pAFirst;     // remove arcs in BS
			pANow!=pALast;pANow++)
				pANow->inSubHgf = false;
		for(pHNow=pNode->pHFirst,pHLast=(pNode+1)->pHFirst;     // remove harcs in BS
			pHNow!=pHLast;pHNow++)
				pHNow->inSubHgf = false;

		pNode->isFixed = true;
		pNode->BSsizeInSubHgf = 1;
		itsArcs[-indexBefore].inSubHgf = true;  //fix the arc
	}
}
*/

// ----------------------------------------------------------------------------

void Hypergraph::NormalizeMult()
{
	idx i;
	TailPtr pNow,pLast;
	flt total,check;

	for (i=1;i<=mh;i++)
	{
		total=check=0;
		for (pNow=itsHArcs[i].pTail,pLast=itsHArcs[i+1].pTail;
			pNow!=pLast;pNow++ ) // scan tail nodes
				total+=pNow->m[0];

		for (pNow=itsHArcs[i].pTail,pLast=itsHArcs[i+1].pTail;
			pNow!=pLast;pNow++ ) // scan tail nodes again
			{
				pNow->m[0]=pNow->m[0]/total;
				//check+=pNow->m[0];
			}
			//printf("%4.2f\n",check);
	}

}

// ----------------------------------------------------------------------------

void Hypergraph::PrintArcs()
{
	idx i;
	cout << "Arcs:\n";
	for(i=1;i<=ma;i++) PrintArc(i);
	cout << "HArcs:\n";
	for(i=1;i<=mh;i++) PrintHArc(i);
}

// ----------------------------------------------------------------------------

void Hypergraph::PrintArc(idx i)
{
	cout.setf(ios::fixed);
	//cout.precision(3);
	cout << itsArcs[i].pHead-itsNodes << " <- " << itsArcs[i].pTail-itsNodes << " w = {";
	for(idx j=0; j<(idx)sizeW-1; j++) cout << itsArcs[i].w[j] << ",";
	cout << itsArcs[i].w[sizeW-1] << "} label=" << *(itsArcs[i].pLabel) << "\n";
}

// ----------------------------------------------------------------------------

void Hypergraph::PrintBSArc(int i)
{
	ArcPtr pANow,pALast;

	for(pANow=itsNodes[i].pAFirst,pALast=itsNodes[i+1].pAFirst;
			pANow!=pALast;pANow++)
	{
		cout << pANow->pHead-itsNodes << " " <<-(pANow->pTail-itsNodes);
		for (idx j=0; j<pANow->w.size(); j++) cout << " " << pANow->w[j];
		cout << "\n";
	}
}

// ----------------------------------------------------------------------------

void Hypergraph::PrintFSArc(int i)
{
	int j;
	ArcPtr* ppANow;
	ArcPtr* ppALast;

	for(ppANow=itsNodes[i].ppAFirst,ppALast=itsNodes[i+1].ppAFirst;
			ppANow!=ppALast;ppANow++)
	{
		j = (*ppANow)-itsArcs;
		PrintArc(j);
	}
}

// ----------------------------------------------------------------------------

void Hypergraph::PrintHArc(idx i)
{
	TailPtr pTailIndex,pLast;

	cout << itsHArcs[i].pHead-itsNodes << " <- {";
	for(pTailIndex=itsHArcs[i].pTail,pLast=itsHArcs[i+1].pTail;
		pTailIndex!=pLast-1;pTailIndex++)
		cout << pTailIndex->pTail-itsNodes << ",";
	cout << pTailIndex->pTail-itsNodes;
	cout << "} w = {";
	for (idx j=0; j<(idx)sizeW-1; ++j) cout << itsHArcs[i].w[j] << ",";
	cout << itsHArcs[i].w[sizeW-1] << "} label=" << *(itsHArcs[i].pLabel) << "\n";
}

// ----------------------------------------------------------------------------

void Hypergraph::PrintFSHArc(int i)
{
	int j;
	HArcPtr* ppHNow;
	HArcPtr* ppHLast;

	for(ppHNow=itsNodes[i].ppHFirst,ppHLast=itsNodes[i+1].ppHFirst;
			ppHNow!=ppHLast;ppHNow++)
	{
		j = (*ppHNow)-itsHArcs;
		PrintHArc(j);
	}
}

// ----------------------------------------------------------------------------

/*void Hypergraph::PrintBSHArc(int i)
{
	HArcPtr pHNow,pHLast;
	TailPtr pTailIndex,pLast;

	for(pHNow=itsNodes[i].pHFirst,pHLast=itsNodes[i+1].pHFirst;     // scan BS and remove BS
			pHNow!=pHLast;pHNow++)
	{
		printf("%d ",pHNow->pHead-itsNodes);
		for(pTailIndex=pHNow->pTail,pLast=(pHNow+1)->pTail;
		pTailIndex!=pLast;pTailIndex++)
			printf("%d ",-(pTailIndex->pTail-itsNodes));
		printf("tailsize: %d weight: %f\n",pHNow->tailSize,pHNow->w[0]);
	}
}*/

// ----------------------------------------------------------------------------

void Hypergraph::FixHArc(int idxHArc) {
	NodePtr pN = NULL;
	ArcPtr pArcNow,pLastArc;
	HArcPtr pHNow,
	pLastHArc;
	if (idxHArc==0) return;
	if (idxHArc<0) pN = itsArcs[-idxHArc].pHead;
	if (idxHArc>0) pN = itsHArcs[idxHArc].pHead;
	for (pArcNow=pN->pAFirst, pLastArc=(pN+1)->pAFirst;
			pArcNow!=pLastArc; pArcNow++)
		pArcNow->inSubHgf = false;
	for (pHNow=pN->pHFirst, pLastHArc=(pN+1)->pHFirst;
			pHNow!=pLastHArc; pHNow++)
		pHNow->inSubHgf = false;
	if (idxHArc<0) itsArcs[-idxHArc].inSubHgf = true;
	if (idxHArc>0) itsHArcs[idxHArc].inSubHgf = true;
	pN->BSsizeInSubHgf = 1;
}

// ----------------------------------------------------------------------------

void Hypergraph::RemoveHArc(int idxHArc) {
	NodePtr pN = NULL;
	if (idxHArc==0) return;
	if (idxHArc<0) pN = itsArcs[-idxHArc].pHead;
	if (idxHArc>0) pN = itsHArcs[idxHArc].pHead;
	pN->BSsizeInSubHgf--;
	if (idxHArc<0) itsArcs[-idxHArc].inSubHgf = false;
	if (idxHArc>0) itsHArcs[idxHArc].inSubHgf = false;
}

// ----------------------------------------------------------------------------

void Hypergraph::SetPred(int idxHArc, idx idxPred) {
	NodePtr pN = NULL;
	if (idxHArc==0) return;
	if (idxHArc<0) pN = itsArcs[-idxHArc].pHead;
	if (idxHArc>0) pN = itsHArcs[idxHArc].pHead;
	pN->pred[idxPred] = idxHArc;
}

// ----------------------------------------------------------------------------

