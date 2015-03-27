#include "htacyclic.hh"

//-----------------------------------------------------------------------------

void HTAcyclic::CalcHTacyclic(Hypergraph& H, idx idxW, idx idxPred,
								idx idxMult) {
	NodePtr pHnode;  // pointer to last node
	TailPtr pTailNow,pTailLast;
	ArcPtr pArcNow,     // pointer to the arc to examine
	pLastArc;   // pointer to the arc we have to examine to (not with)
	HArcPtr pHNow,
	pLastHArc;
	flt weightTmp;      // weight to compaire
	bool isMinInf;      // true if a hyperarc gives -INF in the head node

	// scan the valid ordering
	for (idx i=0; i<validOdr.size(); i++) {
		pHnode = H.GetNodesPtr()+validOdr[i];
		if (pHnode->BSsize>0) pHnode->w[idxW]= -INF;  // reset weight
		// (1) scan simple arcs in backward star
		for (pArcNow=pHnode->pAFirst, pLastArc=(pHnode+1)->pAFirst;
				pArcNow!=pLastArc; pArcNow++) {
			if (pArcNow->inSubHgf) { // if arc in the subhypergf
				weightTmp = pArcNow->pTail->w[idxW]+pArcNow->w[idxW];
				if (pHnode->w[idxW] < weightTmp) { // update node label and re-insert
					pHnode->w[idxW] = weightTmp;
					pHnode->pred[idxPred] = ArcIndexPred(pArcNow);
				}
			}
		}
		// (2) scan hyperarc backward star
		for (pHNow=pHnode->pHFirst, pLastHArc=(pHnode+1)->pHFirst;
				pHNow!=pLastHArc; pHNow++) {
			if (pHNow->inSubHgf) { // if a harc in the subhypergf
				weightTmp=0;
				isMinInf = false;
				// compute weighting function: scan tails
				for (pTailNow=pHNow->pTail,pTailLast=(pHNow+1)->pTail;
						pTailNow!=pTailLast;pTailNow++ ) {
					if ((pTailNow->pTail)->w[idxW]<= -INF) {
						weightTmp= -INF;
						isMinInf = true;
						break;
					}
					weightTmp += ((pTailNow->pTail)->w[idxW])
								 *(pTailNow->m[idxMult]);
				}
				if (isMinInf) continue; // if the (h)arc gives -INF goto next (h)arc
				weightTmp += pHNow->w[idxW];
				if (pHnode->w[idxW] < weightTmp) {
					pHnode->w[idxW] = weightTmp;
					pHnode->pred[idxPred] = HArcIndex(pHNow);
				}
			}
		}
	}
}

//-----------------------------------------------------------------------------

bool HTAcyclic::CalcHTacyclic(Hypergraph& H, idx idxW, idx idxPred,
					idx idxMult, idx idxDur, flt rate, flt rateBase) {
	NodePtr pHnode;  // pointer to last node
	TailPtr pTailNow,pTailLast;
	ArcPtr pArcNow,     // pointer to the arc to examine
	pLastArc;   // pointer to the arc we have to examine to (not with)
	HArcPtr pHNow,
	pLastHArc;
	flt weightTmp;      // weight to compaire
	bool isMinInf;      // true if a hyperarc gives -INF in the head node
	bool newPred = false;       // true if the stored pred change in a node
	flt dB = exp(-rate/rateBase);      // the discount base
	int oldPred;

	// scan the valid ordering
	for (idx i=0; i<validOdr.size(); i++) {
		pHnode = H.GetNodesPtr()+validOdr[i];
		if (pHnode->BSsize>0) pHnode->w[idxW]= -INF;  // reset weight
		//pHnode->SetPred(idxPred,0);
		oldPred = pHnode->pred[idxPred];
		//cout << "old:" << oldPred << " ";
		// (1) scan simple arcs in backward star
		for (pArcNow=pHnode->pAFirst, pLastArc=(pHnode+1)->pAFirst;
				pArcNow!=pLastArc; pArcNow++) {
			if (pArcNow->inSubHgf) { // if arc in the subhypergf
				weightTmp = pArcNow->pTail->w[idxW]*pow(dB,pArcNow->w[idxDur])+pArcNow->w[idxW];
				if (pHnode->w[idxW] < weightTmp) { // update node label and re-insert
					pHnode->w[idxW] = weightTmp;
					pHnode->pred[idxPred] = ArcIndexPred(pArcNow);
				}
			}
		}
		// (2) scan hyperarc backward star
		for (pHNow=pHnode->pHFirst, pLastHArc=(pHnode+1)->pHFirst;
				pHNow!=pLastHArc; pHNow++) {
			if (pHNow->inSubHgf) { // if a harc in the subhypergf
				weightTmp=0;
				isMinInf = false;
				// compute weighting function: scan tails
				for (pTailNow=pHNow->pTail,pTailLast=(pHNow+1)->pTail;
						pTailNow!=pTailLast;pTailNow++ ) {
					if ((pTailNow->pTail)->w[idxW]<= -INF) {
						weightTmp= -INF;
						isMinInf = true;
						break;
					}
					weightTmp += ((pTailNow->pTail)->w[idxW])
								 *(pTailNow->m[idxMult]);
				}
				if (isMinInf) continue; // if the (h)arc gives INF goto next (h)arc
				weightTmp = weightTmp*pow(dB,pHNow->w[idxDur])+pHNow->w[idxW];
				if (pHnode->w[idxW] < weightTmp) {
					pHnode->w[idxW] = weightTmp;
					pHnode->pred[idxPred] = HArcIndex(pHNow);
				}
			}
		}
		//cout << "new:" << pHnode->pred[idxPred] << endl;
		if (pHnode->pred[idxPred] != oldPred) newPred = true;
	}
	//cout << "newPred:" << newPred << endl;
	return newPred;
}

//-----------------------------------------------------------------------------

void HTAcyclic::CalcSubTreeValues(Hypergraph &H, idx idxPred,
	idx idxMult, idx idxDur, flt rate, flt rateBase) {
	NodePtr pNode;
	TailPtr pTailNow,pTailLast;
	ArcPtr pArc;
	HArcPtr pHArc;
	flt f;
	flt dB = exp(-rate/rateBase);      // the discount base

	// scan the valid ordering
	for (idx i=0; i<validOdr.size(); i++) {
		pNode = H.GetNodesPtr()+validOdr[i];
		if (pNode->pred[idxPred]<0) {
			pArc = H.itsArcs - pNode->pred[idxPred];
			pNode->f = pow(dB,pArc->w[idxDur])*pArc->pTail->f;
		}
		if (pNode->pred[idxPred]>0) {
			pHArc = H.itsHArcs + pNode->pred[idxPred];
			f = 0;
			for (pTailNow=pHArc->pTail,pTailLast=(pHArc+1)->pTail;
					pTailNow!=pTailLast; ++pTailNow) {
				f += ((pTailNow->pTail)->f)*(pTailNow->m[idxMult]);
			}
			pNode->f = pow(dB,pHArc->w[idxDur])*f;
		}
	}
}

//-----------------------------------------------------------------------------

void HTAcyclic::CalcSubTreeValues(Hypergraph &H, idx idxPred, idx idxMult) {
	NodePtr pNode;
	TailPtr pTailNow,pTailLast;
	ArcPtr pArc;
	HArcPtr pHArc;
	flt f;

	// scan the valid ordering
	for (idx i=0; i<validOdr.size(); i++) {
		pNode = H.GetNodesPtr()+validOdr[i];
		if (pNode->pred[idxPred]<0) {
			pArc = H.itsArcs - pNode->pred[idxPred];
			pNode->f = pArc->pTail->f;
		}
		if (pNode->pred[idxPred]>0) {
			pHArc = H.itsHArcs + pNode->pred[idxPred];
			f = 0;
			for (pTailNow=pHArc->pTail,pTailLast=(pHArc+1)->pTail;
					pTailNow!=pTailLast; ++pTailNow) {
				f += ((pTailNow->pTail)->f)*(pTailNow->m[idxMult]);
			}
			pNode->f = f;
		}
	}
}

//-----------------------------------------------------------------------------

void HTAcyclic::CalcOptW(Hypergraph& H,idx idxW, idx idxPred, idx idxMult) {
	NodePtr pHnode;     // pointer to node we are examing BS of
	TailPtr pTailNow,pTailLast;
	ArcPtr pArc;     // pointer to the arc to examine
	HArcPtr pHNow;
	flt weightTmp;      // weight to compaire

	// scan the valid ordering
	for (idx i=0; i<validOdr.size(); i++) {
		pHnode = H.GetNodesPtr()+validOdr[i];
		if (pHnode->pred[idxPred]<0) { // arc
			pArc = H.GetArcsPtr()-pHnode->pred[idxPred];
			pHnode->w[idxW] = pArc->pTail->w[idxW]+pArc->w[idxW];
		}
		if (pHnode->pred[idxPred] > 0) { // hyperarc
			pHNow = H.GetHArcsPtr() + pHnode->pred[idxPred];
			weightTmp=0;
			// compute weighting function: scan tails
			for (pTailNow=pHNow->pTail,pTailLast=(pHNow+1)->pTail;
					pTailNow!=pTailLast;pTailNow++ ) {
				weightTmp += ((pTailNow->pTail)->w[idxW])
							 *(pTailNow->m[idxMult]);
			}
			pHnode->w[idxW] = weightTmp + pHNow->w[idxW];
		}
	}
}

//-----------------------------------------------------------------------------

void HTAcyclic::CalcOptW(Hypergraph& H,vector<idx> vW, idx idxPred, idx idxMult) {
	NodePtr pHnode;     // pointer to node we are examing BS of
	TailPtr pTailNow,pTailLast;
	ArcPtr pArc;     // pointer to the arc to examine
	HArcPtr pHNow;
	flt weightTmp;      // weight to compare

	// scan the valid ordering
	for (idx i=0; i<validOdr.size(); i++) {
		pHnode = H.GetNodesPtr()+validOdr[i];
		if (pHnode->pred[idxPred]<0) { // arc
			pArc = H.GetArcsPtr()-pHnode->pred[idxPred];
			for(idx j=0; j<vW.size(); j++) {
				pHnode->w[vW[j]] = pArc->pTail->w[vW[j]]+pArc->w[vW[j]];
			}
		}
		if (pHnode->pred[idxPred] > 0) { // hyperarc
			pHNow = H.GetHArcsPtr() + pHnode->pred[idxPred];
			for(idx j=0; j<vW.size(); j++) {
				weightTmp=0;
				// compute weighting function: scan tails
				for (pTailNow=pHNow->pTail,pTailLast=(pHNow+1)->pTail;
						pTailNow!=pTailLast;pTailNow++ ) {
					weightTmp += ((pTailNow->pTail)->w[vW[j]])
								 *(pTailNow->m[idxMult]);
				}
				pHnode->w[vW[j]] = weightTmp + pHNow->w[vW[j]];
			}
		}
	}
}

//-----------------------------------------------------------------------------

void HTAcyclic::CalcOptWDiscount(Hypergraph& H,idx idxW, idx idxPred,
					idx idxMult, idx idxDur, flt rate, flt rateBase) {
	NodePtr pHnode;     // pointer to node we are examing BS of
	TailPtr pTailNow,pTailLast;
	ArcPtr pArc;     // pointer to the arc to examine
	HArcPtr pHNow;
	flt weightTmp;
	flt dB = exp(-rate/rateBase);      // the discount base

	// scan the valid ordering
	for (idx i=0; i<validOdr.size(); i++) {
		pHnode = H.GetNodesPtr()+validOdr[i];
		if (pHnode->pred[idxPred]<0) { // arc
			pArc = H.GetArcsPtr()-pHnode->pred[idxPred];
            pHnode->w[idxW] = pArc->pTail->w[idxW]*pow(dB,pArc->w[idxDur])+pArc->w[idxW];
		}
		if (pHnode->pred[idxPred] > 0) { // hyperarc
			pHNow = H.GetHArcsPtr() + pHnode->pred[idxPred];
            weightTmp=0;
            // compute weighting function: scan tails
            for (pTailNow=pHNow->pTail,pTailLast=(pHNow+1)->pTail;
                    pTailNow!=pTailLast;pTailNow++ ) {
                weightTmp += ((pTailNow->pTail)->w[idxW])
                             *(pTailNow->m[idxMult]);
            }
            pHnode->w[idxW] = weightTmp*pow(dB,pHNow->w[idxDur]) + pHNow->w[idxW];
		}
	}
}

//-----------------------------------------------------------------------------

void HTAcyclic::CalcOptWAve(Hypergraph& H,idx idxW, idx idxPred,
        idx idxMult, idx idxDur, flt g) {
	NodePtr pHnode;     // pointer to node we are examing BS of
	TailPtr pTailNow,pTailLast;
	ArcPtr pArc;     // pointer to the arc to examine
	HArcPtr pHNow;
	flt weightTmp;

	// scan the valid ordering
	for (idx i=0; i<validOdr.size(); i++) {
		pHnode = H.GetNodesPtr()+validOdr[i];
		if (pHnode->pred[idxPred]<0) { // arc
			pArc = H.GetArcsPtr()-pHnode->pred[idxPred];
            pHnode->w[idxW] = pArc->pTail->w[idxW]+pArc->w[idxW]- pArc->w[idxDur]*g;
		}
		if (pHnode->pred[idxPred] > 0) { // hyperarc
			pHNow = H.GetHArcsPtr() + pHnode->pred[idxPred];
            weightTmp=0;
            // compute weighting function: scan tails
            for (pTailNow=pHNow->pTail,pTailLast=(pHNow+1)->pTail;
                    pTailNow!=pTailLast;pTailNow++ ) {
                weightTmp += ((pTailNow->pTail)->w[idxW])
                             *(pTailNow->m[idxMult]);
            }
            pHnode->w[idxW] = weightTmp + pHNow->w[idxW] - pHNow->w[idxDur]*g;
		}
	}
}

//-----------------------------------------------------------------------------

void HTAcyclic::PrintTree(Hypergraph& H, idx idxPred) {
	NodePtr pNode,pLast;

	cout << endl << "Optimal hypertree:" << endl;
	for (pNode = H.GetNodesPtr()+H.Getn(), pLast = H.GetNodesPtr();
		pNode!=pLast;pNode--) { // scan the valid ordering.
		H.PrintNode(NodeIndex(pNode));
		cout << " - p: ";
		PrintPred(H, NodeIndex(pNode), idxPred);
		//cout << endl;
	}
	cout << endl;
}

//-----------------------------------------------------------------------------

void HTAcyclic::PrintPred(Hypergraph& H, idx idxNode, idx idxPred) {
	NodePtr pNode = H.GetNodesPtr() + idxNode;
	if (pNode->pred[idxPred] > 0) { // hyperarc
		H.PrintHArc(pNode->pred[idxPred]);
	}
	if (pNode->pred[idxPred] < 0) { // arc
		H.PrintArc(-pNode->pred[idxPred]);
	}
	if (pNode->pred[idxPred] == 0) cout << endl;
}

//-----------------------------------------------------------------------------

void HTAcyclic::FindValidOdr(Hypergraph& H, vector<idx> & nodes)
{
	//H.PrintArcs();

	//priority_queue<idx>::iterator it;
	NodePtr pTNode,pHNode;
	ArcPtr *ppArcNow, // pointer to the arc to examine
		   *ppLastArc,  // pointer to the arc we have to examine to (not with)
			pArc;
	HArcPtr *ppFirstHArc,
			*ppLastHArc,
			pHNow;

	vector<idx> tailHArc(H.mh+1,0);   // ctr to check if tails visited
	priority_queue<idx> cand(nodes.begin(),nodes.end());

	H.ResetWeights(0,0);    // use w[0] to store harcs visited
	validOdr.empty();
	while (!cand.empty())
	{
		pTNode = H.itsNodes + cand.top();
		cand.pop();
		//cout << "N:" << NodeIndex(pTNode) << " FS:" << endl << flush;
		validOdr.push_back(NodeIndex(pTNode));

		// scan arcs in forward star
		for(ppArcNow=pTNode->ppAFirst, ppLastArc=(pTNode+1)->ppAFirst;
			ppArcNow!=ppLastArc; ppArcNow++)
		{
			pArc = *ppArcNow;
			//H.PrintArc(ArcIndex(pArc));
			pHNode = pArc->pHead;
			(pHNode->w[0])++;
			if (pHNode->w[0]==pHNode->BSsize) { // all harcs in BS scanned
				cand.push(NodeIndex(pHNode));
			}
		}
		// scan harcs in forward star
		for (ppFirstHArc=pTNode->ppHFirst, ppLastHArc=(pTNode+1)->ppHFirst;
			ppFirstHArc!=ppLastHArc; ppFirstHArc+=1)
		{
			pHNow=*ppFirstHArc;     // pHArc is the harc we are examine
			//H.PrintHArc(HArcIndex(pHNow));
			tailHArc[HArcIndex(pHNow)]++;
			pHNode = pHNow->pHead;
			if (tailHArc[HArcIndex(pHNow)]==(idx)pHNow->tailSize) (pHNode->w[0])++;
			if (pHNode->w[0]==pHNode->BSsize) { // all harcs in BS scanned
				cand.push(NodeIndex(pHNode));
			}
		}
	}
	//PrintValidOdr();
}

//-----------------------------------------------------------------------------

flt HTAcyclic::CalcRPO(Hypergraph& H, idx idxW, idx idxMult, int idxHArc) {
	NodePtr pHnode = NULL;
	TailPtr pTailNow,pTailLast;
	ArcPtr pArcNow,
	pLastArc;           // pointer to the arc we have to examine to (not with)
	HArcPtr pHNow,
	pLastHArc;
	flt weightTmp;      // weight to compaire
	bool isMinInf;      // true if a hyperarc gives -INF in the head node
	flt wA = 0;         // weight the idxHArc
	flt wMax = -INF;    // max weight of the prececessor not equal idxHArc

	if (idxHArc==0) return 0;
	if (idxHArc<0) pHnode = H.itsArcs[-idxHArc].pHead;
	if (idxHArc>0) pHnode = H.itsHArcs[idxHArc].pHead;
	if (pHnode->BSsizeInSubHgf==1) return 0;
	for (pArcNow=pHnode->pAFirst, pLastArc=(pHnode+1)->pAFirst;
			pArcNow!=pLastArc; pArcNow++) {
		if (pArcNow->inSubHgf) { // if arc in the subhypergf
			weightTmp = pArcNow->pTail->w[idxW] + pArcNow->w[idxW];
			if (idxHArc<0) {
				if (&H.itsArcs[-idxHArc]==pArcNow) {   // if idxHArc
					wA = weightTmp;
					continue;
				}
			}
			wMax = max(wMax,weightTmp);
		}
	}
	for (pHNow=pHnode->pHFirst, pLastHArc=(pHnode+1)->pHFirst;
			pHNow!=pLastHArc; pHNow++) {
		if (pHNow->inSubHgf) { // if a harc in the subhypergf
			weightTmp=0;
			isMinInf = false;
			// compute weighting function: scan tails
			for (pTailNow=pHNow->pTail,pTailLast=(pHNow+1)->pTail;
					pTailNow!=pTailLast;pTailNow++ ) {
				if ((pTailNow->pTail)->w[idxW]<= -INF) {
					weightTmp= -INF;
					isMinInf = true;
					break;
				}
				weightTmp += ((pTailNow->pTail)->w[idxW])
							 *(pTailNow->m[idxMult]);
			}
			if (isMinInf) continue; // if the (h)arc gives -INF goto next (h)arc
			weightTmp += pHNow->w[idxW];
			if (idxHArc>0) {
				if (&H.itsHArcs[idxHArc]==pHNow) {   // if idxHArc
					wA = weightTmp;
					continue;
				}
			}
			wMax = max(wMax,weightTmp);
		}
	}
	return wA - wMax;
}

//-----------------------------------------------------------------------------

flt HTAcyclic::CalcRPODiscount(Hypergraph& H, idx idxW, idx idxMult,
	int idxHArc, idx idxDur, flt rate, flt rateBase)
{
	NodePtr pHnode = NULL;
	TailPtr pTailNow,pTailLast;
	ArcPtr pArcNow,
	pLastArc;           // pointer to the arc we have to examine to (not with)
	HArcPtr pHNow,
	pLastHArc;
	flt weightTmp;      // weight to compaire
	bool isMinInf;      // true if a hyperarc gives -INF in the head node
	flt wA = 0;         // weight the idxHArc
	flt wMax = -INF;    // max weight of the prececessor not equal idxHArc
	flt dB = exp(-rate/rateBase);      // the discount base

	if (idxHArc==0) return 0;
	if (idxHArc<0) pHnode = H.itsArcs[-idxHArc].pHead;
	if (idxHArc>0) pHnode = H.itsHArcs[idxHArc].pHead;
	if (pHnode->BSsizeInSubHgf==1) return 0;
	for (pArcNow=pHnode->pAFirst, pLastArc=(pHnode+1)->pAFirst;
			pArcNow!=pLastArc; pArcNow++) {
		if (pArcNow->inSubHgf) { // if arc in the subhypergf
			weightTmp = pArcNow->pTail->w[idxW]*pow(dB,pArcNow->w[idxDur])+pArcNow->w[idxW];
			if (idxHArc<0) {
				if (&H.itsArcs[-idxHArc]==pArcNow) {   // if idxHArc
					wA = weightTmp;
					continue;
				}
			}
			wMax = max(wMax,weightTmp);
		}
	}
	for (pHNow=pHnode->pHFirst, pLastHArc=(pHnode+1)->pHFirst;
			pHNow!=pLastHArc; pHNow++) {
		if (pHNow->inSubHgf) { // if a harc in the subhypergf
			weightTmp=0;
			isMinInf = false;
			// compute weighting function: scan tails
			for (pTailNow=pHNow->pTail,pTailLast=(pHNow+1)->pTail;
					pTailNow!=pTailLast;pTailNow++ ) {
				if ((pTailNow->pTail)->w[idxW]<= -INF) {
					weightTmp= -INF;
					isMinInf = true;
					break;
				}
				weightTmp += ((pTailNow->pTail)->w[idxW])
							 *(pTailNow->m[idxMult]);
			}
			if (isMinInf) continue; // if the (h)arc gives -INF goto next (h)arc
			weightTmp = weightTmp*pow(dB,pHNow->w[idxDur])+pHNow->w[idxW];
			if (idxHArc>0) {
				if (&H.itsHArcs[idxHArc]==pHNow) {   // if idxHArc
					wA = weightTmp;
					continue;
				}
			}
			wMax = max(wMax,weightTmp);
		}
	}
	return wA - wMax;
}

//-----------------------------------------------------------------------------

flt HTAcyclic::CalcRPOAve(Hypergraph& H, idx idxW, idx idxD, idx idxMult,
	int idxHArc, flt g)
{
	NodePtr pHnode = NULL;
	TailPtr pTailNow,pTailLast;
	ArcPtr pArcNow,
	pLastArc;           // pointer to the arc we have to examine to (not with)
	HArcPtr pHNow,
	pLastHArc;
	flt weightTmp;      // weight to compaire
	bool isMinInf;      // true if a hyperarc gives -INF in the head node
	flt wA = 0;         // weight the idxHArc
	flt wMax = -INF;    // max weight of the prececessor not equal idxHArc

	if (idxHArc==0) return 0;
	if (idxHArc<0) pHnode = H.itsArcs[-idxHArc].pHead;
	if (idxHArc>0) pHnode = H.itsHArcs[idxHArc].pHead;
	if (pHnode->BSsizeInSubHgf==1) return 0;
	for (pArcNow=pHnode->pAFirst, pLastArc=(pHnode+1)->pAFirst;
			pArcNow!=pLastArc; pArcNow++) {
		if (pArcNow->inSubHgf) { // if arc in the subhypergf
			weightTmp = pArcNow->pTail->w[idxW] + pArcNow->w[idxW] - pArcNow->w[idxD]*g;
			if (idxHArc<0) {
				if (&H.itsArcs[-idxHArc]==pArcNow) {   // if idxHArc
					wA = weightTmp;
					continue;
				}
			}
			wMax = max(wMax,weightTmp);
		}
	}
	for (pHNow=pHnode->pHFirst, pLastHArc=(pHnode+1)->pHFirst;
			pHNow!=pLastHArc; pHNow++) {
		if (pHNow->inSubHgf) { // if a harc in the subhypergf
			weightTmp=0;
			isMinInf = false;
			// compute weighting function: scan tails
			for (pTailNow=pHNow->pTail,pTailLast=(pHNow+1)->pTail;
					pTailNow!=pTailLast;pTailNow++ ) {
				if ((pTailNow->pTail)->w[idxW]<= -INF) {
					weightTmp= -INF;
					isMinInf = true;
					break;
				}
				weightTmp += ((pTailNow->pTail)->w[idxW])
							 *(pTailNow->m[idxMult]);
			}
			if (isMinInf) continue; // if the (h)arc gives -INF goto next (h)arc
			weightTmp += pHNow->w[idxW]-pHNow->w[idxD]*g;
			if (idxHArc>0) {
				if (&H.itsHArcs[idxHArc]==pHNow) {   // if idxHArc
					wA = weightTmp;
					continue;
				}
			}
			wMax = max(wMax,weightTmp);
		}
	}
	return wA - wMax;
}
