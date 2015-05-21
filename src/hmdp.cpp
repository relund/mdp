#include "hmdp.h"

void HMDP::LoadBin(string stateIdxFile, string stateIdxLblFile, string actionIdxFile,
    string actionIdxLblFile, string actionWFile,  string actionWLblFile,
    string transProbFile, string externalFile)
{
    okay = true;
    externalProc = false;
    HMDPReader reader(stateIdxFile, stateIdxLblFile, actionIdxFile,
        actionIdxLblFile, actionWFile, actionWLblFile, transProbFile, externalFile, this, log);
    if (!reader.okay) okay = false;
    else if (external.size()>0) {
        externalProc = true;
        ExternalAddStageStr();
    }
}

// -----------------------------------------------------------------------------

HMDPReader::HMDPReader(string stateIdxFile, string stateIdxLblFile, string actionIdxFile,
    string actionIdxLblFile, string actionWFile, string actionWLblFile,
    string transProbFile, string externalFile, HMDP *pHMDP, ostringstream & hmdpLog)
{
    pHMDP->ResetLog();
    okay = true;
    this->pHMDP = pHMDP;
    timer.StartTimer();
    AddStates(stateIdxFile, stateIdxLblFile);
    AddActions(actionIdxFile, actionIdxLblFile, actionWFile, actionWLblFile, transProbFile);
    AddExternal(externalFile);
    timer.StopTimer();
    pHMDP->log << "Read binary files (" << timer.ElapsedTime("sec") << " sec.)" << endl;
    timer.StartTimer();
    Compile();
    timer.StopTimer();
    pHMDP->log << "Build the HMDP (" << timer.ElapsedTime("sec") << " sec.)" << endl;
}

// -----------------------------------------------------------------------------


template <class T>
idx HMDPReader::ReadBinary(string fileName, T *&p) {
	ifstream::pos_type fileSize;
	ifstream file;

	// read idx
	file.open(fileName.c_str() ,ios::in|ios::binary|ios::ate);    // open binary file for reading with pointer at end of file to get filesize
	if(!file) {
		if (fileName.find("externalProcesses.bin")==string::npos) pHMDP->log << "Problems opening file " << fileName << "\n" << endl;
		return(0);
	}
	fileSize = file.tellg();
	idx size = fileSize/sizeof(T);
	p = new T[size];
	file.seekg (0, ios::beg);   // set pointer to start of file
	file.read((char *)p,fileSize);
	file.close();
	/*for(idx i=0; i<fileSize/sizeof(T); i++) {
		cout << p[i] << " ";
	}
	cout << endl << endl;*/
	return(size);
}

// -----------------------------------------------------------------------------

void HMDPReader::AddStates(string stateIdxFile, string stateIdxLblFile) {
	int * sIdx;    // raw idx data array
	char * lbl;    // raw labels array
	uInt numb = 0;     // max number of state idx

    // first store all state indices
	idx sIdxSize = ReadBinary<int>(stateIdxFile,sIdx);
    if (sIdxSize==0) {okay = false; return;}

	// now scan sIdx and generate index vectors for each state
	vector<idx> s;  // vector of index
	idx prev = 0;
	TmpState state;
	for(idx i=0; i<sIdxSize; i++) {
		if (sIdx[i]== -1) {
			s.assign(sIdx+prev, sIdx+i);
			numb=MAX(numb,s.size());
			prev=i+1;
			state.iHMDP = s;
			stateVec.push_back(state);
		}
	}
    delete [] sIdx;  // remove tmp array
    pHMDP->levels = numb/3 + 1;   // set number of levels

    // next add labels
	idx lblSize = ReadBinary<char>(stateIdxLblFile,lbl);
	if (lblSize==0) {okay = false; return;}
	// add labels to a string vector
	vector<string> labels;
	char * ptr = lbl;
	for (int i=0;;++i) {
		//cout << ptr << endl;
		labels.push_back(ptr);
		ptr = strrchr(ptr,'\0');
		if ( (ptr==0) | (ptr-lbl>=(int)lblSize) ) break;
		++ptr;
	}
	delete [] lbl;

	// move labels to states
	idx sId;
	for(idx i=0;i<labels.size();++i) {
		if (i % 2 == 0) from_string<idx>(sId, labels[i], std::dec); // if i is even
		else stateVec[sId].label = labels[i];
	} //cout << "size: " << stateVec.size()<<endl;
}

// -----------------------------------------------------------------------------

void HMDPReader::AddActions(string actionIdxFile, string actionIdxLblFile,
	string actionWFile, string actionWLblFile, string transProbFile)
{
	ifstream::pos_type fileSize;
	ifstream file;
	int * aIdx;    // raw idx data
	char * lbl;    // raw labels
	double * aW;
	char * wLbl;
	double * tPr;
	vector<TmpAction> actionVec;  // Vector of all action with actionVec[aId] according to file definitions.
	foundScp3 = false;

	idx aIdxSize = ReadBinary(actionIdxFile,aIdx);
	idx lblSize = ReadBinary(actionIdxLblFile,lbl);
	idx aWSize = ReadBinary(actionWFile,aW);
	idx wLblSize = ReadBinary(actionWLblFile,wLbl);
	idx tPrSize = ReadBinary(transProbFile,tPr);
	// note that all arrays (except the label arrays) have the same number of rows (same number of -1's).
    if ( (aIdxSize==0) | (lblSize==0) | (aWSize==0) | (wLblSize==0) | (tPrSize==0) ) {okay = false; return;}

	// add weight labels to HMDP
	vector<string> labels;
	char * ptr;
	ptr = wLbl;
	for (int i=0;;++i) {
		labels.push_back(ptr);
		ptr = strrchr(ptr,'\0');
		if ( (ptr==0) | (ptr-wLbl>=(int)wLblSize) ) break;
		++ptr;
	}
	labels.pop_back();  // the last element is a dummy
	pHMDP->SetActionWeightNames(labels);
	wLblSize = labels.size();   // number of weights
	delete [] wLbl;

    // scan aIdx
	vector<idx> a;  // vector of index
	idx prev=0;
	TmpAction tmpAction;
	for(idx i=0; i<aIdxSize; i++) {
		if (aIdx[i]== -1) {
			a.assign(aIdx+prev, aIdx+i);    // a now contains the sId and (scp,idx) pairs
			tmpAction.sId = a[0];
			tmpAction.index.clear(); tmpAction.scp.clear();
			for (idx j=1;j<a.size();j++) {  // add scp and idx
				/*if (a[j]<0) {
					cout << "In action scope, index or probability is negative!" << endl;
					exit(1);
				}*/
				if (j%2==0) tmpAction.index.push_back(a[j]);
				if (j%2==1) {
                    tmpAction.scp.push_back(a[j]);
                    if (a[j]==3) foundScp3 = true;
				}
			}
			prev=i+1;
			actionVec.push_back(tmpAction);
		}
	}
    delete [] aIdx;

	// scan aW
	vector<double> b;  // vector of doubles
	idx aId;
	for(aId=0; aId<actionVec.size(); aId++) {
		b.assign(aW + aId*wLblSize, aW + (aId+1)*wLblSize);
		for (idx j=0;j<b.size();j++) {
			actionVec[aId].w.push_back((flt)b[j]);
		}
	}
	delete [] aW;

	// scan tPr
	prev=0;
	aId = 0;
	for(idx i=0; i<tPrSize; i++) {
		if (tPr[i]== -1) {
			b.assign(tPr+prev, tPr+i);
			for (idx j=0;j<b.size();j++) {
				actionVec[aId].pr.push_back((flt)b[j]);
			}
			prev=i+1;
			aId++;
		}
	}
	delete [] tPr;

	// scan lbl
	labels.clear();
	ptr = lbl;
	for (int i=0;;++i) {
		labels.push_back(ptr);
		ptr = strrchr(ptr,'\0');
		if ( (ptr==0) | (ptr-lbl>=(int)lblSize) ) break;
		++ptr;
	}
	labels.pop_back();  // the last element is a dummy
	// add labels to actions
	for(idx i=0;i<labels.size();++i) {
		if (i % 2 == 0) from_string<idx>(aId, labels[i], std::dec); // if i is even
		else actionVec[aId].label = labels[i];
	}
    delete [] lbl; //cout << "aSize: " << actionVec.size()<<endl;
	// copy actions to states
	for(idx i=0;i<actionVec.size();++i) {
        stateVec[actionVec[i].sId].actions.push_back(actionVec[i]);
        actionVec[i].Clear();
	}
}

// -----------------------------------------------------------------------------

void HMDPReader::Compile() {
    Timer cpu;
    if (pHMDP->verbose) {pHMDP->log << "Start building the HMDP ...\n";}
    // create multimap for stages
    cpu.StartTimer();
    string stageStr;
    for (idx i=0; i<stateVec.size(); i++) {
        stageStr = pHMDP->GetStageStr(stateVec[i].iHMDP); //cout << "stageStr: " << stageStr << endl;
        stagesMap.insert(pair< string, int >(stageStr,i));
    }
    cpu.StopTimer();
    if (pHMDP->verbose) {pHMDP->log << "  Create map for stages (" << cpu.ElapsedTime("sec") << " sec.)\n";}
	// set time horizon (have not added dummy stage yet)
	for (uInt s=1;;++s) {
		if (stagesMap.find(ToString(s)) == stagesMap.end()) {  // if stage s not found
			if (s==1) pHMDP->timeHorizon = INFINT;
			else pHMDP->timeHorizon = s;    // since idx start from 0 the set of decision epochs is s
			break;
		}
	} //cout << "tHorizon: " << pHMDP->timeHorizon<<endl;
    // add stage at founder level if infinite time horizon
	vector<idx> s(1,1);  // stage 1
	if (pHMDP->timeHorizon>=INFINT) {   // add second stage at founder level
		uInt ctr = stagesMap.count("0");  // states at founder level
		TmpState tmpState;
		for (idx i=0;i<ctr;++i) {
			s.push_back(i);
			tmpState.iHMDP = s;
			stateVec.push_back(tmpState);
			stagesMap.insert(pair< string, int >("1",stateVec.size()-1));
			s.pop_back();
		}
	}
    // set state ids which are stored in idx of an action
    cpu.StartTimer();
    foundScp3 = false;
    for (idx sId=0; sId<stateVec.size(); ++sId) {
        SetSIds(sId, foundScp3);
    }
    cpu.StopTimer();
    if (pHMDP->verbose) {pHMDP->log << "  Transform actions to internal data structure (" << cpu.ElapsedTime("sec") << " sec.)\n";}
    // find valid ordering of states (no matter value of foundScp3)
    cpu.StartTimer();
    vector<idx> order;
    FindValidOdr(order); //cout << "order: " << vec2String(order) << endl;
    // find a reverse valid ordering of stages
    vector<string> keys;
    set<string> keySet;
    pair<set<string>::iterator, bool> ret;
    string str;
    for (vector<idx>::reverse_iterator rit=order.rbegin(); rit!=order.rend(); ++rit) {
        str = pHMDP->GetStageStr(stateVec[*rit].iHMDP);
        ret = keySet.insert(str);
        if (ret.second==true) {
            keys.push_back(str);
            //cout << " i:" << str << " - " ;
        }
    } //cout << endl;
    reverse(keys.begin(), keys.end());  // the valid ordering of the states
    cpu.StopTimer();
    if (pHMDP->verbose) {pHMDP->log << "  Find valid ordering of stages (" << cpu.ElapsedTime("sec") << " sec.)\n";}
	// build the HMDP data structure based on valid odr of stages (keys vector)
	cpu.StartTimer();
    pair< multimap<string, int >::iterator, multimap<string, int >::iterator > pairS;
    multimap<string, int>::iterator ite;
    HMDP::state_iterator sIte;
	for (idx i=0; i<keys.size(); i++) {
        pairS = stagesMap.equal_range(keys[i]);
        idx sSize;
        idx firstSId = pHMDP->states.size();
        for (ite=pairS.first, sSize = 0; ite!=pairS.second; ++ite, ++sSize) {
            idx sId = ite->second;
            pHMDP->states.push_back(HMDPState(stateVec[sId].label));
            order[sId] = pHMDP->states.size()-1;   // use as map of new id to old id (order[oldId] = newId)
            sIte = pHMDP->state_end()-1;
            for (idx j=0; j<stateVec[sId].actions.size(); ++j) {
                TmpAction & a = stateVec[sId].actions[j];
                sIte->AddAction(a.w, a.index, a.pr, a.label);
            }
        }
        pHMDP->stages[keys[i]] = pair<idx,idx>(firstSId, sSize);    // store first state id of stage
	}
	cpu.StopTimer();
	if (pHMDP->verbose) {pHMDP->log << "  Build the internal HMDP data structure (" << cpu.ElapsedTime("sec") << " sec.)\n";}
	// Sort all trans pr increasing in id
	cpu.StartTimer();
	for (HMDP::state_iterator iteS = pHMDP->state_begin(); iteS!=pHMDP->state_end(); ++iteS) {
        for (HMDP::action_iterator iteA = pHMDP->action_begin(iteS); iteA!=pHMDP->action_end(iteS); ++iteA) {
            // Set correct id in HMDPTrans (id's in stateVec stored now)
            for (HMDP::trans_iterator iteT=pHMDP->trans_begin(iteA); iteT!=pHMDP->trans_end(iteA); ++iteT) {
                iteT->id = order[iteT->id];
            }
            iteA->Sort();
        }
	}
	cpu.StopTimer();
	if (pHMDP->verbose) {pHMDP->log << "  Sort transitions increasing in state id (" << cpu.ElapsedTime("sec") << " sec.)\n";}
}

// -----------------------------------------------------------------------------

void HMDPReader::SetSIds(const idx & iState, bool & findValidOdr) {
	bool up, next;    // where do the actions go
	idx iS = 0;
	int level = pHMDP->GetLevel(stateVec[iState].iHMDP);
	string stageNext = pHMDP->GetNextStageStr(stateVec[iState].iHMDP);
	string stageNextFather = pHMDP->GetNextFatherStageStr(stateVec[iState].iHMDP);
	string stageNextChild;
	pair< multimap<string, int >::iterator, multimap<string, int >::iterator > pairUp;
	pair< multimap<string, int >::iterator, multimap<string, int >::iterator > pairNext;
	multimap<string, int >::iterator ite;

    // check what scopes we have
    up=next=false;
    for (idx a=0; a<stateVec[iState].actions.size(); a++) {
        for (idx j=0; j<stateVec[iState].actions[a].pr.size(); j++) {
            if (stateVec[iState].actions[a].scp[j]==0) up=true;
            if (stateVec[iState].actions[a].scp[j]==1) next=true;
        }
    }
    if (up)	pairUp = stagesMap.equal_range(stageNextFather);
    if (next) pairNext = stagesMap.equal_range(stageNext);
	//cout << endl << sUp << " - " <<sNext << endl;
	/*for (ite = pairUp.first; ite != pairUp.second; ++ite)
	{
	   cout << "  [" << (*ite).first << ", " << (*ite).second << "]" << endl;
	} */
    //log << "level: " << level << " levels:" << levels << " - ";
	for (idx a=0; a<stateVec[iState].actions.size(); a++) {
        vector<idx> & index = stateVec[iState].actions[a].index;
        vector<idx> & scp = stateVec[iState].actions[a].scp;
        vector<idx> sIds;
		for (idx j=0; j<index.size(); j++) {
			if (scp[j]==1) { // next stage
				if (level==pHMDP->levels-1) {    // ASSUME states at a stage are defined in sequence. TODO: This may be dangerous does it always hold!!
                    iS = pairNext.first->second + index[j];
				}
				else {
                    ite = pairNext.first;
                    for (idx i=0; i<index[j]; i++) ++ite;      // TODO This is very slow for stages with many states!! e.g. a ordinary big MDP. Current hack define your MDP using scp 3
                    iS = ite->second;
				}
			}
			if (scp[j]==0) { // next father stage
				ite = pairUp.first;
				for (idx i=0; i<index[j]; i++) ++ite;
				iS = ite->second;
			}
			if (scp[j]==2) { // next child stage
                stageNextChild = pHMDP->GetNextChildStageStr(stateVec[iState].iHMDP, a);
                pair< multimap<string, int >::iterator, multimap<string, int >::iterator > pairDown = stagesMap.equal_range(stageNextChild);
                if (level+1==pHMDP->levels-1) { // check if child stage at lowest level -> states at a stage are defined in sequence.
                    iS = pairDown.first->second + index[j];
                } else {
                    ite = pairDown.first;
                    for (idx i=0; i<index[j]; i++) ++ite;      // TODO This is very slow for stages with many states!! e.g. a ordinary big MDP. Current hack define your MDP using scope 3
                    iS = ite->second;
				}
			}
			if (scp[j]==3) { // specify state index/id
				iS = index[j];
				findValidOdr = true;    // possible that have to create new valid ordering.
			}
			sIds.push_back(iS);
		}
        stateVec[iState].actions[a].index = sIds;     // replace idx with state ids
        //cout << "(iS,iA) = (" << iState << "," << a << ") tails: " << vec2String(stateVec[iState].actions[a].index) << endl;
        stateVec[iState].actions[a].scp.clear();
	}
}

// -----------------------------------------------------------------------------

void HMDPReader::AddExternal(string externalFile) {
    char * lbl;    // raw str
    string stageStr, prefix, tmp;
    idx lblSize = ReadBinary<char>(externalFile,lbl);
    if (lblSize==0) return;   // no external processes

    char * ptr = lbl;
    for (int i=0;;++i) {
        stageStr = ptr;
        ptr = strrchr(ptr,'\0');
        if ( (ptr==0) | (ptr-lbl>=(int)lblSize) ) break;
        ++ptr;
        prefix = ptr;
        pHMDP->external[stageStr] = prefix;
        ptr = strrchr(ptr,'\0');
        if ( (ptr==0) | (ptr-lbl>=(int)lblSize) ) break;
        ++ptr;
    }
    delete [] lbl;
    // showing contents:
    /*std::map<string,string>::iterator it;
    std::cout << "mymap contains:\n";
    for (it=pHMDP->external.begin(); it!=pHMDP->external.end(); ++it)
        std::cout << it->first << " => " << it->second << '\n';*/
}

// -----------------------------------------------------------------------------

void HMDPReader::FindValidOdr(vector<idx> & order) {
    // find forward star actions for each node
    for (idx iS=0; iS<stateVec.size(); ++iS) {
        for (idx iA=0; iA<stateVec[iS].actions.size(); ++iA) {
            stateVec[iS].actions[iA].scp.resize(1); stateVec[iS].actions[iA].scp[0]=0;  // use scp[0] as visits counter
            vector<idx> & bStar = stateVec[iS].actions[iA].index;
            for (idx i=0; i<bStar.size(); ++i) { //cout << iS << "<-" << bStar[i] << endl;
                stateVec[ bStar[i] ].fStar.push_back( pair<idx,idx>(iS,iA) );
            }
        }
    }
//    // make items in fStar unique
//    vector<idx>::iterator it;
//    for (idx iS=0; iS<stateVec.size(); ++iS) {
//        sort(stateVec[iS].fStar.begin(), stateVec[iS].fStar.end());
//        it = unique(stateVec[iS].fStar.begin(), stateVec[iS].fStar.end());
//        stateVec[iS].fStar.resize( distance(stateVec[iS].fStar.begin(),it) );
//    }
    // find last stage
    pair< multimap<string, int>::iterator, multimap<string, int>::iterator > itP;
    if (pHMDP->timeHorizon>=INFINT) itP = stagesMap.equal_range("1");
    else itP = stagesMap.equal_range(pHMDP->GetLastStageStr());
    priority_queue<idx> cand;
    for (multimap<string,int>::iterator it = itP.first; it!=itP.second; ++it) {
        cand.push(it->second);
    }
    idx sId;
    order.clear();
    vector<idx> visit(stateVec.size(),0);
	while (!cand.empty())
	{
	    sId = cand.top(); //cout << "select id=" << sId << endl;
	    cand.pop();
		order.push_back(sId);
		for (idx i=0; i<stateVec[sId].fStar.size(); i++) {
            pair<idx,idx> pF = stateVec[sId].fStar[i];
            stateVec[pF.first].actions[pF.second].scp[0]++;
            if (stateVec[pF.first].actions[pF.second].scp[0] == stateVec[pF.first].actions[pF.second].index.size()) {  // if visited all tails
                visit[pF.first]++; //cout << "  increment fstar id=" << pF.first << " to " << visit[pF.first] << endl;
            }
            if (visit[pF.first]==stateVec[pF.first].actions.size()) {
                cand.push(pF.first); //cout << " add id=" << pF.first << endl;
            }
		}
	}
}

// -----------------------------------------------------------------------------

string HMDP::Print() {
    ostringstream out;
	out << "HMDP with " << levels << " level(s), time-horizon: ";
	if (timeHorizon>=INFINT) out << "infinite";
	else out << timeHorizon << " (finite)";
	out << endl << "Weights: " << vec2String(weightNames) << endl;
	for (stage_iterator ite = stage_begin(); ite!=stage_end(); ++ite) {
        idx iS = ite->second.first;
        idx sizeS = ite->second.second;
        out << "Stage " << ite->first << " (id,size)=(" << iS << "," << sizeS << "):" << endl;
        for (idx i=0;i<sizeS;++iS,++i) {
            out << "  " << iS << ": " << states[iS].Print();
        }
	}
	return out.str();
}

// ----------------------------------------------------------------------------

    void HMDP::ExternalAddStageStr() {
        //cout << "Add labels!!\n";
        if (!externalProc) return;
        vector<idx> id;
        map<string,string>::iterator it;
        for (it=external.begin(); it!=external.end(); ++it) {
            pair<idx,idx> sP;
            sP = stages[it->first];
            idx idS = sP.first;
            idx sSize = sP.second;
            for (idx j=0; j<sSize; ++j, ++idS) {
                //cout << "Node:" << id[j] << " lbl:" << it->first << endl;
                states[ idS ].label = it->first;
            }
        }
    }

// ----------------------------------------------------------------------------

void HMDP::ExternalResetActions(const idx & idxW, const idx & idxD) {
    if (!externalProc) return;
    vector<idx> id;
    map<string,string>::iterator it;
    for (it=external.begin(); it!=external.end(); ++it) {
        id = GetIds(it->first);
        for (idx j=0; j<id.size(); ++j) {
            SetActionW( (flt)0, id[j], 0, idxW);
            SetActionW( (flt)0, id[j], 0, idxD);
            SetActionPrZero(id[j], 0);
        }
    }
}

// ----------------------------------------------------------------------------

void HMDP::ExternalResetStates() {
    if (!externalProc) return;
    for (map<string,string>::iterator it=external.begin(); it!=external.end(); ++it) {
        SetStateWStage(it->first, -INF);
    }
}

// ----------------------------------------------------------------------------

bool HMDP::ExternalStatesUpdate(Crit crit, state_iterator iteS, string & curPrefix, HMDPPtr & pExt,
     const idx & idxW, const idx & idxD, const flt & g, const flt & rate, const flt & rateBase)
{
    //cout << "ExtStatesU: idxD=" << idxD << endl;
    string stageStr = iteS->label;     // external stage in HMDP corresponding to first stage in external
    string prefix = external[stageStr];    // prefix of external process //cout << "label: " << stageStr << " prefix: " << prefix << endl;
    ExternalAllocteMem(pExt, prefix, curPrefix);
    if (!okay) return false;
    string stageNextStr = GetNextStageStr(stageStr);  // external stage in HMDP corresponding to last stage in external
    vector<flt> rewards = GetStageW(stageNextStr);   // get the rewards from external nodes corresponding to last stage //cout << "next stage: " << stageNextStr << endl; //cout << "Start valueIte\n";
    pExt->ValueIte(crit, 1, 0, idxW, idxD, rewards, g, rate, rateBase);
    string stageZeroExtStr = "0"; // first stage in external //cout << "Copy from external:" << endl;
    ExternalCopyWState(stageStr, stageZeroExtStr, pExt, false);   // copy rewards to the HMDP //cout << "Update actions:" << endl;
    bool newPred = ExternalSetActions(stageStr, pExt, idxW, idxD);
    return newPred;
}

// ----------------------------------------------------------------------------

void HMDP::ExternalCopyWState(string stage, string stageExt, const HMDPPtr & pExt, const bool toExt)
{
	multimap<string, int >::iterator iteTo, iteFrom;
	pair<idx,idx> pS = stages[stage];
	pair<idx,idx> pSExt = pExt->stages[stageExt];

	if (toExt) {  // transfer to external process
        for (idx i=0, iSExt=pSExt.first, iS = pS.first; i<pSExt.second; ++i, ++iSExt, ++iS) {
            pExt->states[iSExt].w = states[iS].w;
        }
	}
	else {
        for (idx i=0, iSExt=pSExt.first, iS = pS.first; i<pSExt.second; ++i, ++iSExt, ++iS) {
            states[iS].w = pExt->states[iSExt].w;
        }
	}
}

// ----------------------------------------------------------------------------

void HMDP::ExternalAllocteMem(HMDPPtr & pExt, const string & prefix, string & curPrefix) {
    if (prefix != curPrefix) {   // then have to load a new external process
        if (verbose && pExt!=NULL) log << "  Free memory of external process with prefix '" << curPrefix << "'." << endl;
        delete pExt;   // delete the previous process if exists
        if (verbose) log << "  Allocate memory for external process with prefix '" << prefix << "'." << endl;
        pExt = new HMDP(prefix);
        //cout << "Print external: " << endl << pExt->Print() << endl;
        if (!pExt->okay) {
            log << "  Error: Cannot read external process with prefix '" << prefix << "'!" << endl;
            delete pExt; okay = false; pExt=NULL;
            return;
        }
        curPrefix = prefix;
    }
    else if (verbose) log << "  Use current external process with prefix '" << prefix << "' again." << endl;
}

// ----------------------------------------------------------------------------

bool HMDP::ExternalSetActions(string stageStr, const HMDPPtr & pExt, const idx & idxW, const idx & idxD)
{
    //cout << "ExtSetA: idxD=" << idxD << endl;
    string stageLastExtStr = ToString(pExt->GetLastStageStr());
    string stageZeroExtStr = "0";
    string stageNextStr = GetNextStageStr(stageStr);
	bool newPolicy = false;
	// rewards
	pExt->SetStateWStage(stageLastExtStr,0);  // reset weights
	pExt->CalcPolicy(Reward,idxW);
    for (state_iterator iteTo = state_begin(stageStr), iteFrom=pExt->state_begin(stageZeroExtStr);
         iteTo!=state_end(stageStr); ++iteTo, ++iteFrom)
    {
        if (w(iteTo,0,idxW)!=w(iteFrom)) newPolicy = true;      // a new policy of the external process have been found
        w(iteTo,0,idxW) = w(iteFrom); //cout << "Set rew of action in " << GetId(iteTo) << " to " << w(iteFrom) << endl;
    }
    // durations
    pExt->SetStateWStage(stageLastExtStr,0);  // reset weights
    pExt->CalcPolicy(Reward,idxD);  // calc durations of external actions
    for (state_iterator iteTo = state_begin(stageStr), iteFrom=pExt->state_begin(stageZeroExtStr);
         iteTo!=state_end(stageStr); ++iteTo, ++iteFrom)
    {
        if (w(iteTo,0,idxD)!=w(iteFrom)) newPolicy = true;      // a new policy of the external process have been found
        w(iteTo,0,idxD) = w(iteFrom); //cout << "Set dur of action in " << GetId(iteTo) << " to " << w(iteFrom) << endl;
    }
    // trans pr
    pExt->SetStateWStage(stageLastExtStr,0);  // reset weights
    for (state_iterator iteN = state_begin(stageNextStr), iteL=pExt->state_begin(stageLastExtStr);
         iteL!=pExt->state_end(stageLastExtStr); ++iteL, ++iteN)
    {
        idx id = GetId(iteN);
		w(iteL) = 1;
		if (iteL!=state_begin(stageLastExtStr)) w(iteL-1) = 0;   // restore previous
        pExt->CalcPolicy(TransPr);
        for (state_iterator iteTo = state_begin(stageStr), iteFrom=pExt->state_begin(stageZeroExtStr);
             iteTo!=state_end(stageStr); ++iteTo, ++iteFrom)
        {
            action_iterator iteA = action_begin(iteTo); //cout << "Set pr of action in " << GetId(iteTo) << " trans to " << id << " to " << w(iteFrom) << endl;
            flt oldVal = iteA->SetTransPr(id,w(iteFrom));
            if (w(iteFrom) != oldVal) newPolicy = true;
        }
	}
	return newPolicy;
}

//-----------------------------------------------------------------------------

flt HMDP::PolicyIte(Crit crit, uSInt maxIte, const idx idxW, const idx idxD, const flt rate, const flt rateBase) {
	//cout << "PolicyIte: idxD=" << idxD << endl;
	ResetLog();
	if (timeHorizon<INFINT) {
		log << "Policy iteration can only be done on infinite time-horizon HMDPs!" << endl;
		return -INF;
	}
    log << "Run policy iteration ";
	switch (crit) {
        case AverageReward: log << "under average reward criterion using \nreward '" <<
            GetWName(idxW) << "' over '" << GetWName(idxD) << "'. Iterations (g):" << endl;
            break;
        case DiscountedReward: log << "using quantity '" << GetWName(idxW)
            << "' under discounting criterion \nwith '" << GetWName(idxD)
            << "' as duration using interest rate " << rate
            << " and a rate basis equal " << rateBase << ". \nIteration(s):";
            break;
        default: log << "Criterion not defined for policy iteration!" << endl; return -INF;
	}
	MatAlg matAlg; // Matrix routines
	ExternalResetActions(idxW, idxD);
	timer.StartTimer();
	SetStateWStage("1", (flt)0);
	int rows = GetStateSize("0");
	MatSimple<double> r(rows,1),   // Matrix of founder rewards
				   w(rows,1),      // Matrix of weights (the unknown)
				   d(rows,1),      // Matrix of denominator values
				   P(rows,rows);   // Matrix of prob values
	MatSimple<double> I(rows,true); // identity
	flt g = 0;
	okay = true;
	bool newPred;
	SetPred(0); // default policy
	if (externalProc) CalcOptPolicy(crit, idxW, g, idxD, rate, rateBase);   // if external processes we have to find the optimal policy of the external processes and set external action w and trans pr
	for (idx k=1; ; ++k) { //cout << endl << "IteP:" << k << endl;
		if (verbose) log << endl; log << k << " "; if (verbose) log << endl;
		// find rewards, dur, trans pr at founder given policy
		if (crit==AverageReward) {
            FounderW(Reward, r, idxW);
            FounderPr(TransPr,P);
            FounderW(Reward, d, idxD);
        }
        else {
            FounderW(crit, r, idxW,g,idxD,rate,rateBase); //cout << "r mat: " << r << endl;
            FounderPr(TransPrDiscounted,P,idxD,rate,rateBase); //cout << "P mat: " << P << endl;
        }
		// If AverageReward solve equations h = r - dg + Ph where r, d and P have been calculated for the founder. This is equivalent to solving (I-P)h + dg = r -> (I-P,d)(h,g)' = r which is equivalent to solving Qw = r (equation (8.6.8) in Puterman) where last col in (I-P) replaced with d.
		// If DiscountedReward solve equations w = r + Pw -> (I-P)w = r
		matAlg.IMinusP(P);  // Set P := I-P
		if (crit==AverageReward) for(idx j=0; j<(idx)rows; ++j) P(j,rows-1) = d(j,0);   // set implicit h_{rows-1}=0 and calc g here.
		if (matAlg.LASolve(P,w,r)) {g = -INF; log << " Error: can not solve system equations. Is the model fulfilling the model assumptions (e.g. unichain)? "; break;}
		if (crit==AverageReward) {
            g = w(rows-1,0);
            log << "(" << g << ") "; if (verbose) log << endl; //cout << "g=" << g << endl;
		} //cout << "w mat: " << w << endl;
		state_iterator iteL; idx j;
		for (iteL=state_begin("1"), j=0; iteL!=state_end("1"); ++iteL, ++j) {
            if (j<(idx)rows-1 ) HMDP::w(iteL) = w(j,0);
            else if (crit==DiscountedReward) HMDP::w(iteL) = w(j,0);
		}
		// update policy
		newPred = CalcOptPolicy(crit, idxW, g, idxD, rate, rateBase);
		if (!okay) {g=-INF; break;}   // something went wrong (see the log)
		if (!newPred) {
			log << k+1 << " (" << g << ") "; if (verbose) log << endl;
			break;    // optimal strategy found
		}
		if (k>=maxIte) { log << "\nReached upper limit of iterations! Seems to loop. \nIs the model fulfilling the model assumptions (e.g. unichain)?\n"; break;}
	}
	log << "finished. Cpu time: " << timer.ElapsedTime("sec") << " sec." << endl;
	if (crit==AverageReward) return g; //cout << "Rewards: " << vec2String(GetStageW("0")) << endl;
	return -INF;
}

// ----------------------------------------------------------------------------

void HMDP::ValueIte(Crit crit, idx maxIte, flt epsilon, const idx idxW,
     const idx idxDur, vector<flt> & termValues,
     const flt g, const flt rate, const flt rateBase)
{
	ResetLog();
	log << "Run value iteration with epsilon = " << epsilon  << " at most "
		<< maxIte << " time(s)" << endl << "using quantity '" << GetWName(idxW) << "'";
	switch (crit) {
        case AverageReward: log << " under average reward criterion given an average reward g = " << g << ".\n";
            maxIte = 1;     // not implemented more than one time yet
            break;
        case Reward: log << " under reward criterion." << endl; break;
        case DiscountedReward: log << " under expected discounted reward criterion \nwith '" <<
            GetWName(idxDur) << "' as duration using interest rate " << rate <<
            " and rate basis " << rateBase << ".\nIterations:"; break;
        default: log << "Criterion not defined for value iteration!" << endl; return;
	}
	timer.StartTimer();
	SetPred(-1);
	string stageZeroStr = "0";
	string stageLastStr = GetLastStageStr();
    if (termValues.size()!=GetStateSize( GetLastStageStr() )) {
        log << "Error initial values vector does not have the same size " << termValues.size()
        << " as the states that must be assigned the values (" << GetStateSize(stageLastStr) << ")!\n";
        return;
    }
    vector<flt>::iterator iteV; state_iterator iteS;
    for (iteS = state_begin(stageLastStr), iteV=termValues.begin(); iteS!=state_end(stageLastStr); ++iteS, ++iteV) {
		w(iteS) = *iteV;
	}
	idx i;
	for (i=0; i<maxIte; ++i) { //cout << "Ite: " << i+1 << endl;
        CalcOptPolicy(crit,idxW,g,idxDur,rate,rateBase);
		if (crit==DiscountedReward)
            if(MaxDiffFounder()<epsilon) break;
		if (i<maxIte-1) {    // set next last stage values to stage zero values
            for (state_iterator iteZ = state_begin(stageZeroStr), iteL=state_begin(stageLastStr);
                iteZ!=state_end(stageZeroStr); ++iteZ, ++iteL)
                    w(iteL) = w(iteZ);
		}
	}
	if (crit==DiscountedReward && timeHorizon>=INFINT) log << " " << i+1;
	timer.StopTimer();
	log << " Finished. Cpu time " << timer.ElapsedTime("sec") << " sec." << endl;
}

// ----------------------------------------------------------------------------

bool HMDP::CalcOptPolicy(Crit crit, idx idxW, flt g, idx idxDur, flt rate, flt rateBase) {
	//cout << "CalcOptP: idxD=" << idxDur << endl;
	flt wTmp;      // weight to compare
	bool newPred = false;       // true if the stored pred change in a node
	bool isMinInf;      // true if a hyperarc gives -INF in the head node
	int oldPred;
	string externalPrefix; // prefix of the external process in memory
	flt dB = exp(-rate/rateBase);      // the discount base     // cout << "dB=" << dB << endl;
	HMDP * pExtProc = NULL;    // pointer to external process
    ExternalResetStates();  // set state weight to -INF
    // scan states according to the valid ordering
    for(state_iterator iteS = state_begin(); iteS!=state_end(); ++iteS) {
       if ( ExternalState(iteS) ) { //cout << "State " << GetId(iteS) << " is external\n";
            if (iteS->w== -INF) newPred = ExternalStatesUpdate(crit, iteS, externalPrefix, pExtProc, idxW, idxDur, g, rate, rateBase);
            if (!okay) return false;
            pred(iteS) = 0;
        }
        else { //cout << "State " << GetId(iteS) << " is normal - ";
            if (GetActionSize(iteS)>0) w(iteS)= -INF;  // reset weight
            oldPred = pred(iteS);  //cout << "  oldPred=" << oldPred << endl;
            for (action_iterator iteA = action_begin(iteS); iteA!=action_end(iteS); ++iteA) { //cout << "    iA: " << GetIdx(iteS,iteA) << " w=" << vec2String(iteA->w) << " ";
                wTmp=0; isMinInf = false;
                for (trans_iterator iteT = trans_begin(iteA); iteT!=trans_end(iteA); ++iteT) { //cout << "      t: w(" << iteT->id << ")=" << w(GetIte(iteT->id)) << endl;
                    if ( w(GetIte(iteT->id) ) <= -INF) {
                        wTmp= -INF;
                        isMinInf = true;
                        break;
                    }
                    wTmp += w( GetIte(iteT->id) ) * pr(iteT);
                } //cout << "wTmp tails=" << wTmp << endl;
                if (isMinInf) continue; // if the (h)arc gives -INF go to next (h)arc
                switch(crit){
                    case AverageReward: wTmp += w(iteA,idxW)-w(iteA,idxDur)*g; break;
                    case Reward: wTmp += w(iteA,idxW); break;
                    case DiscountedReward: wTmp = wTmp*pow(dB,w(iteA,idxDur)) + w(iteA,idxW); break;
                    case TransPr: wTmp = wTmp; break;
                    case TransPrDiscounted: wTmp = wTmp*pow(dB,w(iteA,idxDur)); break;
                    default: log << "Criterion not defined!" << endl; break;
                }  //cout << "wTmp=" << wTmp << endl;
                if (w(iteS)<wTmp) {
                    w(iteS) = wTmp;
                    pred(iteS) = GetIdx(iteS,iteA); //cout << "Set State: " << GetId(iteS) << " to  w=" << w(iteS) << " pred=" << pred(iteS) << endl;
                }
            }
            if (pred(iteS) != oldPred) {
                newPred = true;
            }
        } //cout << "State: " << GetId(iteS) << " w=" << w(iteS) << " pred=" << pred(iteS) << endl;
    }
	if (verbose && pExtProc!=NULL) log << "  Free memory of external process with prefix '" << externalPrefix << "'." << endl;
	delete pExtProc;
	return newPred;
}

// ----------------------------------------------------------------------------

void HMDP::CalcPolicy(Crit crit, idx idxW, flt g, idx idxDur, flt rate, flt rateBase) {
	//cout << "CalcP: idxW=" << idxW << " idxD=" << idxDur << endl;
	flt wTmp;      // weight to compare
	flt dB = exp(-rate/rateBase);      // the discount base   //  cout<< "r:" << rate << " b:" << rateBase << endl;
    // scan states according to the valid ordering
    for(state_iterator iteS = state_begin(); iteS!=state_end(); ++iteS) {
        //cout << "State " << GetId(iteS) << " is normal with wPred=" << w(iteS) << endl;
        if (GetActionSize(iteS)>0) {
            w(iteS)= -INF;  // reset weight
            action_iterator iteA = GetIte(iteS, pred(iteS));
            wTmp=0;
            for (trans_iterator iteT = trans_begin(iteA); iteT!=trans_end(iteA); ++iteT) {
                wTmp += w( GetIte(iteT->id) ) * pr(iteT);
            }
            switch(crit){
                case AverageReward: w(iteS) = wTmp + w(iteA,idxW)-w(iteA,idxDur)*g; break;
                case Reward: w(iteS) = wTmp + w(iteA,idxW); break;
                case DiscountedReward: w(iteS) = wTmp*pow(dB,w(iteA,idxDur)) + w(iteA,idxW); break;
                case TransPr: w(iteS) = wTmp; break;
                case TransPrDiscounted: w(iteS) = wTmp*pow(dB,w(iteA,idxDur)); break;
                default: log << "Criterion not defined!" << endl; break;
            }
        } //cout << "Policy::State: " << GetId(iteS) << " w=" << w(iteS) << " pred=" << pred(iteS) << endl;
    }
}

// ----------------------------------------------------------------------------

uSInt HMDP::Check(flt eps) {
    idx msg = 0;
    ResetLog();
    timer.StartTimer();
    okay = true;
    log << "Checking MDP";
    for(state_iterator iteS = state_begin(); iteS!=state_end(); ++iteS) {
       if ( ExternalState(iteS) ) {
            for (action_iterator iteA = action_begin(iteS); iteA!=action_end(iteS); ++iteA) {
                for (trans_iterator iteT = trans_begin(iteA); iteT!=trans_end(iteA); ++iteT) {
                    if (iteT->id>=states.size()) {
                        log << "Error: External state " << iteS->label << " (id = " << GetId(iteS) << "). Action with index"
                            << GetIdx(iteS, iteA) << " has a transition to non-existing state with id " << iteT->id << "!" << endl;
                        okay = false;
                        msg = 2;
                        break;
                    }
                }
            }
        }
        else {
            for (action_iterator iteA = action_begin(iteS); iteA!=action_end(iteS); ++iteA) {
                flt sum = 0;
                for (trans_iterator iteT = trans_begin(iteA); iteT!=trans_end(iteA); ++iteT) sum += iteT->pr;
                if (abs(sum-1)>eps) {
                    log << "Warning: In action " << iteA->label << " probabilities do not sum to one! ";
                    log << "Sum equals " << sum << endl;
                    log << "(state with id " << GetId(iteS) << " (" << iteS->label << ") action with index " << GetIdx(iteS, iteA) << ")" << endl;
                    msg = 1;
                }
                for (trans_iterator iteT = trans_begin(iteA); iteT!=trans_end(iteA); ++iteT) {
                    if (iteT->id>=states.size()) {
                        log << "Error: State " << iteS->label << " (id = " << GetId(iteS) << "). Action " << iteA->label
                            << " (" << GetIdx(iteS, iteA) << ") has a transition to non-existing state with id " << iteT->id << "!" << endl;
                        okay = false;
                        msg = 2;
                        break;
                    }
                }
            }
        }
    }
    timer.StopTimer();
    if (msg==0) log << " and found no errors ";
    log << "(" << timer.ElapsedTime("sec") << " sec.)" << endl;
    return msg;
}
