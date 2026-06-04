#include "hmdp.h"

static vector<string> ParseBinaryStrings(const char * data, idx size) {
    vector<string> labels;
    string label;
    for (idx i=0; i<size; ++i) {
        if (data[i]=='\0') {
            if (!label.empty()) {
                labels.push_back(label);
                label.clear();
            }
        } else {
            label.push_back(data[i]);
        }
    }
    if (!label.empty()) labels.push_back(label);
    return labels;
}

void HMDP::LoadBin(string stateIdxFile, string stateIdxLblFile, string actionIdxFile,
    string actionIdxLblFile, string actionWFile,  string actionWLblFile,
    string transProbFile, string externalFile, string transWFile, string transWLblFile)
{
    okay = true;
    externalProc = false;
    HMDPReader reader(stateIdxFile, stateIdxLblFile, actionIdxFile,
        actionIdxLblFile, actionWFile, actionWLblFile, transProbFile, externalFile,
        transWFile, transWLblFile, this, log);
    if (!reader.okay) okay = false;
    else if (external.size()>0) {
        externalProc = true;
        ExternalAddStageStr();
    }
}

// -----------------------------------------------------------------------------

HMDPBuilder::HMDPBuilder(bool verbose_)
{
    pHMDP = new HMDP(verbose_);
    reader.pHMDP = pHMDP;
    reader.okay = true;
    reader.foundScp3 = false;
    closed = false;
    released = false;
}

// -----------------------------------------------------------------------------

HMDPBuilder::~HMDPBuilder()
{
    if (!released && pHMDP!=NULL) {
        delete pHMDP;
        pHMDP = NULL;
    }
}

// -----------------------------------------------------------------------------

void HMDPBuilder::SetWeights(vector<string> labels)
{
    if (closed) throw runtime_error("memoryMDPWriter is closed.");
    pHMDP->SetActionWeightNames(labels);
}

// -----------------------------------------------------------------------------

void HMDPBuilder::SetTransWeights(vector<string> labels)
{
    if (closed) throw runtime_error("memoryMDPWriter is closed.");
    pHMDP->SetTransWeightNames(labels);
}

// -----------------------------------------------------------------------------

idx HMDPBuilder::AddState(vector<idx> index, string label)
{
    if (closed) throw runtime_error("memoryMDPWriter is closed.");
    if (index.empty()) throw runtime_error("State index must not be empty.");
    HMDPReader::TmpState state;
    state.iHMDP = index;
    state.label = label;
    reader.stateVec.push_back(state);
    pHMDP->levels = MAX(pHMDP->levels, (int)(index.size()/3 + 1));
    return reader.stateVec.size()-1;
}

// -----------------------------------------------------------------------------

void HMDPBuilder::AddAction(idx stateRowId, vector<idx> scope, vector<idx> id,
    vector<flt> pr, vector<flt> weights, vector<flt> transWeights, string label)
{
    if (closed) throw runtime_error("memoryMDPWriter is closed.");
    if (stateRowId>=reader.stateVec.size()) throw runtime_error("Action state row id does not exist.");
    if (scope.size()!=id.size()) throw runtime_error("Action scope and id vectors must have the same length.");
    if (scope.size()!=pr.size()) throw runtime_error("Action transition probability length must match scope and id.");
    if (weights.size()!=pHMDP->weightActionNames.size()) {
        throw runtime_error("Action weight length must match the number of action weight labels.");
    }
    idx transWeightCount = pHMDP->weightTransNames.size();
    if (transWeightCount>0 && transWeights.size()!=pr.size()*transWeightCount) {
        throw runtime_error("Transition weight length must equal transitions times transition weight labels.");
    }
    if (transWeightCount==0 && transWeights.size()>0) {
        throw runtime_error("Transition weights were supplied but no transition weight labels were set.");
    }

    HMDPReader::TmpAction action;
    action.sId = stateRowId;
    action.scp = scope;
    action.index = id;
    action.pr = pr;
    action.w = weights;
    action.label = label;
    action.transW.resize(pr.size());
    if (transWeightCount>0) {
        for (idx i=0; i<pr.size(); ++i) {
            for (idx j=0; j<transWeightCount; ++j) {
                action.transW[i].push_back(transWeights[i*transWeightCount+j]);
            }
        }
    }
    reader.stateVec[stateRowId].actions.push_back(action);
}

// -----------------------------------------------------------------------------

HMDP* HMDPBuilder::Close()
{
    if (closed) throw runtime_error("memoryMDPWriter is already closed.");
    closed = true;
    pHMDP->ResetLog();
    pHMDP->okay = true;
    pHMDP->externalProc = false;
    reader.okay = true;
    if (reader.stateVec.empty()) {
        pHMDP->okay = false;
        pHMDP->log << "No states have been added to the HMDP." << endl;
    } else {
        reader.timer.StartTimer();
        reader.Compile();
        reader.timer.StopTimer();
        pHMDP->log << "Build the HMDP from memory (" << reader.timer.ElapsedTime("sec") << " sec.)" << endl;
        if (!reader.okay) pHMDP->okay = false;
    }
    vector<HMDPReader::TmpState>().swap(reader.stateVec);
    reader.stagesMap.clear();
    released = true;
    return pHMDP;
}

// -----------------------------------------------------------------------------

string HMDPBuilder::GetLog()
{
    if (pHMDP==NULL) return string();
    return pHMDP->GetLog();
}

// -----------------------------------------------------------------------------

HMDPReader::HMDPReader(string stateIdxFile, string stateIdxLblFile, string actionIdxFile,
    string actionIdxLblFile, string actionWFile, string actionWLblFile,
    string transProbFile, string externalFile, string transWFile, string transWLblFile,
    HMDP *pHMDP, ostringstream & hmdpLog)
{
    pHMDP->ResetLog();
    okay = true;
    this->pHMDP = pHMDP;
    timer.StartTimer();
    AddStates(stateIdxFile, stateIdxLblFile);
    AddActions(actionIdxFile, actionIdxLblFile, actionWFile, actionWLblFile, transProbFile,
        transWFile, transWLblFile);
    AddExternal(externalFile);
    timer.StopTimer();
    pHMDP->log << "Read binary files (" << timer.ElapsedTime("sec") << " sec.)" << endl;
    timer.StartTimer();
    Compile();
    timer.StopTimer();
    pHMDP->log << "Build the HMDP (" << timer.ElapsedTime("sec") << " sec.)" << endl;
}

// -----------------------------------------------------------------------------

HMDPReader::HMDPReader()
{
    okay = true;
    foundScp3 = false;
    pHMDP = NULL;
}

// -----------------------------------------------------------------------------


template <class T>
idx HMDPReader::ReadBinary(string fileName, T *&p) {
	ifstream::pos_type fileSize;
	ifstream file;
	p = NULL;

	// read idx
	file.open(fileName.c_str() ,ios::in|ios::binary|ios::ate);    // open binary file for reading with pointer at end of file to get filesize
	if(!file) {
		if (fileName.find("externalProcesses.bin")==string::npos) pHMDP->log << "Problems opening file " << fileName << "\n" << endl;
		return(0);
	}
	fileSize = file.tellg();
	idx size = fileSize/sizeof(T);
	if (size==0) {
		file.close();
		return(0);
	}
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
	//if (lblSize==0) {okay = false; return;}
	// fix bug show no labels okay
	if (lblSize==0) {return;}
	// add labels to a string vector
	vector<string> labels = ParseBinaryStrings(lbl, lblSize);
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
	string actionWFile, string actionWLblFile, string transProbFile,
    string transWFile, string transWLblFile)
{
	ifstream::pos_type fileSize;
	ifstream file;
	int * aIdx = NULL;    // raw idx data
	char * lbl = NULL;    // raw labels
	double * aW = NULL;
	char * wLbl = NULL;
	double * tPr = NULL;
    double * tW = NULL;
    char * tWLbl = NULL;
	vector<TmpAction> actionVec;  // Vector of all action with actionVec[aId] according to file definitions.
	foundScp3 = false;

	idx aIdxSize = ReadBinary(actionIdxFile,aIdx);
	idx lblSize = ReadBinary(actionIdxLblFile,lbl);
	idx aWSize = ReadBinary(actionWFile,aW);
	idx wLblSize = ReadBinary(actionWLblFile,wLbl);
	idx tPrSize = ReadBinary(transProbFile,tPr);
    idx tWSize = 0;
    idx tWLblSize = 0;
    if (transWFile.length()>0 && transWLblFile.length()>0) {
        ifstream transWTest(transWFile.c_str(), ios::in|ios::binary);
        ifstream transWLblTest(transWLblFile.c_str(), ios::in|ios::binary);
        if (transWTest && transWLblTest) {
            transWTest.close();
            transWLblTest.close();
            tWSize = ReadBinary(transWFile,tW);
            tWLblSize = ReadBinary(transWLblFile,tWLbl);
        }
	}
	// add weight labels to HMDP
	vector<string> labels;
	if (wLblSize>0) {
		labels = ParseBinaryStrings(wLbl, wLblSize);
	}
	pHMDP->SetActionWeightNames(labels);
	wLblSize = labels.size();   // number of weights
	delete [] wLbl;

    idx transWLblCount = 0;
    if (tWLblSize>0) {
        labels = ParseBinaryStrings(tWLbl, tWLblSize);
        pHMDP->SetTransWeightNames(labels);
        transWLblCount = labels.size();
        delete [] tWLbl;
    } else {
        pHMDP->SetTransWeightNames(vector<string>());
    }

	// Models with states and no actions are valid. In that case the action,
	// action-weight and transition-probability files are empty, but weight label
	// files may still define the model's weight names.
	if (aIdxSize==0 && aWSize==0 && tPrSize==0) {
		delete [] aIdx;
		delete [] lbl;
		delete [] aW;
		delete [] tPr;
		delete [] tW;
		return;
	}

	// Note that all action arrays have the same number of rows (same number of
	// -1 row terminators). If one of the required action files is empty while
	// others contain data, the binary model is malformed.
	if (aIdxSize==0 || tPrSize==0) {okay = false; return;}

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

	if (wLblSize>0 && aWSize<actionVec.size()*wLblSize) {
		throw runtime_error("Action weight file has fewer values than required by the action weight labels.");
	}

	// scan aW
	vector<double> b;  // vector of doubles
	idx aId;
	for(aId=0; aId<actionVec.size(); aId++) {
		if (wLblSize>0) {
			b.assign(aW + aId*wLblSize, aW + (aId+1)*wLblSize);
			for (idx j=0;j<b.size();j++) {
				actionVec[aId].w.push_back((flt)b[j]);
			}
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

    if (tWSize>0 && transWLblCount>0) {
        prev=0;
        aId = 0;
        for(idx i=0; i<tWSize; i++) {
            if (tW[i]== -1) {
                b.assign(tW+prev, tW+i);
                idx transCount = actionVec[aId].pr.size();
                if (b.size() != transCount * transWLblCount) {
                    throw runtime_error("Transition weight row length does not match transitions times transition weight names.");
                }
                actionVec[aId].transW.resize(transCount);
                for (idx t=0; t<transCount; ++t) {
                    for (idx j=0; j<transWLblCount; ++j) {
                        actionVec[aId].transW[t].push_back((flt)b[t*transWLblCount+j]);
                    }
                }
                prev=i+1;
                aId++;
            }
        }
        delete [] tW;
    } else {
        for(aId=0; aId<actionVec.size(); aId++) {
            actionVec[aId].transW.resize(actionVec[aId].pr.size());
        }
    }

	// scan lbl
	// fix bug such that an mdp with no labels okay
	if (lblSize>0) {
   	labels = ParseBinaryStrings(lbl, lblSize);
   	// add labels to actions
   	for(idx i=0;i<labels.size();++i) {
   		if (i % 2 == 0) from_string<idx>(aId, labels[i], std::dec); // if i is even
   		else actionVec[aId].label = labels[i];
   	}
       delete [] lbl; //cout << "aSize: " << actionVec.size()<<endl;
	}
	
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

    bool hasActions = false;
    for (idx sId=0; sId<stateVec.size(); ++sId) {
        if (stateVec[sId].actions.size()>0) {
            hasActions = true;
            break;
        }
    }
    if (!hasActions) {
        vector<string> keys;
        set<string> keySet;
        pair<set<string>::iterator, bool> ret;
        for (idx sId=stateVec.size(); sId>0; --sId) {
            string str = pHMDP->GetStageStr(stateVec[sId-1].iHMDP);
            ret = keySet.insert(str);
            if (ret.second==true) keys.push_back(str);
        }
        pair< multimap<string, int >::iterator, multimap<string, int >::iterator > pairS;
        multimap<string, int>::iterator ite;
        for (idx i=0; i<keys.size(); i++) {
            pairS = stagesMap.equal_range(keys[i]);
            idx sSize;
            idx firstSId = pHMDP->states.size();
            for (ite=pairS.first, sSize = 0; ite!=pairS.second; ++ite, ++sSize) {
                idx sId = ite->second;
                pHMDP->states.push_back(HMDPState(stateVec[sId].label));
            }
            pHMDP->stages[keys[i]] = pair<idx,idx>(firstSId, sSize);
        }
        return;
    }

    // set state ids which are stored in idx of an action
    cpu.StartTimer();
    foundScp3 = false;
    for (idx sId=0; sId<stateVec.size(); ++sId) {
        if (!SetSIds(sId, foundScp3)) {
            okay = false;
            cpu.StopTimer();
            if (pHMDP->verbose) {pHMDP->log << "  Transform actions to internal data structure (" << cpu.ElapsedTime("sec") << " sec.)\n";}
            return;
        }
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
                sIte->AddAction(a.w, a.index, a.pr, a.transW, a.label);
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

bool HMDPReader::SetSIds(const idx & iState, bool & findValidOdr) {
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
                    if (pairNext.first==pairNext.second) {
                        pHMDP->log << "Error: State " << stateVec[iState].label << " (id = " << iState << "). Action "
                            << stateVec[iState].actions[a].label << " (" << a << ") has a transition to a non-existing state "
                            << "with scope " << scp[j] << " and id " << index[j] << "!" << endl;
                        return false;
                    }
                    iS = pairNext.first->second + index[j];
                    if (iS>=stateVec.size() || pHMDP->GetStageStr(stateVec[iS].iHMDP)!=stageNext) {
                        pHMDP->log << "Error: State " << stateVec[iState].label << " (id = " << iState << "). Action "
                            << stateVec[iState].actions[a].label << " (" << a << ") has a transition to a non-existing state "
                            << "with scope " << scp[j] << " and id " << index[j] << "!" << endl;
                        return false;
                    }
				}
				else {
                    ite = pairNext.first;
                    for (idx i=0; i<index[j] && ite!=pairNext.second; i++) ++ite;      // TODO This is very slow for stages with many states!! e.g. a ordinary big MDP. Current hack define your MDP using scp 3
                    if (ite==pairNext.second) {
                        pHMDP->log << "Error: State " << stateVec[iState].label << " (id = " << iState << "). Action "
                            << stateVec[iState].actions[a].label << " (" << a << ") has a transition to a non-existing state "
                            << "with scope " << scp[j] << " and id " << index[j] << "!" << endl;
                        return false;
                    }
                    iS = ite->second;
				}
			}
			if (scp[j]==0) { // next father stage
				ite = pairUp.first;
				for (idx i=0; i<index[j] && ite!=pairUp.second; i++) ++ite;
                if (ite==pairUp.second) {
                    pHMDP->log << "Error: State " << stateVec[iState].label << " (id = " << iState << "). Action "
                        << stateVec[iState].actions[a].label << " (" << a << ") has a transition to a non-existing state "
                        << "with scope " << scp[j] << " and id " << index[j] << "!" << endl;
                    return false;
                }
				iS = ite->second;
			}
			if (scp[j]==2) { // next child stage
                stageNextChild = pHMDP->GetNextChildStageStr(stateVec[iState].iHMDP, a);
                pair< multimap<string, int >::iterator, multimap<string, int >::iterator > pairDown = stagesMap.equal_range(stageNextChild);
                if (level+1==pHMDP->levels-1) { // check if child stage at lowest level -> states at a stage are defined in sequence.
                    if (pairDown.first==pairDown.second) {
                        pHMDP->log << "Error: State " << stateVec[iState].label << " (id = " << iState << "). Action "
                            << stateVec[iState].actions[a].label << " (" << a << ") has a transition to a non-existing state "
                            << "with scope " << scp[j] << " and id " << index[j] << "!" << endl;
                        return false;
                    }
                    iS = pairDown.first->second + index[j];
                    if (iS>=stateVec.size() || pHMDP->GetStageStr(stateVec[iS].iHMDP)!=stageNextChild) {
                        pHMDP->log << "Error: State " << stateVec[iState].label << " (id = " << iState << "). Action "
                            << stateVec[iState].actions[a].label << " (" << a << ") has a transition to a non-existing state "
                            << "with scope " << scp[j] << " and id " << index[j] << "!" << endl;
                        return false;
                    }
                } else {
                    ite = pairDown.first;
                    for (idx i=0; i<index[j] && ite!=pairDown.second; i++) ++ite;      // TODO This is very slow for stages with many states!! e.g. a ordinary big MDP. Current hack define your MDP using scope 3
                    if (ite==pairDown.second) {
                        pHMDP->log << "Error: State " << stateVec[iState].label << " (id = " << iState << "). Action "
                            << stateVec[iState].actions[a].label << " (" << a << ") has a transition to a non-existing state "
                            << "with scope " << scp[j] << " and id " << index[j] << "!" << endl;
                        return false;
                    }
                    iS = ite->second;
                }
			}
			if (scp[j]==3) { // specify state index/id
                if (index[j]>=stateVec.size()) {
                    pHMDP->log << "Error: State " << stateVec[iState].label << " (id = " << iState << "). Action "
                        << stateVec[iState].actions[a].label << " (" << a << ") has a transition to a non-existing state "
                        << "with scope " << scp[j] << " and id " << index[j] << "!" << endl;
                    return false;
                }
				iS = index[j];
				findValidOdr = true;    // possible that have to create new valid ordering.
			}
			sIds.push_back(iS);
		}
        stateVec[iState].actions[a].index = sIds;     // replace idx with state ids
        //cout << "(iS,iA) = (" << iState << "," << a << ") tails: " << vec2String(stateVec[iState].actions[a].index) << endl;
        stateVec[iState].actions[a].scp.clear();
	}
    return true;
}

// -----------------------------------------------------------------------------

void HMDPReader::AddExternal(string externalFile) {
    char * lbl;    // raw str
    string stageStr, prefix, tmp;
    idx lblSize = ReadBinary<char>(externalFile,lbl);
    if (lblSize==0) return;   // no external processes

    vector<string> labels = ParseBinaryStrings(lbl, lblSize);
    for (idx i=0; i+1<labels.size(); i += 2) {
        stageStr = labels[i];
        prefix = labels[i+1];
        pHMDP->external[stageStr] = prefix;
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
	out << endl << "Action weights: " << vec2String(weightActionNames) << endl;
    out << "Transition weights: " << vec2String(weightTransNames) << endl;
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

bool HMDP::ExternalStatesUpdate(BellmanOp op, OptSense sense, state_iterator iteS, string & curPrefix, HMDPPtr & pExt,
     const idx & idxW, const idx & idxD, const flt & g, const flt & discountF)
{
    //cout << "ExtStatesU: idxD=" << idxD << endl;
    string stageStr = iteS->label;     // external stage in HMDP corresponding to first stage in external
    string prefix = external[stageStr];    // prefix of external process //cout << "label: " << stageStr << " prefix: " << prefix << endl;
    ExternalAllocteMem(pExt, prefix, curPrefix);
    if (!okay) return false;
    string stageNextStr = GetNextStageStr(stageStr);  // external stage in HMDP corresponding to last stage in external
    vector<flt> weights = GetStageW(stageNextStr);   // get the weights from external nodes corresponding to last stage //cout << "next stage: " << stageNextStr << endl; //cout << "Start valueIte\n";
    pExt->ValueIte(op, sense, 1, 0, idxW, idxD, weights, g, discountF);
    string stageZeroExtStr = "0"; // first stage in external //cout << "Copy from external:" << endl;
    ExternalCopyWState(stageStr, stageZeroExtStr, pExt, false);   // copy weights to the HMDP //cout << "Update actions:" << endl;
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
	// weights
	pExt->SetStateWStage(stageLastExtStr,0);  // reset weights
	pExt->CalcPolicy(BellmanOp::Expected,idxW);
    for (state_iterator iteTo = state_begin(stageStr), iteFrom=pExt->state_begin(stageZeroExtStr);
         iteTo!=state_end(stageStr); ++iteTo, ++iteFrom)
    {
        if (w(iteTo,0,idxW)!=w(iteFrom)) newPolicy = true;      // a new policy of the external process have been found
        w(iteTo,0,idxW) = w(iteFrom); //cout << "Set rew of action in " << GetId(iteTo) << " to " << w(iteFrom) << endl;
    }
    // durations
    pExt->SetStateWStage(stageLastExtStr,0);  // reset weights
    pExt->CalcPolicy(BellmanOp::Expected,idxD);  // calc durations of external actions
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
        pExt->CalcPolicy(BellmanOp::TransPr);
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

flt HMDP::PolicyIte(BellmanOp op, OptSense sense, uSInt maxIte, const idx idxW, const idx idxD, const flt discountF) {
	//cout << "PolicyIte: idxD=" << idxD << endl;
	ResetLog();
    ValidateGlobalWeightForOp(op, idxW);
	if (timeHorizon<INFINT) {
		log << "Policy iteration can only be done on infinite time-horizon HMDPs!" << endl;
		return -INF;
	}
    log << "Run policy iteration ";
	switch (op) {
        case BellmanOp::Average: log << "under average expected-weight Bellman operator using \nweight '" <<
            GetWName(idxW) << "' over '" << GetWName(idxD) << "'. Iterations (g): " << endl;
            break;
        case BellmanOp::Discounted: log << "using weight '" << GetWName(idxW)
            << "' under discounted expected-weight Bellman operator \nwith '" << GetWName(idxD)
            << "' as duration using discount factor " << discountF
            << ". \nIteration(s): ";
            break;
        default: log << "Bellman operator not defined for policy iteration!" << endl; return -INF;
	}
	ExternalResetActions(idxW, idxD);
	timer.StartTimer();
	SetStateWStage("1", (flt)0);
	int rows = GetStateSize("0");
	arma::vec r(rows),   // Vector of founder weights
              w(rows),   // Vector of weights (the unknown)
              d(rows);   // Vector of denominator values
	arma::mat P(rows, rows); // Matrix of prob values
	flt g = 0;
	okay = true;
	bool newPred;
	SetPred(0); // default policy
	if (externalProc) CalcOptPolicy(op, sense, idxW, g, idxD, discountF);   // if external processes we have to find the optimal policy of the external processes and set external action w and trans pr
	for (idx k=1; ; ++k) { //cout << endl << "IteP:" << k << endl;
		if (verbose) log << endl; 
		log << k << " "; 
		if (verbose) log << endl;
		// find weights, dur, trans pr at founder given policy
		if (op==BellmanOp::Average) {
            FounderW(BellmanOp::Expected, r, idxW);
            FounderPr(BellmanOp::TransPr,P);
            FounderW(BellmanOp::Expected, d, idxD);
        }
        else {
            FounderW(op, r, idxW,g,idxD,discountF); //cout << "r mat: " << r << endl;
            FounderPr(BellmanOp::DiscountedTransPr,P,idxD,discountF); //cout << "P mat: " << P << endl;
        }
		// If Average solve equations h = r - dg + Ph where r, d and P have been calculated for the founder. This is equivalent to solving (I-P)h + dg = r -> (I-P,d)(h,g)' = r which is equivalent to solving Qw = r (equation (8.6.8) in Puterman) where last col in (I-P) replaced with d.
		// If Discounted solve equations w = r + Pw -> (I-P)w = r
		P *= -1.0;       // Set P := I-P
        P.diag() += 1.0;
		if (op==BellmanOp::Average) P.col(rows-1) = d;   // set implicit h_{rows-1}=0 and calc g here.
		if (!arma::solve(w, P, r)) {g = -INF; log << " Error: can not solve system equations. Is the model fulfilling the model assumptions (e.g. unichain)? "; break;}
		if (op==BellmanOp::Average) {
            g = w(rows-1);
            log << "(" << g << ") "; if (verbose) log << endl; //cout << "g=" << g << endl;
		} //cout << "w mat: " << w << endl;
		state_iterator iteL; idx j;
		for (iteL=state_begin("1"), j=0; iteL!=state_end("1"); ++iteL, ++j) {
            if (j<(idx)rows-1 ) HMDP::w(iteL) = w(j);
            else if (op==BellmanOp::Discounted) HMDP::w(iteL) = w(j);
		}
		// update policy
		newPred = CalcOptPolicy(op, sense, idxW, g, idxD, discountF);
		if (!okay) {g=-INF; break;}   // something went wrong (see the log)
		if (!newPred) {
			log << k+1;
			if (op==BellmanOp::Average) log << " (" << g << ") "; else log << " ";
			if (verbose) log << endl;
			break;    // optimal strategy found
		}
		if (k>=maxIte) { log << "\nReached upper limit of iterations! Seems to loop. \nIs the model fulfilling the model assumptions (e.g. unichain)?\n"; break;}
	}
	log << "finished. Cpu time: " << timer.ElapsedTime("sec") << " sec." << endl;
	if (op==BellmanOp::Average) return g; //cout << "Weights: " << vec2String(GetStageW("0")) << endl;
	return -INF;
}


//-----------------------------------------------------------------------------

flt HMDP::PolicyIteFixedPolicy(BellmanOp op, const idx idxW, const idx idxD, const flt discountF) {
	ResetLog();
    ValidateGlobalWeightForOp(op, idxW);
	if (timeHorizon<INFINT) {
		log << "Policy iteration can only be done on infinite time-horizon HMDPs!" << endl;
		return -INF;
	}
    log << "Run policy iteration (given a fixed policy) ";
	switch (op) {
        case BellmanOp::Average: log << "under average expected-weight Bellman operator using \nweight '" <<
            GetWName(idxW) << "' over '" << GetWName(idxD) << "'. Iterations (g):" << endl;
            break;
        case BellmanOp::Discounted: log << "using weight '" << GetWName(idxW)
            << "' under discounted expected-weight Bellman operator \nwith '" << GetWName(idxD)
            << "' as duration using discount factor " << discountF
            << ". \nIteration(s):";
            break;
        default: log << "Bellman operator not defined for policy iteration!" << endl; return -INF;
	}
	ExternalResetActions(idxW, idxD);
	timer.StartTimer();
	SetStateWStage("1", (flt)0);
	int rows = GetStateSize("0");
	arma::vec r(rows),   // Vector of founder weights
              w(rows),   // Vector of weights (the unknown)
              d(rows);   // Vector of denominator values
	arma::mat P(rows, rows); // Matrix of prob values
	flt g = 0;
	okay = true;
    for (state_iterator iteS = state_begin(); iteS!=state_end(); ++iteS) {
        idx actionSize = GetActionSize(iteS);
        if (actionSize>0 && (iteS->pred<0 || iteS->pred>=(int)actionSize)) {
            log << "Error: a valid fixed policy must be set before policyIteFixedPolicy()." << endl;
            return -INF;
        }
    }

    // find weights, dur, trans pr at founder given policy
    if (op==BellmanOp::Average) {
        FounderW(BellmanOp::Expected, r, idxW);
        FounderPr(BellmanOp::TransPr,P);
        FounderW(BellmanOp::Expected, d, idxD);
    }
    else {
        FounderW(op, r, idxW,g,idxD,discountF); //cout << "r mat: " << r << endl;
        FounderPr(BellmanOp::DiscountedTransPr,P,idxD,discountF); //cout << "P mat: " << P << endl;
    }
    // If Average solve equations h = r - dg + Ph where r, d and P have been calculated for the founder. This is equivalent to solving (I-P)h + dg = r -> (I-P,d)(h,g)' = r which is equivalent to solving Qw = r (equation (8.6.8) in Puterman) where last col in (I-P) replaced with d.
    // If Discounted solve equations w = r + Pw -> (I-P)w = r
    P *= -1.0;       // Set P := I-P
    P.diag() += 1.0;
    if (op==BellmanOp::Average) P.col(rows-1) = d;   // set implicit h_{rows-1}=0 and calc g here.
    if (!arma::solve(w, P, r)) {g = -INF; log << " Error: can not solve system equations. Is the model fulfilling the model assumptions (e.g. unichain)? "; return -INF;}
    if (op==BellmanOp::Average) {
        g = w(rows-1);
    }
    state_iterator iteL; idx j;
    for (iteL=state_begin("1"), j=0; iteL!=state_end("1"); ++iteL, ++j) {
        if (j<(idx)rows-1 ) HMDP::w(iteL) = w(j);
        else if (op==BellmanOp::Discounted) HMDP::w(iteL) = w(j);
    }
    // calc weights policy
    CalcPolicy(op, idxW, g, idxD, discountF);

	log << "finished. Cpu time: " << timer.ElapsedTime("sec") << " sec." << endl;
	if (op==BellmanOp::Average) return g; //cout << "Weights: " << vec2String(GetStageW("0")) << endl;
	return -INF;
}


// ----------------------------------------------------------------------------

void HMDP::ValueIte(BellmanOp op, OptSense sense, idx maxIte, flt epsilon, const idx idxW,
     const idx idxDur, vector<flt> & termValues,
     const flt g, const flt discountF)
{
	ResetLog();
    if (op!=BellmanOp::TransPr && op!=BellmanOp::DiscountedTransPr) ValidateGlobalWeightForOp(op, idxW);
	log << "Run value iteration with epsilon = " << epsilon  << " at most "
		<< maxIte << " time(s)" << endl << "using weight '" << GetWName(idxW) << "'";
	switch (op) {
        case BellmanOp::Average: log << " under average expected-weight Bellman operator given an average weight g = " << g << ".\n";
            maxIte = 1;     // not implemented more than one time yet
            break;
        case BellmanOp::Expected: log << " under expected-weight Bellman operator." << endl; break;
        case BellmanOp::Discounted: log << " under discounted expected-weight Bellman operator \nwith '" <<
            GetWName(idxDur) << "' as duration using discount factor " << discountF <<
            ".\nIterations:"; break;
        case BellmanOp::Min: log << " under minimum-successor Bellman operator." << endl; break;
        case BellmanOp::Max: log << " under maximum-successor Bellman operator." << endl; break;
        case BellmanOp::SecondMoment: log << " under second-moment Bellman operator." << endl; break;
        case BellmanOp::Variance: log << "Bellman operator not defined for value iteration!" << endl; return;
        default: log << "Bellman operator not defined for value iteration!" << endl; return;
	}
    if (op==BellmanOp::SecondMoment && timeHorizon>=INFINT) {
        log << "SecondMoment value iteration is currently only supported for finite time-horizon HMDPs." << endl;
        return;
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
    if (op==BellmanOp::SecondMoment) {
        WeightLevel level = ValidateGlobalWeightForOp(op, idxW);
        idx localIdxW = LocalWeightIdx(level, idxW);
        vector<flt> mean(states.size(), 0);
        for (iteS = state_begin(stageLastStr), iteV=termValues.begin(); iteS!=state_end(stageLastStr); ++iteS, ++iteV) {
            mean[GetId(iteS)] = *iteV;
            w(iteS) = (*iteV) * (*iteV);
        }
        CalcOptPolicySecondMoment(op, sense, level, localIdxW, mean);
        timer.StopTimer();
        log << " Finished. Cpu time " << timer.ElapsedTime("sec") << " sec." << endl;
        return;
    }
	idx i;
	for (i=1;; ++i) { //cout << "Ite: " << i+1 << endl;
        CalcOptPolicy(op,sense,idxW,g,idxDur,discountF);
		if (op==BellmanOp::Discounted)
            if(MaxDiffFounder()<epsilon) break;
		if (i<maxIte) {    // set next last stage values to stage zero values
            for (state_iterator iteZ = state_begin(stageZeroStr), iteL=state_begin(stageLastStr);
                iteZ!=state_end(stageZeroStr); ++iteZ, ++iteL)
                    w(iteL) = w(iteZ);
		}
		else break;
	}
	if (op==BellmanOp::Discounted && timeHorizon>=INFINT) log << " " << i;
	timer.StopTimer();
	log << " Finished. Cpu time " << timer.ElapsedTime("sec") << " sec." << endl;
	if ( (i==maxIte) & (maxIte!=1) ) log << "Reached upper limit of iterations! Should the limit be increased or \nis the model fulfilling the model assumptions (e.g. no periodicity)?\n";
}

// ----------------------------------------------------------------------------

bool HMDP::CalcOptPolicy(BellmanOp op, OptSense sense, idx idxW, flt g, idx idxDur, flt discountF) {
    if (op==BellmanOp::TransPr || op==BellmanOp::DiscountedTransPr) {
        return CalcOptPolicy(op, sense, WeightLevel::Action, idxW, g, idxDur, discountF);
    }
    WeightLevel level = ValidateGlobalWeightForOp(op, idxW);
    idx localIdxW = LocalWeightIdx(level, idxW);
    return CalcOptPolicy(op, sense, level, localIdxW, g, idxDur, discountF);
}

// Dispatch optimal-policy calculation to a specialized Bellman implementation.
bool HMDP::CalcOptPolicy(BellmanOp op, OptSense sense, WeightLevel level, idx idxW, flt g, idx idxDur, flt discountF) {
    if (level==WeightLevel::Transition && op!=BellmanOp::Expected && op!=BellmanOp::Min && op!=BellmanOp::Max && op!=BellmanOp::SecondMoment && op!=BellmanOp::Variance) {
        throw runtime_error("Transition-level weights are not supported for " + BellmanOpName(op) + ".");
    }
    if (sense==OptSense::Maximize) {
        if (op==BellmanOp::Expected && level==WeightLevel::Action) return CalcOptPolicyActionExpectedMax(idxW);
        if (op==BellmanOp::Expected && level==WeightLevel::Transition) return CalcOptPolicyTransitionExpectedMax(idxW);
        if (op==BellmanOp::Min && level==WeightLevel::Action) return CalcOptPolicyActionMinMax(idxW);
        if (op==BellmanOp::Min && level==WeightLevel::Transition) return CalcOptPolicyTransitionMinMax(idxW);
        if (op==BellmanOp::Max && level==WeightLevel::Action) return CalcOptPolicyActionMaxMax(idxW);
        if (op==BellmanOp::Max && level==WeightLevel::Transition) return CalcOptPolicyTransitionMaxMax(idxW);
        if (op==BellmanOp::SecondMoment) {
            vector<flt> mean(states.size(), 0);
            return CalcOptPolicySecondMoment(op, sense, level, idxW, mean);
        }
        if (op==BellmanOp::Average && level==WeightLevel::Action) return CalcOptPolicyActionAverageMax(idxW, g, idxDur);
        if (op==BellmanOp::Discounted && level==WeightLevel::Action) return CalcOptPolicyActionDiscountedMax(idxW, idxDur, discountF);
        if (op==BellmanOp::TransPr && level==WeightLevel::Action) return CalcOptPolicyActionTransPrMax();
        if (op==BellmanOp::DiscountedTransPr && level==WeightLevel::Action) return CalcOptPolicyActionDiscountedTransPrMax(idxDur, discountF);
    } else if (sense==OptSense::Minimize) {
        if (op==BellmanOp::Expected && level==WeightLevel::Action) return CalcOptPolicyActionExpectedMin(idxW);
        if (op==BellmanOp::Expected && level==WeightLevel::Transition) return CalcOptPolicyTransitionExpectedMin(idxW);
        if (op==BellmanOp::Min && level==WeightLevel::Action) return CalcOptPolicyActionMinMin(idxW);
        if (op==BellmanOp::Min && level==WeightLevel::Transition) return CalcOptPolicyTransitionMinMin(idxW);
        if (op==BellmanOp::Max && level==WeightLevel::Action) return CalcOptPolicyActionMaxMin(idxW);
        if (op==BellmanOp::Max && level==WeightLevel::Transition) return CalcOptPolicyTransitionMaxMin(idxW);
        if (op==BellmanOp::SecondMoment) {
            vector<flt> mean(states.size(), 0);
            return CalcOptPolicySecondMoment(op, sense, level, idxW, mean);
        }
        if (op==BellmanOp::Average && level==WeightLevel::Action) return CalcOptPolicyActionAverageMin(idxW, g, idxDur);
        if (op==BellmanOp::Discounted && level==WeightLevel::Action) return CalcOptPolicyActionDiscountedMin(idxW, idxDur, discountF);
        if (op==BellmanOp::TransPr && level==WeightLevel::Action) return CalcOptPolicyActionTransPrMin();
        if (op==BellmanOp::DiscountedTransPr && level==WeightLevel::Action) return CalcOptPolicyActionDiscountedTransPrMin(idxDur, discountF);
    } else {
        throw runtime_error("Invalid optimization sense.");
    }
    throw runtime_error("Bellman operator not implemented.");
}

string HMDP::BellmanOpName(BellmanOp op) const {
    switch (op) {
        case BellmanOp::Expected: return "BellmanOp::Expected";
        case BellmanOp::Discounted: return "BellmanOp::Discounted";
        case BellmanOp::Average: return "BellmanOp::Average";
        case BellmanOp::TransPr: return "BellmanOp::TransPr";
        case BellmanOp::DiscountedTransPr: return "BellmanOp::DiscountedTransPr";
        case BellmanOp::Min: return "BellmanOp::Min";
        case BellmanOp::Max: return "BellmanOp::Max";
        case BellmanOp::SecondMoment: return "BellmanOp::SecondMoment";
        case BellmanOp::Variance: return "BellmanOp::Variance";
    }
    return "Invalid Bellman operator";
}

string HMDP::OptSenseName(OptSense sense) const {
    switch (sense) {
        case OptSense::Maximize: return "OptSense::Maximize";
        case OptSense::Minimize: return "OptSense::Minimize";
    }
    return "Invalid optimization sense";
}

HMDP::WeightLevel HMDP::ValidateGlobalWeightForOp(BellmanOp op, idx iW) const {
    WeightLevel level = WeightLevelFromGlobalIdx(iW);
    if (level==WeightLevel::Transition && op!=BellmanOp::Expected && op!=BellmanOp::Min && op!=BellmanOp::Max && op!=BellmanOp::SecondMoment && op!=BellmanOp::Variance) {
        throw runtime_error("Transition-level weights are not supported for " + BellmanOpName(op) + ".");
    }
    return level;
}

// Validate that an action weight is present on every action.
void HMDP::CheckActionWeightsAvailable(idx idxW) const {
    if (idxW>=weightActionNames.size()) throw runtime_error("Action weight index out of range.");
    for (vector<HMDPState>::const_iterator iteS=states.begin(); iteS!=states.end(); ++iteS) {
        for (vector<HMDPAction>::const_iterator iteA=iteS->actions.begin(); iteA!=iteS->actions.end(); ++iteA) {
            if (idxW>=iteA->w.size()) throw runtime_error("Action weight value is missing for the requested index.");
        }
    }
}

// Validate that a transition weight is present on every transition.
void HMDP::CheckTransitionWeightsAvailable(idx idxW) const {
    if (idxW>=weightTransNames.size()) throw runtime_error("Transition weight index out of range.");
    for (vector<HMDPState>::const_iterator iteS=states.begin(); iteS!=states.end(); ++iteS) {
        for (vector<HMDPAction>::const_iterator iteA=iteS->actions.begin(); iteA!=iteS->actions.end(); ++iteA) {
            for (vector<HMDPTrans>::const_iterator iteT=iteA->trans.begin(); iteT!=iteA->trans.end(); ++iteT) {
                if (idxW>=iteT->w.size()) throw runtime_error("Transition-level weight missing for transition.");
            }
        }
    }
}

vector<flt> HMDP::CalcRPO(BellmanOp op, OptSense sense, vector<idx> & iS, idx idxW, vector<idx> & idxA, flt g, idx idxDur, flt discountF) {
    if (op==BellmanOp::TransPr) {
        if (sense==OptSense::Maximize) return CalcRPOActionTransPrMax(iS, idxA);
        if (sense==OptSense::Minimize) return CalcRPOActionTransPrMin(iS, idxA);
        throw runtime_error("Invalid optimization sense.");
    }
    if (op==BellmanOp::DiscountedTransPr) {
        if (sense==OptSense::Maximize) return CalcRPOActionDiscountedTransPrMax(iS, idxA, idxDur, discountF);
        if (sense==OptSense::Minimize) return CalcRPOActionDiscountedTransPrMin(iS, idxA, idxDur, discountF);
        throw runtime_error("Invalid optimization sense.");
    }

    WeightLevel level = ValidateGlobalWeightForOp(op, idxW);
    idx localIdxW = LocalWeightIdx(level, idxW);

    if (sense==OptSense::Maximize) {
        if (op==BellmanOp::Expected && level==WeightLevel::Action) return CalcRPOActionExpectedMax(iS, localIdxW, idxA);
        if (op==BellmanOp::Expected && level==WeightLevel::Transition) return CalcRPOTransitionExpectedMax(iS, localIdxW, idxA);
        if (op==BellmanOp::Average && level==WeightLevel::Action) return CalcRPOActionAverageMax(iS, localIdxW, idxA, g, idxDur);
        if (op==BellmanOp::Discounted && level==WeightLevel::Action) return CalcRPOActionDiscountedMax(iS, localIdxW, idxA, idxDur, discountF);
    } else if (sense==OptSense::Minimize) {
        if (op==BellmanOp::Expected && level==WeightLevel::Action) return CalcRPOActionExpectedMin(iS, localIdxW, idxA);
        if (op==BellmanOp::Expected && level==WeightLevel::Transition) return CalcRPOTransitionExpectedMin(iS, localIdxW, idxA);
        if (op==BellmanOp::Average && level==WeightLevel::Action) return CalcRPOActionAverageMin(iS, localIdxW, idxA, g, idxDur);
        if (op==BellmanOp::Discounted && level==WeightLevel::Action) return CalcRPOActionDiscountedMin(iS, localIdxW, idxA, idxDur, discountF);
    } else {
        throw runtime_error("Invalid optimization sense.");
    }

    throw runtime_error("Bellman operator not implemented.");
}

vector<flt> HMDP::CalcRPOActionExpectedMax(vector<idx> & iS, idx idxW, vector<idx> & idxA) {
    CheckActionWeightsAvailable(idxW);
    vector<flt> result;
    for (idx i=0; i<iS.size(); ++i) {
        flt wA = -INF;
        flt wMax = -INF;
        state_iterator iteS = GetIte(iS[i]);
        action_iterator iteAA = GetIte(iteS, idxA[i]);
        if ((GetActionSize(iteS)==0) || (GetActionSize(iteS)==1)) {
            result.push_back(-INF);
            continue;
        }
        for (action_iterator iteA = action_begin(iteS); iteA!=action_end(iteS); ++iteA) {
            flt wTmp = iteA->w[idxW];
            bool isMinInf = false;
            for (trans_iterator iteT = trans_begin(iteA); iteT!=trans_end(iteA); ++iteT) {
                flt nextW = states[iteT->id].w;
                if (nextW <= -INF) {
                    wTmp = -INF;
                    isMinInf = true;
                    break;
                }
                wTmp += iteT->pr * nextW;
            }
            if (isMinInf) continue;
            if (iteA==iteAA) wA = wTmp;
            else wMax = max(wMax, wTmp);
        }
        result.push_back(wA - wMax);
    }
    return result;
}

vector<flt> HMDP::CalcRPOTransitionExpectedMax(vector<idx> & iS, idx idxW, vector<idx> & idxA) {
    CheckTransitionWeightsAvailable(idxW);
    vector<flt> result;
    for (idx i=0; i<iS.size(); ++i) {
        flt wA = -INF;
        flt wMax = -INF;
        state_iterator iteS = GetIte(iS[i]);
        action_iterator iteAA = GetIte(iteS, idxA[i]);
        if ((GetActionSize(iteS)==0) || (GetActionSize(iteS)==1)) {
            result.push_back(-INF);
            continue;
        }
        for (action_iterator iteA = action_begin(iteS); iteA!=action_end(iteS); ++iteA) {
            flt wTmp = 0;
            bool isMinInf = false;
            for (trans_iterator iteT = trans_begin(iteA); iteT!=trans_end(iteA); ++iteT) {
                flt nextW = states[iteT->id].w;
                if (nextW <= -INF) {
                    wTmp = -INF;
                    isMinInf = true;
                    break;
                }
                wTmp += iteT->pr * (iteT->w[idxW] + nextW);
            }
            if (isMinInf) continue;
            if (iteA==iteAA) wA = wTmp;
            else wMax = max(wMax, wTmp);
        }
        result.push_back(wA - wMax);
    }
    return result;
}

vector<flt> HMDP::CalcRPOActionAverageMax(vector<idx> & iS, idx idxW, vector<idx> & idxA, flt g, idx idxDur) {
    CheckActionWeightsAvailable(idxW);
    CheckActionWeightsAvailable(idxDur);
    vector<flt> result;
    for (idx i=0; i<iS.size(); ++i) {
        flt wA = -INF;
        flt wMax = -INF;
        state_iterator iteS = GetIte(iS[i]);
        action_iterator iteAA = GetIte(iteS, idxA[i]);
        if ((GetActionSize(iteS)==0) || (GetActionSize(iteS)==1)) {
            result.push_back(-INF);
            continue;
        }
        for (action_iterator iteA = action_begin(iteS); iteA!=action_end(iteS); ++iteA) {
            flt wTmp = iteA->w[idxW] - iteA->w[idxDur] * g;
            bool isMinInf = false;
            for (trans_iterator iteT = trans_begin(iteA); iteT!=trans_end(iteA); ++iteT) {
                flt nextW = states[iteT->id].w;
                if (nextW <= -INF) {
                    wTmp = -INF;
                    isMinInf = true;
                    break;
                }
                wTmp += iteT->pr * nextW;
            }
            if (isMinInf) continue;
            if (iteA==iteAA) wA = wTmp;
            else wMax = max(wMax, wTmp);
        }
        result.push_back(wA - wMax);
    }
    return result;
}

vector<flt> HMDP::CalcRPOActionDiscountedMax(vector<idx> & iS, idx idxW, vector<idx> & idxA, idx idxDur, flt discountF) {
    CheckActionWeightsAvailable(idxW);
    CheckActionWeightsAvailable(idxDur);
    vector<flt> result;
    for (idx i=0; i<iS.size(); ++i) {
        flt wA = -INF;
        flt wMax = -INF;
        state_iterator iteS = GetIte(iS[i]);
        action_iterator iteAA = GetIte(iteS, idxA[i]);
        if ((GetActionSize(iteS)==0) || (GetActionSize(iteS)==1)) {
            result.push_back(-INF);
            continue;
        }
        for (action_iterator iteA = action_begin(iteS); iteA!=action_end(iteS); ++iteA) {
            flt wTmp = 0;
            bool isMinInf = false;
            for (trans_iterator iteT = trans_begin(iteA); iteT!=trans_end(iteA); ++iteT) {
                flt nextW = states[iteT->id].w;
                if (nextW <= -INF) {
                    wTmp = -INF;
                    isMinInf = true;
                    break;
                }
                wTmp += iteT->pr * nextW;
            }
            if (isMinInf) continue;
            wTmp = wTmp * pow(discountF, iteA->w[idxDur]) + iteA->w[idxW];
            if (iteA==iteAA) wA = wTmp;
            else wMax = max(wMax, wTmp);
        }
        result.push_back(wA - wMax);
    }
    return result;
}

vector<flt> HMDP::CalcRPOActionTransPrMax(vector<idx> & iS, vector<idx> & idxA) {
    vector<flt> result;
    for (idx i=0; i<iS.size(); ++i) {
        flt wA = -INF;
        flt wMax = -INF;
        state_iterator iteS = GetIte(iS[i]);
        action_iterator iteAA = GetIte(iteS, idxA[i]);
        if ((GetActionSize(iteS)==0) || (GetActionSize(iteS)==1)) {
            result.push_back(-INF);
            continue;
        }
        for (action_iterator iteA = action_begin(iteS); iteA!=action_end(iteS); ++iteA) {
            flt wTmp = 0;
            bool isMinInf = false;
            for (trans_iterator iteT = trans_begin(iteA); iteT!=trans_end(iteA); ++iteT) {
                flt nextW = states[iteT->id].w;
                if (nextW <= -INF) {
                    wTmp = -INF;
                    isMinInf = true;
                    break;
                }
                wTmp += iteT->pr * nextW;
            }
            if (isMinInf) continue;
            if (iteA==iteAA) wA = wTmp;
            else wMax = max(wMax, wTmp);
        }
        result.push_back(wA - wMax);
    }
    return result;
}

vector<flt> HMDP::CalcRPOActionDiscountedTransPrMax(vector<idx> & iS, vector<idx> & idxA, idx idxDur, flt discountF) {
    CheckActionWeightsAvailable(idxDur);
    vector<flt> result;
    for (idx i=0; i<iS.size(); ++i) {
        flt wA = -INF;
        flt wMax = -INF;
        state_iterator iteS = GetIte(iS[i]);
        action_iterator iteAA = GetIte(iteS, idxA[i]);
        if ((GetActionSize(iteS)==0) || (GetActionSize(iteS)==1)) {
            result.push_back(-INF);
            continue;
        }
        for (action_iterator iteA = action_begin(iteS); iteA!=action_end(iteS); ++iteA) {
            flt wTmp = 0;
            bool isMinInf = false;
            for (trans_iterator iteT = trans_begin(iteA); iteT!=trans_end(iteA); ++iteT) {
                flt nextW = states[iteT->id].w;
                if (nextW <= -INF) {
                    wTmp = -INF;
                    isMinInf = true;
                    break;
                }
                wTmp += iteT->pr * nextW;
            }
            if (isMinInf) continue;
            wTmp *= pow(discountF, iteA->w[idxDur]);
            if (iteA==iteAA) wA = wTmp;
            else wMax = max(wMax, wTmp);
        }
        result.push_back(wA - wMax);
    }
    return result;
}

vector<flt> HMDP::CalcRPOActionExpectedMin(vector<idx> & iS, idx idxW, vector<idx> & idxA) {
    CheckActionWeightsAvailable(idxW);
    vector<flt> result;
    for (idx i=0; i<iS.size(); ++i) {
        flt wA = INF;
        flt wMin = INF;
        state_iterator iteS = GetIte(iS[i]);
        action_iterator iteAA = GetIte(iteS, idxA[i]);
        if ((GetActionSize(iteS)==0) || (GetActionSize(iteS)==1)) {
            result.push_back(-INF);
            continue;
        }
        for (action_iterator iteA = action_begin(iteS); iteA!=action_end(iteS); ++iteA) {
            flt wTmp = iteA->w[idxW];
            bool isMinInf = false;
            for (trans_iterator iteT = trans_begin(iteA); iteT!=trans_end(iteA); ++iteT) {
                flt nextW = states[iteT->id].w;
                if (nextW <= -INF) {
                    wTmp = -INF;
                    isMinInf = true;
                    break;
                }
                wTmp += iteT->pr * nextW;
            }
            if (isMinInf) continue;
            if (iteA==iteAA) wA = wTmp;
            else wMin = min(wMin, wTmp);
        }
        result.push_back(wMin - wA);
    }
    return result;
}

vector<flt> HMDP::CalcRPOTransitionExpectedMin(vector<idx> & iS, idx idxW, vector<idx> & idxA) {
    CheckTransitionWeightsAvailable(idxW);
    vector<flt> result;
    for (idx i=0; i<iS.size(); ++i) {
        flt wA = INF;
        flt wMin = INF;
        state_iterator iteS = GetIte(iS[i]);
        action_iterator iteAA = GetIte(iteS, idxA[i]);
        if ((GetActionSize(iteS)==0) || (GetActionSize(iteS)==1)) {
            result.push_back(-INF);
            continue;
        }
        for (action_iterator iteA = action_begin(iteS); iteA!=action_end(iteS); ++iteA) {
            flt wTmp = 0;
            bool isMinInf = false;
            for (trans_iterator iteT = trans_begin(iteA); iteT!=trans_end(iteA); ++iteT) {
                flt nextW = states[iteT->id].w;
                if (nextW <= -INF) {
                    wTmp = -INF;
                    isMinInf = true;
                    break;
                }
                wTmp += iteT->pr * (iteT->w[idxW] + nextW);
            }
            if (isMinInf) continue;
            if (iteA==iteAA) wA = wTmp;
            else wMin = min(wMin, wTmp);
        }
        result.push_back(wMin - wA);
    }
    return result;
}

vector<flt> HMDP::CalcRPOActionAverageMin(vector<idx> & iS, idx idxW, vector<idx> & idxA, flt g, idx idxDur) {
    CheckActionWeightsAvailable(idxW);
    CheckActionWeightsAvailable(idxDur);
    vector<flt> result;
    for (idx i=0; i<iS.size(); ++i) {
        flt wA = INF;
        flt wMin = INF;
        state_iterator iteS = GetIte(iS[i]);
        action_iterator iteAA = GetIte(iteS, idxA[i]);
        if ((GetActionSize(iteS)==0) || (GetActionSize(iteS)==1)) {
            result.push_back(-INF);
            continue;
        }
        for (action_iterator iteA = action_begin(iteS); iteA!=action_end(iteS); ++iteA) {
            flt wTmp = iteA->w[idxW] - iteA->w[idxDur] * g;
            bool isMinInf = false;
            for (trans_iterator iteT = trans_begin(iteA); iteT!=trans_end(iteA); ++iteT) {
                flt nextW = states[iteT->id].w;
                if (nextW <= -INF) {
                    wTmp = -INF;
                    isMinInf = true;
                    break;
                }
                wTmp += iteT->pr * nextW;
            }
            if (isMinInf) continue;
            if (iteA==iteAA) wA = wTmp;
            else wMin = min(wMin, wTmp);
        }
        result.push_back(wMin - wA);
    }
    return result;
}

vector<flt> HMDP::CalcRPOActionDiscountedMin(vector<idx> & iS, idx idxW, vector<idx> & idxA, idx idxDur, flt discountF) {
    CheckActionWeightsAvailable(idxW);
    CheckActionWeightsAvailable(idxDur);
    vector<flt> result;
    for (idx i=0; i<iS.size(); ++i) {
        flt wA = INF;
        flt wMin = INF;
        state_iterator iteS = GetIte(iS[i]);
        action_iterator iteAA = GetIte(iteS, idxA[i]);
        if ((GetActionSize(iteS)==0) || (GetActionSize(iteS)==1)) {
            result.push_back(-INF);
            continue;
        }
        for (action_iterator iteA = action_begin(iteS); iteA!=action_end(iteS); ++iteA) {
            flt wTmp = 0;
            bool isMinInf = false;
            for (trans_iterator iteT = trans_begin(iteA); iteT!=trans_end(iteA); ++iteT) {
                flt nextW = states[iteT->id].w;
                if (nextW <= -INF) {
                    wTmp = -INF;
                    isMinInf = true;
                    break;
                }
                wTmp += iteT->pr * nextW;
            }
            if (isMinInf) continue;
            wTmp = wTmp * pow(discountF, iteA->w[idxDur]) + iteA->w[idxW];
            if (iteA==iteAA) wA = wTmp;
            else wMin = min(wMin, wTmp);
        }
        result.push_back(wMin - wA);
    }
    return result;
}

vector<flt> HMDP::CalcRPOActionTransPrMin(vector<idx> & iS, vector<idx> & idxA) {
    vector<flt> result;
    for (idx i=0; i<iS.size(); ++i) {
        flt wA = INF;
        flt wMin = INF;
        state_iterator iteS = GetIte(iS[i]);
        action_iterator iteAA = GetIte(iteS, idxA[i]);
        if ((GetActionSize(iteS)==0) || (GetActionSize(iteS)==1)) {
            result.push_back(-INF);
            continue;
        }
        for (action_iterator iteA = action_begin(iteS); iteA!=action_end(iteS); ++iteA) {
            flt wTmp = 0;
            bool isMinInf = false;
            for (trans_iterator iteT = trans_begin(iteA); iteT!=trans_end(iteA); ++iteT) {
                flt nextW = states[iteT->id].w;
                if (nextW <= -INF) {
                    wTmp = -INF;
                    isMinInf = true;
                    break;
                }
                wTmp += iteT->pr * nextW;
            }
            if (isMinInf) continue;
            if (iteA==iteAA) wA = wTmp;
            else wMin = min(wMin, wTmp);
        }
        result.push_back(wMin - wA);
    }
    return result;
}

vector<flt> HMDP::CalcRPOActionDiscountedTransPrMin(vector<idx> & iS, vector<idx> & idxA, idx idxDur, flt discountF) {
    CheckActionWeightsAvailable(idxDur);
    vector<flt> result;
    for (idx i=0; i<iS.size(); ++i) {
        flt wA = INF;
        flt wMin = INF;
        state_iterator iteS = GetIte(iS[i]);
        action_iterator iteAA = GetIte(iteS, idxA[i]);
        if ((GetActionSize(iteS)==0) || (GetActionSize(iteS)==1)) {
            result.push_back(-INF);
            continue;
        }
        for (action_iterator iteA = action_begin(iteS); iteA!=action_end(iteS); ++iteA) {
            flt wTmp = 0;
            bool isMinInf = false;
            for (trans_iterator iteT = trans_begin(iteA); iteT!=trans_end(iteA); ++iteT) {
                flt nextW = states[iteT->id].w;
                if (nextW <= -INF) {
                    wTmp = -INF;
                    isMinInf = true;
                    break;
                }
                wTmp += iteT->pr * nextW;
            }
            if (isMinInf) continue;
            wTmp *= pow(discountF, iteA->w[idxDur]);
            if (iteA==iteAA) wA = wTmp;
            else wMin = min(wMin, wTmp);
        }
        result.push_back(wMin - wA);
    }
    return result;
}

// Optimize a policy using action weights.
bool HMDP::CalcOptPolicyActionExpectedMax(idx idxW) {
    CheckActionWeightsAvailable(idxW);
    flt wTmp;
    bool newPred = false;
    bool isMinInf;
    int oldPred;
    string externalPrefix;
    HMDP * pExtProc = NULL;
    ExternalResetStates();
    for(state_iterator iteS = state_begin(); iteS!=state_end(); ++iteS) {
        if (ExternalState(iteS)) {
            if (iteS->w== -INF) newPred = ExternalStatesUpdate(BellmanOp::Expected, OptSense::Maximize, iteS, externalPrefix, pExtProc, idxW, 0, 0, 1);
            if (!okay) return false;
            pred(iteS) = 0;
        } else {
            if (GetActionSize(iteS)>0) iteS->w = -INF;
            oldPred = iteS->pred;
            for (action_iterator iteA = action_begin(iteS); iteA!=action_end(iteS); ++iteA) {
                // Action weight means r(s,a): add it once, outside the transition loop.
                wTmp = iteA->w[idxW];
                isMinInf = false;
                for (trans_iterator iteT = trans_begin(iteA); iteT!=trans_end(iteA); ++iteT) {
                    flt nextW = states[iteT->id].w;
                    if (nextW <= -INF) {
                        wTmp = -INF;
                        isMinInf = true;
                        break;
                    }
                    wTmp += iteT->pr * nextW;
                }
                if (isMinInf) continue;
                if (iteS->w < wTmp) {
                    iteS->w = wTmp;
                    iteS->pred = GetIdx(iteS,iteA);
                }
            }
            if (iteS->pred != oldPred) newPred = true;
        }
    }
    if (verbose && pExtProc!=NULL) log << "  Free memory of external process with prefix '" << externalPrefix << "'." << endl;
    delete pExtProc;
    return newPred;
}

// Optimize a policy using transition weights.
bool HMDP::CalcOptPolicyTransitionExpectedMax(idx idxW) {
    CheckTransitionWeightsAvailable(idxW);
    flt wTmp;
    bool newPred = false;
    bool isMinInf;
    int oldPred;
    for(state_iterator iteS = state_begin(); iteS!=state_end(); ++iteS) {
        if (ExternalState(iteS)) throw runtime_error("Transition-level weights are not implemented for external process states.");
        if (GetActionSize(iteS)>0) iteS->w = -INF;
        oldPred = iteS->pred;
        for (action_iterator iteA = action_begin(iteS); iteA!=action_end(iteS); ++iteA) {
            wTmp = 0;
            isMinInf = false;
            for (trans_iterator iteT = trans_begin(iteA); iteT!=trans_end(iteA); ++iteT) {
                // Transition weight means r(s,a,s'): add it per transition.
                flt nextW = states[iteT->id].w;
                if (nextW <= -INF) {
                    wTmp = -INF;
                    isMinInf = true;
                    break;
                }
                wTmp += iteT->pr * (iteT->w[idxW] + nextW);
            }
            if (isMinInf) continue;
            if (iteS->w < wTmp) {
                iteS->w = wTmp;
                iteS->pred = GetIdx(iteS,iteA);
            }
        }
        if (iteS->pred != oldPred) newPred = true;
    }
    return newPred;
}

// Optimize a policy using action-level average weights.
bool HMDP::CalcOptPolicyActionAverageMax(idx idxW, flt g, idx idxDur) {
    CheckActionWeightsAvailable(idxW);
    CheckActionWeightsAvailable(idxDur);
    flt wTmp;
    bool newPred = false;
    bool isMinInf;
    int oldPred;
    string externalPrefix;
    HMDP * pExtProc = NULL;
    ExternalResetStates();
    for(state_iterator iteS = state_begin(); iteS!=state_end(); ++iteS) {
        if (ExternalState(iteS)) {
            if (iteS->w== -INF) newPred = ExternalStatesUpdate(BellmanOp::Average, OptSense::Maximize, iteS, externalPrefix, pExtProc, idxW, idxDur, g, 1);
            if (!okay) return false;
            iteS->pred = 0;
        } else {
            if (GetActionSize(iteS)>0) iteS->w = -INF;
            oldPred = iteS->pred;
            for (action_iterator iteA = action_begin(iteS); iteA!=action_end(iteS); ++iteA) {
                wTmp = iteA->w[idxW] - iteA->w[idxDur] * g;
                isMinInf = false;
                for (trans_iterator iteT = trans_begin(iteA); iteT!=trans_end(iteA); ++iteT) {
                    flt nextW = states[iteT->id].w;
                    if (nextW <= -INF) {
                        wTmp = -INF;
                        isMinInf = true;
                        break;
                    }
                    wTmp += iteT->pr * nextW;
                }
                if (isMinInf) continue;
                if (iteS->w < wTmp) {
                    iteS->w = wTmp;
                    iteS->pred = GetIdx(iteS,iteA);
                }
            }
            if (iteS->pred != oldPred) newPred = true;
        }
    }
    if (verbose && pExtProc!=NULL) log << "  Free memory of external process with prefix '" << externalPrefix << "'." << endl;
    delete pExtProc;
    return newPred;
}

// Optimize a policy using action-level discounted weights.
bool HMDP::CalcOptPolicyActionDiscountedMax(idx idxW, idx idxDur, flt discountF) {
    CheckActionWeightsAvailable(idxW);
    CheckActionWeightsAvailable(idxDur);
    flt wTmp;
    bool newPred = false;
    bool isMinInf;
    int oldPred;
    string externalPrefix;
    HMDP * pExtProc = NULL;
    ExternalResetStates();
    for(state_iterator iteS = state_begin(); iteS!=state_end(); ++iteS) {
        if (ExternalState(iteS)) {
            if (iteS->w== -INF) newPred = ExternalStatesUpdate(BellmanOp::Discounted, OptSense::Maximize, iteS, externalPrefix, pExtProc, idxW, idxDur, 0, discountF);
            if (!okay) return false;
            iteS->pred = 0;
        } else {
            if (GetActionSize(iteS)>0) iteS->w = -INF;
            oldPred = iteS->pred;
            for (action_iterator iteA = action_begin(iteS); iteA!=action_end(iteS); ++iteA) {
                wTmp = 0;
                isMinInf = false;
                for (trans_iterator iteT = trans_begin(iteA); iteT!=trans_end(iteA); ++iteT) {
                    flt nextW = states[iteT->id].w;
                    if (nextW <= -INF) {
                        wTmp = -INF;
                        isMinInf = true;
                        break;
                    }
                    wTmp += iteT->pr * nextW;
                }
                if (isMinInf) continue;
                wTmp = wTmp * pow(discountF, iteA->w[idxDur]) + iteA->w[idxW];
                if (iteS->w < wTmp) {
                    iteS->w = wTmp;
                    iteS->pred = GetIdx(iteS,iteA);
                }
            }
            if (iteS->pred != oldPred) newPred = true;
        }
    }
    if (verbose && pExtProc!=NULL) log << "  Free memory of external process with prefix '" << externalPrefix << "'." << endl;
    delete pExtProc;
    return newPred;
}

/** Optimize a policy using transition probabilities as the Bellman value. */
bool HMDP::CalcOptPolicyActionTransPrMax() {
    flt wTmp;
    bool newPred = false;
    bool isMinInf;
    int oldPred;
    string externalPrefix;
    HMDP * pExtProc = NULL;
    ExternalResetStates();
    for(state_iterator iteS = state_begin(); iteS!=state_end(); ++iteS) {
        if (ExternalState(iteS)) {
            if (iteS->w== -INF) newPred = ExternalStatesUpdate(BellmanOp::TransPr, OptSense::Maximize, iteS, externalPrefix, pExtProc, 0, 0, 0, 1);
            if (!okay) return false;
            iteS->pred = 0;
        } else {
            if (GetActionSize(iteS)>0) iteS->w = -INF;
            oldPred = iteS->pred;
            for (action_iterator iteA = action_begin(iteS); iteA!=action_end(iteS); ++iteA) {
                wTmp = 0;
                isMinInf = false;
                for (trans_iterator iteT = trans_begin(iteA); iteT!=trans_end(iteA); ++iteT) {
                    flt nextW = states[iteT->id].w;
                    if (nextW <= -INF) {
                        wTmp = -INF;
                        isMinInf = true;
                        break;
                    }
                    wTmp += iteT->pr * nextW;
                }
                if (isMinInf) continue;
                if (iteS->w < wTmp) {
                    iteS->w = wTmp;
                    iteS->pred = GetIdx(iteS,iteA);
                }
            }
            if (iteS->pred != oldPred) newPred = true;
        }
    }
    if (verbose && pExtProc!=NULL) log << "  Free memory of external process with prefix '" << externalPrefix << "'." << endl;
    delete pExtProc;
    return newPred;
}

/** Optimize a policy using discounted transition probabilities as the Bellman value. */
bool HMDP::CalcOptPolicyActionDiscountedTransPrMax(idx idxDur, flt discountF) {
    CheckActionWeightsAvailable(idxDur);
    flt wTmp;
    bool newPred = false;
    bool isMinInf;
    int oldPred;
    string externalPrefix;
    HMDP * pExtProc = NULL;
    ExternalResetStates();
    for(state_iterator iteS = state_begin(); iteS!=state_end(); ++iteS) {
        if (ExternalState(iteS)) {
            if (iteS->w== -INF) newPred = ExternalStatesUpdate(BellmanOp::DiscountedTransPr, OptSense::Maximize, iteS, externalPrefix, pExtProc, 0, idxDur, 0, discountF);
            if (!okay) return false;
            iteS->pred = 0;
        } else {
            if (GetActionSize(iteS)>0) iteS->w = -INF;
            oldPred = iteS->pred;
            for (action_iterator iteA = action_begin(iteS); iteA!=action_end(iteS); ++iteA) {
                wTmp = 0;
                isMinInf = false;
                for (trans_iterator iteT = trans_begin(iteA); iteT!=trans_end(iteA); ++iteT) {
                    flt nextW = states[iteT->id].w;
                    if (nextW <= -INF) {
                        wTmp = -INF;
                        isMinInf = true;
                        break;
                    }
                    wTmp += iteT->pr * nextW;
                }
                if (isMinInf) continue;
                wTmp *= pow(discountF, iteA->w[idxDur]);
                if (iteS->w < wTmp) {
                    iteS->w = wTmp;
                    iteS->pred = GetIdx(iteS,iteA);
                }
            }
            if (iteS->pred != oldPred) newPred = true;
        }
    }
    if (verbose && pExtProc!=NULL) log << "  Free memory of external process with prefix '" << externalPrefix << "'." << endl;
    delete pExtProc;
    return newPred;
}

bool HMDP::CalcOptPolicyActionExpectedMin(idx idxW) {
    CheckActionWeightsAvailable(idxW);
    flt wTmp;
    bool newPred = false;
    bool isMinInf;
    int oldPred;
    string externalPrefix;
    HMDP * pExtProc = NULL;
    ExternalResetStates();
    for(state_iterator iteS = state_begin(); iteS!=state_end(); ++iteS) {
        if (ExternalState(iteS)) {
            if (iteS->w== -INF) newPred = ExternalStatesUpdate(BellmanOp::Expected, OptSense::Minimize, iteS, externalPrefix, pExtProc, idxW, 0, 0, 1);
            if (!okay) return false;
            pred(iteS) = 0;
        } else {
            if (GetActionSize(iteS)>0) iteS->w = INF;
            oldPred = iteS->pred;
            for (action_iterator iteA = action_begin(iteS); iteA!=action_end(iteS); ++iteA) {
                wTmp = iteA->w[idxW];
                isMinInf = false;
                for (trans_iterator iteT = trans_begin(iteA); iteT!=trans_end(iteA); ++iteT) {
                    flt nextW = states[iteT->id].w;
                    if (nextW <= -INF) {
                        wTmp = -INF;
                        isMinInf = true;
                        break;
                    }
                    wTmp += iteT->pr * nextW;
                }
                if (isMinInf) continue;
                if (iteS->w > wTmp) {
                    iteS->w = wTmp;
                    iteS->pred = GetIdx(iteS,iteA);
                }
            }
            if (iteS->pred != oldPred) newPred = true;
        }
    }
    if (verbose && pExtProc!=NULL) log << "  Free memory of external process with prefix '" << externalPrefix << "'." << endl;
    delete pExtProc;
    return newPred;
}

bool HMDP::CalcOptPolicyTransitionExpectedMin(idx idxW) {
    CheckTransitionWeightsAvailable(idxW);
    flt wTmp;
    bool newPred = false;
    bool isMinInf;
    int oldPred;
    for(state_iterator iteS = state_begin(); iteS!=state_end(); ++iteS) {
        if (ExternalState(iteS)) throw runtime_error("Transition-level weights are not implemented for external process states.");
        if (GetActionSize(iteS)>0) iteS->w = INF;
        oldPred = iteS->pred;
        for (action_iterator iteA = action_begin(iteS); iteA!=action_end(iteS); ++iteA) {
            wTmp = 0;
            isMinInf = false;
            for (trans_iterator iteT = trans_begin(iteA); iteT!=trans_end(iteA); ++iteT) {
                flt nextW = states[iteT->id].w;
                if (nextW <= -INF) {
                    wTmp = -INF;
                    isMinInf = true;
                    break;
                }
                wTmp += iteT->pr * (iteT->w[idxW] + nextW);
            }
            if (isMinInf) continue;
            if (iteS->w > wTmp) {
                iteS->w = wTmp;
                iteS->pred = GetIdx(iteS,iteA);
            }
        }
        if (iteS->pred != oldPred) newPred = true;
    }
    return newPred;
}

bool HMDP::CalcOptPolicyActionSecondMomentMax(idx idxW, vector<flt> &mean) {
    CheckActionWeightsAvailable(idxW);
    flt wTmp;
    flt uTmp;
    bool newPred = false;
    bool isMinInf;
    int oldPred;
    for(state_iterator iteS = state_begin(); iteS!=state_end(); ++iteS) {
        if (ExternalState(iteS)) throw runtime_error("SecondMoment is not implemented for external process states.");
        idx iS = GetId(iteS);
        if (GetActionSize(iteS)>0) iteS->w = -INF;
        else {
            iteS->w = mean[iS] * mean[iS];
            continue;
        }
        oldPred = iteS->pred;
        for (action_iterator iteA = action_begin(iteS); iteA!=action_end(iteS); ++iteA) {
            wTmp = 0;
            uTmp = iteA->w[idxW];
            isMinInf = false;
            for (trans_iterator iteT = trans_begin(iteA); iteT!=trans_end(iteA); ++iteT) {
                flt qNext = states[iteT->id].w;
                if (qNext <= -INF) {
                    wTmp = -INF;
                    isMinInf = true;
                    break;
                }
                flt y = iteA->w[idxW];
                wTmp += iteT->pr * (y * y + 2 * y * mean[iteT->id] + qNext);
                uTmp += iteT->pr * mean[iteT->id];
            }
            if (isMinInf) continue;
            if (iteS->w < wTmp) {
                iteS->w = wTmp;
                mean[iS] = uTmp;
                iteS->pred = GetIdx(iteS,iteA);
            }
        }
        if (iteS->pred != oldPred) newPred = true;
    }
    return newPred;
}

bool HMDP::CalcOptPolicyActionSecondMomentMin(idx idxW, vector<flt> &mean) {
    CheckActionWeightsAvailable(idxW);
    flt wTmp;
    flt uTmp;
    bool newPred = false;
    bool isMinInf;
    int oldPred;
    for(state_iterator iteS = state_begin(); iteS!=state_end(); ++iteS) {
        if (ExternalState(iteS)) throw runtime_error("SecondMoment is not implemented for external process states.");
        idx iS = GetId(iteS);
        if (GetActionSize(iteS)>0) iteS->w = INF;
        else {
            iteS->w = mean[iS] * mean[iS];
            continue;
        }
        oldPred = iteS->pred;
        for (action_iterator iteA = action_begin(iteS); iteA!=action_end(iteS); ++iteA) {
            wTmp = 0;
            uTmp = iteA->w[idxW];
            isMinInf = false;
            for (trans_iterator iteT = trans_begin(iteA); iteT!=trans_end(iteA); ++iteT) {
                flt qNext = states[iteT->id].w;
                if (qNext <= -INF) {
                    wTmp = -INF;
                    isMinInf = true;
                    break;
                }
                flt y = iteA->w[idxW];
                wTmp += iteT->pr * (y * y + 2 * y * mean[iteT->id] + qNext);
                uTmp += iteT->pr * mean[iteT->id];
            }
            if (isMinInf) continue;
            if (iteS->w > wTmp) {
                iteS->w = wTmp;
                mean[iS] = uTmp;
                iteS->pred = GetIdx(iteS,iteA);
            }
        }
        if (iteS->pred != oldPred) newPred = true;
    }
    return newPred;
}

bool HMDP::CalcOptPolicyTransitionSecondMomentMax(idx idxW, vector<flt> &mean) {
    CheckTransitionWeightsAvailable(idxW);
    flt wTmp;
    flt uTmp;
    bool newPred = false;
    bool isMinInf;
    int oldPred;
    for(state_iterator iteS = state_begin(); iteS!=state_end(); ++iteS) {
        if (ExternalState(iteS)) throw runtime_error("Transition-level weights are not implemented for external process states.");
        idx iS = GetId(iteS);
        if (GetActionSize(iteS)>0) iteS->w = -INF;
        else {
            iteS->w = mean[iS] * mean[iS];
            continue;
        }
        oldPred = iteS->pred;
        for (action_iterator iteA = action_begin(iteS); iteA!=action_end(iteS); ++iteA) {
            wTmp = 0;
            uTmp = 0;
            isMinInf = false;
            for (trans_iterator iteT = trans_begin(iteA); iteT!=trans_end(iteA); ++iteT) {
                flt qNext = states[iteT->id].w;
                if (qNext <= -INF) {
                    wTmp = -INF;
                    isMinInf = true;
                    break;
                }
                flt y = iteT->w[idxW];
                wTmp += iteT->pr * (y * y + 2 * y * mean[iteT->id] + qNext);
                uTmp += iteT->pr * (y + mean[iteT->id]);
            }
            if (isMinInf) continue;
            if (iteS->w < wTmp) {
                iteS->w = wTmp;
                mean[iS] = uTmp;
                iteS->pred = GetIdx(iteS,iteA);
            }
        }
        if (iteS->pred != oldPred) newPred = true;
    }
    return newPred;
}

bool HMDP::CalcOptPolicyTransitionSecondMomentMin(idx idxW, vector<flt> &mean) {
    CheckTransitionWeightsAvailable(idxW);
    flt wTmp;
    flt uTmp;
    bool newPred = false;
    bool isMinInf;
    int oldPred;
    for(state_iterator iteS = state_begin(); iteS!=state_end(); ++iteS) {
        if (ExternalState(iteS)) throw runtime_error("Transition-level weights are not implemented for external process states.");
        idx iS = GetId(iteS);
        if (GetActionSize(iteS)>0) iteS->w = INF;
        else {
            iteS->w = mean[iS] * mean[iS];
            continue;
        }
        oldPred = iteS->pred;
        for (action_iterator iteA = action_begin(iteS); iteA!=action_end(iteS); ++iteA) {
            wTmp = 0;
            uTmp = 0;
            isMinInf = false;
            for (trans_iterator iteT = trans_begin(iteA); iteT!=trans_end(iteA); ++iteT) {
                flt qNext = states[iteT->id].w;
                if (qNext <= -INF) {
                    wTmp = -INF;
                    isMinInf = true;
                    break;
                }
                flt y = iteT->w[idxW];
                wTmp += iteT->pr * (y * y + 2 * y * mean[iteT->id] + qNext);
                uTmp += iteT->pr * (y + mean[iteT->id]);
            }
            if (isMinInf) continue;
            if (iteS->w > wTmp) {
                iteS->w = wTmp;
                mean[iS] = uTmp;
                iteS->pred = GetIdx(iteS,iteA);
            }
        }
        if (iteS->pred != oldPred) newPred = true;
    }
    return newPred;
}

bool HMDP::CalcOptPolicyActionAverageMin(idx idxW, flt g, idx idxDur) {
    CheckActionWeightsAvailable(idxW);
    CheckActionWeightsAvailable(idxDur);
    flt wTmp;
    bool newPred = false;
    bool isMinInf;
    int oldPred;
    string externalPrefix;
    HMDP * pExtProc = NULL;
    ExternalResetStates();
    for(state_iterator iteS = state_begin(); iteS!=state_end(); ++iteS) {
        if (ExternalState(iteS)) {
            if (iteS->w== -INF) newPred = ExternalStatesUpdate(BellmanOp::Average, OptSense::Minimize, iteS, externalPrefix, pExtProc, idxW, idxDur, g, 1);
            if (!okay) return false;
            iteS->pred = 0;
        } else {
            if (GetActionSize(iteS)>0) iteS->w = INF;
            oldPred = iteS->pred;
            for (action_iterator iteA = action_begin(iteS); iteA!=action_end(iteS); ++iteA) {
                wTmp = iteA->w[idxW] - iteA->w[idxDur] * g;
                isMinInf = false;
                for (trans_iterator iteT = trans_begin(iteA); iteT!=trans_end(iteA); ++iteT) {
                    flt nextW = states[iteT->id].w;
                    if (nextW <= -INF) {
                        wTmp = -INF;
                        isMinInf = true;
                        break;
                    }
                    wTmp += iteT->pr * nextW;
                }
                if (isMinInf) continue;
                if (iteS->w > wTmp) {
                    iteS->w = wTmp;
                    iteS->pred = GetIdx(iteS,iteA);
                }
            }
            if (iteS->pred != oldPred) newPred = true;
        }
    }
    if (verbose && pExtProc!=NULL) log << "  Free memory of external process with prefix '" << externalPrefix << "'." << endl;
    delete pExtProc;
    return newPred;
}

bool HMDP::CalcOptPolicyActionDiscountedMin(idx idxW, idx idxDur, flt discountF) {
    CheckActionWeightsAvailable(idxW);
    CheckActionWeightsAvailable(idxDur);
    flt wTmp;
    bool newPred = false;
    bool isMinInf;
    int oldPred;
    string externalPrefix;
    HMDP * pExtProc = NULL;
    ExternalResetStates();
    for(state_iterator iteS = state_begin(); iteS!=state_end(); ++iteS) {
        if (ExternalState(iteS)) {
            if (iteS->w== -INF) newPred = ExternalStatesUpdate(BellmanOp::Discounted, OptSense::Minimize, iteS, externalPrefix, pExtProc, idxW, idxDur, 0, discountF);
            if (!okay) return false;
            iteS->pred = 0;
        } else {
            if (GetActionSize(iteS)>0) iteS->w = INF;
            oldPred = iteS->pred;
            for (action_iterator iteA = action_begin(iteS); iteA!=action_end(iteS); ++iteA) {
                wTmp = 0;
                isMinInf = false;
                for (trans_iterator iteT = trans_begin(iteA); iteT!=trans_end(iteA); ++iteT) {
                    flt nextW = states[iteT->id].w;
                    if (nextW <= -INF) {
                        wTmp = -INF;
                        isMinInf = true;
                        break;
                    }
                    wTmp += iteT->pr * nextW;
                }
                if (isMinInf) continue;
                wTmp = wTmp * pow(discountF, iteA->w[idxDur]) + iteA->w[idxW];
                if (iteS->w > wTmp) {
                    iteS->w = wTmp;
                    iteS->pred = GetIdx(iteS,iteA);
                }
            }
            if (iteS->pred != oldPred) newPred = true;
        }
    }
    if (verbose && pExtProc!=NULL) log << "  Free memory of external process with prefix '" << externalPrefix << "'." << endl;
    delete pExtProc;
    return newPred;
}

bool HMDP::CalcOptPolicyActionTransPrMin() {
    flt wTmp;
    bool newPred = false;
    bool isMinInf;
    int oldPred;
    string externalPrefix;
    HMDP * pExtProc = NULL;
    ExternalResetStates();
    for(state_iterator iteS = state_begin(); iteS!=state_end(); ++iteS) {
        if (ExternalState(iteS)) {
            if (iteS->w== -INF) newPred = ExternalStatesUpdate(BellmanOp::TransPr, OptSense::Minimize, iteS, externalPrefix, pExtProc, 0, 0, 0, 1);
            if (!okay) return false;
            iteS->pred = 0;
        } else {
            if (GetActionSize(iteS)>0) iteS->w = INF;
            oldPred = iteS->pred;
            for (action_iterator iteA = action_begin(iteS); iteA!=action_end(iteS); ++iteA) {
                wTmp = 0;
                isMinInf = false;
                for (trans_iterator iteT = trans_begin(iteA); iteT!=trans_end(iteA); ++iteT) {
                    flt nextW = states[iteT->id].w;
                    if (nextW <= -INF) {
                        wTmp = -INF;
                        isMinInf = true;
                        break;
                    }
                    wTmp += iteT->pr * nextW;
                }
                if (isMinInf) continue;
                if (iteS->w > wTmp) {
                    iteS->w = wTmp;
                    iteS->pred = GetIdx(iteS,iteA);
                }
            }
            if (iteS->pred != oldPred) newPred = true;
        }
    }
    if (verbose && pExtProc!=NULL) log << "  Free memory of external process with prefix '" << externalPrefix << "'." << endl;
    delete pExtProc;
    return newPred;
}

bool HMDP::CalcOptPolicyActionDiscountedTransPrMin(idx idxDur, flt discountF) {
    CheckActionWeightsAvailable(idxDur);
    flt wTmp;
    bool newPred = false;
    bool isMinInf;
    int oldPred;
    string externalPrefix;
    HMDP * pExtProc = NULL;
    ExternalResetStates();
    for(state_iterator iteS = state_begin(); iteS!=state_end(); ++iteS) {
        if (ExternalState(iteS)) {
            if (iteS->w== -INF) newPred = ExternalStatesUpdate(BellmanOp::DiscountedTransPr, OptSense::Minimize, iteS, externalPrefix, pExtProc, 0, idxDur, 0, discountF);
            if (!okay) return false;
            iteS->pred = 0;
        } else {
            if (GetActionSize(iteS)>0) iteS->w = INF;
            oldPred = iteS->pred;
            for (action_iterator iteA = action_begin(iteS); iteA!=action_end(iteS); ++iteA) {
                wTmp = 0;
                isMinInf = false;
                for (trans_iterator iteT = trans_begin(iteA); iteT!=trans_end(iteA); ++iteT) {
                    flt nextW = states[iteT->id].w;
                    if (nextW <= -INF) {
                        wTmp = -INF;
                        isMinInf = true;
                        break;
                    }
                    wTmp += iteT->pr * nextW;
                }
                if (isMinInf) continue;
                wTmp *= pow(discountF, iteA->w[idxDur]);
                if (iteS->w > wTmp) {
                    iteS->w = wTmp;
                    iteS->pred = GetIdx(iteS,iteA);
                }
            }
            if (iteS->pred != oldPred) newPred = true;
        }
    }
    if (verbose && pExtProc!=NULL) log << "  Free memory of external process with prefix '" << externalPrefix << "'." << endl;
    delete pExtProc;
    return newPred;
}

bool HMDP::CalcOptPolicyActionMinMax(idx idxW) {
    CheckActionWeightsAvailable(idxW);
    flt wTmp;
    flt inner;
    bool hasNext;
    bool newPred = false;
    int oldPred;
    string externalPrefix;
    HMDP * pExtProc = NULL;
    ExternalResetStates();
    for(state_iterator iteS = state_begin(); iteS!=state_end(); ++iteS) {
        if (ExternalState(iteS)) {
            if (iteS->w== -INF) newPred = ExternalStatesUpdate(BellmanOp::Min, OptSense::Maximize, iteS, externalPrefix, pExtProc, idxW, 0, 0, 1);
            if (!okay) return false;
            pred(iteS) = 0;
        } else {
            if (GetActionSize(iteS)>0) iteS->w = -INF;
            oldPred = iteS->pred;
            for (action_iterator iteA = action_begin(iteS); iteA!=action_end(iteS); ++iteA) {
                inner = INF;
                hasNext = false;
                for (trans_iterator iteT = trans_begin(iteA); iteT!=trans_end(iteA); ++iteT) {
                    if (iteT->pr<=0) continue;
                    hasNext = true;
                    inner = min(inner, states[iteT->id].w);
                }
                wTmp = iteA->w[idxW] + (hasNext ? inner : 0);
                if (iteS->w < wTmp) {
                    iteS->w = wTmp;
                    iteS->pred = GetIdx(iteS,iteA);
                }
            }
            if (iteS->pred != oldPred) newPred = true;
        }
    }
    if (verbose && pExtProc!=NULL) log << "  Free memory of external process with prefix '" << externalPrefix << "'." << endl;
    delete pExtProc;
    return newPred;
}

bool HMDP::CalcOptPolicyActionMinMin(idx idxW) {
    CheckActionWeightsAvailable(idxW);
    flt wTmp;
    flt inner;
    bool hasNext;
    bool newPred = false;
    int oldPred;
    string externalPrefix;
    HMDP * pExtProc = NULL;
    ExternalResetStates();
    for(state_iterator iteS = state_begin(); iteS!=state_end(); ++iteS) {
        if (ExternalState(iteS)) {
            if (iteS->w== -INF) newPred = ExternalStatesUpdate(BellmanOp::Min, OptSense::Minimize, iteS, externalPrefix, pExtProc, idxW, 0, 0, 1);
            if (!okay) return false;
            pred(iteS) = 0;
        } else {
            if (GetActionSize(iteS)>0) iteS->w = INF;
            oldPred = iteS->pred;
            for (action_iterator iteA = action_begin(iteS); iteA!=action_end(iteS); ++iteA) {
                inner = INF;
                hasNext = false;
                for (trans_iterator iteT = trans_begin(iteA); iteT!=trans_end(iteA); ++iteT) {
                    if (iteT->pr<=0) continue;
                    hasNext = true;
                    inner = min(inner, states[iteT->id].w);
                }
                wTmp = iteA->w[idxW] + (hasNext ? inner : 0);
                if (iteS->w > wTmp) {
                    iteS->w = wTmp;
                    iteS->pred = GetIdx(iteS,iteA);
                }
            }
            if (iteS->pred != oldPred) newPred = true;
        }
    }
    if (verbose && pExtProc!=NULL) log << "  Free memory of external process with prefix '" << externalPrefix << "'." << endl;
    delete pExtProc;
    return newPred;
}

bool HMDP::CalcOptPolicyTransitionMinMax(idx idxW) {
    CheckTransitionWeightsAvailable(idxW);
    flt wTmp;
    bool hasNext;
    bool newPred = false;
    int oldPred;
    for(state_iterator iteS = state_begin(); iteS!=state_end(); ++iteS) {
        if (ExternalState(iteS)) throw runtime_error("Transition-level weights are not implemented for external process states.");
        if (GetActionSize(iteS)>0) iteS->w = -INF;
        oldPred = iteS->pred;
        for (action_iterator iteA = action_begin(iteS); iteA!=action_end(iteS); ++iteA) {
            wTmp = INF;
            hasNext = false;
            for (trans_iterator iteT = trans_begin(iteA); iteT!=trans_end(iteA); ++iteT) {
                if (iteT->pr<=0) continue;
                hasNext = true;
                wTmp = min(wTmp, iteT->w[idxW] + states[iteT->id].w);
            }
            if (!hasNext) wTmp = 0;
            if (iteS->w < wTmp) {
                iteS->w = wTmp;
                iteS->pred = GetIdx(iteS,iteA);
            }
        }
        if (iteS->pred != oldPred) newPred = true;
    }
    return newPred;
}

bool HMDP::CalcOptPolicyTransitionMinMin(idx idxW) {
    CheckTransitionWeightsAvailable(idxW);
    flt wTmp;
    bool hasNext;
    bool newPred = false;
    int oldPred;
    for(state_iterator iteS = state_begin(); iteS!=state_end(); ++iteS) {
        if (ExternalState(iteS)) throw runtime_error("Transition-level weights are not implemented for external process states.");
        if (GetActionSize(iteS)>0) iteS->w = INF;
        oldPred = iteS->pred;
        for (action_iterator iteA = action_begin(iteS); iteA!=action_end(iteS); ++iteA) {
            wTmp = INF;
            hasNext = false;
            for (trans_iterator iteT = trans_begin(iteA); iteT!=trans_end(iteA); ++iteT) {
                if (iteT->pr<=0) continue;
                hasNext = true;
                wTmp = min(wTmp, iteT->w[idxW] + states[iteT->id].w);
            }
            if (!hasNext) wTmp = 0;
            if (iteS->w > wTmp) {
                iteS->w = wTmp;
                iteS->pred = GetIdx(iteS,iteA);
            }
        }
        if (iteS->pred != oldPred) newPred = true;
    }
    return newPred;
}

bool HMDP::CalcOptPolicyActionMaxMax(idx idxW) {
    CheckActionWeightsAvailable(idxW);
    flt wTmp;
    flt inner;
    bool hasNext;
    bool newPred = false;
    int oldPred;
    string externalPrefix;
    HMDP * pExtProc = NULL;
    ExternalResetStates();
    for(state_iterator iteS = state_begin(); iteS!=state_end(); ++iteS) {
        if (ExternalState(iteS)) {
            if (iteS->w== -INF) newPred = ExternalStatesUpdate(BellmanOp::Max, OptSense::Maximize, iteS, externalPrefix, pExtProc, idxW, 0, 0, 1);
            if (!okay) return false;
            pred(iteS) = 0;
        } else {
            if (GetActionSize(iteS)>0) iteS->w = -INF;
            oldPred = iteS->pred;
            for (action_iterator iteA = action_begin(iteS); iteA!=action_end(iteS); ++iteA) {
                inner = -INF;
                hasNext = false;
                for (trans_iterator iteT = trans_begin(iteA); iteT!=trans_end(iteA); ++iteT) {
                    if (iteT->pr<=0) continue;
                    hasNext = true;
                    inner = max(inner, states[iteT->id].w);
                }
                wTmp = iteA->w[idxW] + (hasNext ? inner : 0);
                if (iteS->w < wTmp) {
                    iteS->w = wTmp;
                    iteS->pred = GetIdx(iteS,iteA);
                }
            }
            if (iteS->pred != oldPred) newPred = true;
        }
    }
    if (verbose && pExtProc!=NULL) log << "  Free memory of external process with prefix '" << externalPrefix << "'." << endl;
    delete pExtProc;
    return newPred;
}

bool HMDP::CalcOptPolicyActionMaxMin(idx idxW) {
    CheckActionWeightsAvailable(idxW);
    flt wTmp;
    flt inner;
    bool hasNext;
    bool newPred = false;
    int oldPred;
    string externalPrefix;
    HMDP * pExtProc = NULL;
    ExternalResetStates();
    for(state_iterator iteS = state_begin(); iteS!=state_end(); ++iteS) {
        if (ExternalState(iteS)) {
            if (iteS->w== -INF) newPred = ExternalStatesUpdate(BellmanOp::Max, OptSense::Minimize, iteS, externalPrefix, pExtProc, idxW, 0, 0, 1);
            if (!okay) return false;
            pred(iteS) = 0;
        } else {
            if (GetActionSize(iteS)>0) iteS->w = INF;
            oldPred = iteS->pred;
            for (action_iterator iteA = action_begin(iteS); iteA!=action_end(iteS); ++iteA) {
                inner = -INF;
                hasNext = false;
                for (trans_iterator iteT = trans_begin(iteA); iteT!=trans_end(iteA); ++iteT) {
                    if (iteT->pr<=0) continue;
                    hasNext = true;
                    inner = max(inner, states[iteT->id].w);
                }
                wTmp = iteA->w[idxW] + (hasNext ? inner : 0);
                if (iteS->w > wTmp) {
                    iteS->w = wTmp;
                    iteS->pred = GetIdx(iteS,iteA);
                }
            }
            if (iteS->pred != oldPred) newPred = true;
        }
    }
    if (verbose && pExtProc!=NULL) log << "  Free memory of external process with prefix '" << externalPrefix << "'." << endl;
    delete pExtProc;
    return newPred;
}

bool HMDP::CalcOptPolicyTransitionMaxMax(idx idxW) {
    CheckTransitionWeightsAvailable(idxW);
    flt wTmp;
    bool hasNext;
    bool newPred = false;
    int oldPred;
    for(state_iterator iteS = state_begin(); iteS!=state_end(); ++iteS) {
        if (ExternalState(iteS)) throw runtime_error("Transition-level weights are not implemented for external process states.");
        if (GetActionSize(iteS)>0) iteS->w = -INF;
        oldPred = iteS->pred;
        for (action_iterator iteA = action_begin(iteS); iteA!=action_end(iteS); ++iteA) {
            wTmp = -INF;
            hasNext = false;
            for (trans_iterator iteT = trans_begin(iteA); iteT!=trans_end(iteA); ++iteT) {
                if (iteT->pr<=0) continue;
                hasNext = true;
                wTmp = max(wTmp, iteT->w[idxW] + states[iteT->id].w);
            }
            if (!hasNext) wTmp = 0;
            if (iteS->w < wTmp) {
                iteS->w = wTmp;
                iteS->pred = GetIdx(iteS,iteA);
            }
        }
        if (iteS->pred != oldPred) newPred = true;
    }
    return newPred;
}

bool HMDP::CalcOptPolicyTransitionMaxMin(idx idxW) {
    CheckTransitionWeightsAvailable(idxW);
    flt wTmp;
    bool hasNext;
    bool newPred = false;
    int oldPred;
    for(state_iterator iteS = state_begin(); iteS!=state_end(); ++iteS) {
        if (ExternalState(iteS)) throw runtime_error("Transition-level weights are not implemented for external process states.");
        if (GetActionSize(iteS)>0) iteS->w = INF;
        oldPred = iteS->pred;
        for (action_iterator iteA = action_begin(iteS); iteA!=action_end(iteS); ++iteA) {
            wTmp = -INF;
            hasNext = false;
            for (trans_iterator iteT = trans_begin(iteA); iteT!=trans_end(iteA); ++iteT) {
                if (iteT->pr<=0) continue;
                hasNext = true;
                wTmp = max(wTmp, iteT->w[idxW] + states[iteT->id].w);
            }
            if (!hasNext) wTmp = 0;
            if (iteS->w > wTmp) {
                iteS->w = wTmp;
                iteS->pred = GetIdx(iteS,iteA);
            }
        }
        if (iteS->pred != oldPred) newPred = true;
    }
    return newPred;
}

// ----------------------------------------------------------------------------

void HMDP::CalcPolicyActionMean(idx idxW, vector<flt> &mean) {
    CheckActionWeightsAvailable(idxW);
    mean.assign(states.size(), 0);
    for(state_iterator iteS = state_begin(); iteS!=state_end(); ++iteS) {
        idx iS = GetId(iteS);
        if (GetActionSize(iteS)>0) {
            action_iterator iteA = GetIte(iteS, iteS->pred);
            flt uTmp = iteA->w[idxW];
            for (trans_iterator iteT = trans_begin(iteA); iteT!=trans_end(iteA); ++iteT) {
                uTmp += iteT->pr * mean[iteT->id];
            }
            mean[iS] = uTmp;
        } else {
            mean[iS] = iteS->w;
        }
    }
}

void HMDP::CalcPolicyTransitionMean(idx idxW, vector<flt> &mean) {
    CheckTransitionWeightsAvailable(idxW);
    mean.assign(states.size(), 0);
    for(state_iterator iteS = state_begin(); iteS!=state_end(); ++iteS) {
        if (ExternalState(iteS)) throw runtime_error("Transition-level weights are not implemented for external process states.");
        idx iS = GetId(iteS);
        if (GetActionSize(iteS)>0) {
            action_iterator iteA = GetIte(iteS, iteS->pred);
            flt uTmp = 0;
            for (trans_iterator iteT = trans_begin(iteA); iteT!=trans_end(iteA); ++iteT) {
                uTmp += iteT->pr * (iteT->w[idxW] + mean[iteT->id]);
            }
            mean[iS] = uTmp;
        } else {
            mean[iS] = iteS->w;
        }
    }
}

bool HMDP::CalcOptPolicySecondMoment(BellmanOp op, OptSense sense, WeightLevel level, idx idxW, vector<flt> &mean) {
    if (op!=BellmanOp::SecondMoment) throw runtime_error("Invalid Bellman operator for second-moment optimization.");
    if (sense==OptSense::Maximize) {
        if (level==WeightLevel::Action) return CalcOptPolicyActionSecondMomentMax(idxW, mean);
        if (level==WeightLevel::Transition) return CalcOptPolicyTransitionSecondMomentMax(idxW, mean);
    } else if (sense==OptSense::Minimize) {
        if (level==WeightLevel::Action) return CalcOptPolicyActionSecondMomentMin(idxW, mean);
        if (level==WeightLevel::Transition) return CalcOptPolicyTransitionSecondMomentMin(idxW, mean);
    }
    throw runtime_error("Bellman operator not implemented.");
}

void HMDP::CalcPolicy(BellmanOp op, idx idxW, flt g, idx idxDur, flt discountF) {
    if (op==BellmanOp::TransPr || op==BellmanOp::DiscountedTransPr) {
        CalcPolicy(op, WeightLevel::Action, idxW, g, idxDur, discountF);
        return;
    }
    WeightLevel level = ValidateGlobalWeightForOp(op, idxW);
    idx localIdxW = LocalWeightIdx(level, idxW);
    CalcPolicy(op, level, localIdxW, g, idxDur, discountF);
}

// Dispatch fixed-policy evaluation to a specialized Bellman implementation.
void HMDP::CalcPolicy(BellmanOp op, WeightLevel level, idx idxW, flt g, idx idxDur, flt discountF) {
    if (level==WeightLevel::Transition && op!=BellmanOp::Expected && op!=BellmanOp::Min && op!=BellmanOp::Max && op!=BellmanOp::SecondMoment && op!=BellmanOp::Variance) {
        throw runtime_error("Transition-level weights are not supported for " + BellmanOpName(op) + ".");
    }
    if (op==BellmanOp::Expected && level==WeightLevel::Action) {
        CalcPolicyActionWeight(idxW);
        return;
    }
    if (op==BellmanOp::Expected && level==WeightLevel::Transition) {
        CalcPolicyTransitionWeight(idxW);
        return;
    }
    if (op==BellmanOp::Min && level==WeightLevel::Action) {
        CalcPolicyMinActionWeight(idxW);
        return;
    }
    if (op==BellmanOp::Min && level==WeightLevel::Transition) {
        CalcPolicyMinTransitionWeight(idxW);
        return;
    }
    if (op==BellmanOp::Max && level==WeightLevel::Action) {
        CalcPolicyMaxActionWeight(idxW);
        return;
    }
    if (op==BellmanOp::Max && level==WeightLevel::Transition) {
        CalcPolicyMaxTransitionWeight(idxW);
        return;
    }
    if (op==BellmanOp::SecondMoment && level==WeightLevel::Action) {
        CalcPolicyActionSecondMoment(idxW);
        return;
    }
    if (op==BellmanOp::SecondMoment && level==WeightLevel::Transition) {
        CalcPolicyTransitionSecondMoment(idxW);
        return;
    }
    if (op==BellmanOp::Variance && level==WeightLevel::Action) {
        CalcPolicyActionVariance(idxW);
        return;
    }
    if (op==BellmanOp::Variance && level==WeightLevel::Transition) {
        CalcPolicyTransitionVariance(idxW);
        return;
    }
    if (op==BellmanOp::Average && level==WeightLevel::Action) {
        CalcPolicyActionAverageWeight(idxW, g, idxDur);
        return;
    }
    if (op==BellmanOp::Discounted && level==WeightLevel::Action) {
        CalcPolicyActionDiscountedWeight(idxW, idxDur, discountF);
        return;
    }
    if (op==BellmanOp::TransPr && level==WeightLevel::Action) {
        CalcPolicyActionTransPr();
        return;
    }
    if (op==BellmanOp::DiscountedTransPr && level==WeightLevel::Action) {
        CalcPolicyActionDiscountedTransPr(idxDur, discountF);
        return;
    }
    throw runtime_error("Bellman operator not implemented.");
}

// Evaluate the current policy using action weights.
void HMDP::CalcPolicyActionWeight(idx idxW) {
    CheckActionWeightsAvailable(idxW);
    flt wTmp;
    for(state_iterator iteS = state_begin(); iteS!=state_end(); ++iteS) {
        if (GetActionSize(iteS)>0) {
            action_iterator iteA = GetIte(iteS, iteS->pred);
            // Action weight means r(s,a): add it once, outside the transition loop.
            wTmp = iteA->w[idxW];
            for (trans_iterator iteT = trans_begin(iteA); iteT!=trans_end(iteA); ++iteT) {
                wTmp += iteT->pr * states[iteT->id].w;
            }
            iteS->w = wTmp;
        }
    }
}

// Evaluate the current policy using transition weights.
void HMDP::CalcPolicyTransitionWeight(idx idxW) {
    CheckTransitionWeightsAvailable(idxW);
    flt wTmp;
    for(state_iterator iteS = state_begin(); iteS!=state_end(); ++iteS) {
        if (ExternalState(iteS)) throw runtime_error("Transition-level weights are not implemented for external process states.");
        if (GetActionSize(iteS)>0) {
            action_iterator iteA = GetIte(iteS, iteS->pred);
            wTmp = 0;
            for (trans_iterator iteT = trans_begin(iteA); iteT!=trans_end(iteA); ++iteT) {
                // Transition weight means r(s,a,s'): add it per transition.
                wTmp += iteT->pr * (iteT->w[idxW] + states[iteT->id].w);
            }
            iteS->w = wTmp;
        }
    }
}

void HMDP::CalcPolicyActionSecondMoment(idx idxW) {
    CheckActionWeightsAvailable(idxW);
    vector<flt> mean;
    CalcPolicyActionMean(idxW, mean);
    for(state_iterator iteS = state_begin(); iteS!=state_end(); ++iteS) {
        if (GetActionSize(iteS)==0) iteS->w = mean[GetId(iteS)] * mean[GetId(iteS)];
    }
    for(state_iterator iteS = state_begin(); iteS!=state_end(); ++iteS) {
        if (GetActionSize(iteS)>0) {
            action_iterator iteA = GetIte(iteS, iteS->pred);
            flt qTmp = 0;
            for (trans_iterator iteT = trans_begin(iteA); iteT!=trans_end(iteA); ++iteT) {
                flt y = iteA->w[idxW];
                qTmp += iteT->pr * (y * y + 2 * y * mean[iteT->id] + states[iteT->id].w);
            }
            iteS->w = qTmp;
        }
    }
}

void HMDP::CalcPolicyTransitionSecondMoment(idx idxW) {
    CheckTransitionWeightsAvailable(idxW);
    vector<flt> mean;
    CalcPolicyTransitionMean(idxW, mean);
    for(state_iterator iteS = state_begin(); iteS!=state_end(); ++iteS) {
        if (GetActionSize(iteS)==0) iteS->w = mean[GetId(iteS)] * mean[GetId(iteS)];
    }
    for(state_iterator iteS = state_begin(); iteS!=state_end(); ++iteS) {
        if (ExternalState(iteS)) throw runtime_error("Transition-level weights are not implemented for external process states.");
        if (GetActionSize(iteS)>0) {
            action_iterator iteA = GetIte(iteS, iteS->pred);
            flt qTmp = 0;
            for (trans_iterator iteT = trans_begin(iteA); iteT!=trans_end(iteA); ++iteT) {
                flt y = iteT->w[idxW];
                qTmp += iteT->pr * (y * y + 2 * y * mean[iteT->id] + states[iteT->id].w);
            }
            iteS->w = qTmp;
        }
    }
}

void HMDP::CalcPolicyActionVariance(idx idxW) {
    CheckActionWeightsAvailable(idxW);
    vector<flt> mean;
    CalcPolicyActionMean(idxW, mean);
    for(state_iterator iteS = state_begin(); iteS!=state_end(); ++iteS) {
        if (GetActionSize(iteS)==0) iteS->w = 0;
    }
    for(state_iterator iteS = state_begin(); iteS!=state_end(); ++iteS) {
        if (GetActionSize(iteS)>0) {
            idx iS = GetId(iteS);
            action_iterator iteA = GetIte(iteS, iteS->pred);
            flt vTmp = 0;
            flt uCur = mean[iS];
            for (trans_iterator iteT = trans_begin(iteA); iteT!=trans_end(iteA); ++iteT) {
                flt y = iteA->w[idxW];
                flt centered = y + mean[iteT->id] - uCur;
                vTmp += iteT->pr * (states[iteT->id].w + centered * centered);
            }
            iteS->w = vTmp;
        }
    }
}

void HMDP::CalcPolicyTransitionVariance(idx idxW) {
    CheckTransitionWeightsAvailable(idxW);
    vector<flt> mean;
    CalcPolicyTransitionMean(idxW, mean);
    for(state_iterator iteS = state_begin(); iteS!=state_end(); ++iteS) {
        if (GetActionSize(iteS)==0) iteS->w = 0;
    }
    for(state_iterator iteS = state_begin(); iteS!=state_end(); ++iteS) {
        if (ExternalState(iteS)) throw runtime_error("Transition-level weights are not implemented for external process states.");
        if (GetActionSize(iteS)>0) {
            idx iS = GetId(iteS);
            action_iterator iteA = GetIte(iteS, iteS->pred);
            flt vTmp = 0;
            flt uCur = mean[iS];
            for (trans_iterator iteT = trans_begin(iteA); iteT!=trans_end(iteA); ++iteT) {
                flt y = iteT->w[idxW];
                flt centered = y + mean[iteT->id] - uCur;
                vTmp += iteT->pr * (states[iteT->id].w + centered * centered);
            }
            iteS->w = vTmp;
        }
    }
}

// Evaluate the current policy using action weights and the worst feasible successor.
void HMDP::CalcPolicyMinActionWeight(idx idxW) {
    CheckActionWeightsAvailable(idxW);
    flt wTmp;
    flt inner;
    bool hasNext;
    for(state_iterator iteS = state_begin(); iteS!=state_end(); ++iteS) {
        if (GetActionSize(iteS)>0) {
            action_iterator iteA = GetIte(iteS, iteS->pred);
            inner = INF;
            hasNext = false;
            for (trans_iterator iteT = trans_begin(iteA); iteT!=trans_end(iteA); ++iteT) {
                if (iteT->pr<=0) continue;
                hasNext = true;
                inner = min(inner, states[iteT->id].w);
            }
            wTmp = iteA->w[idxW] + (hasNext ? inner : 0);
            iteS->w = wTmp;
        }
    }
}

// Evaluate the current policy using transition weights and the worst feasible successor.
void HMDP::CalcPolicyMinTransitionWeight(idx idxW) {
    CheckTransitionWeightsAvailable(idxW);
    flt wTmp;
    bool hasNext;
    for(state_iterator iteS = state_begin(); iteS!=state_end(); ++iteS) {
        if (ExternalState(iteS)) throw runtime_error("Transition-level weights are not implemented for external process states.");
        if (GetActionSize(iteS)>0) {
            action_iterator iteA = GetIte(iteS, iteS->pred);
            wTmp = INF;
            hasNext = false;
            for (trans_iterator iteT = trans_begin(iteA); iteT!=trans_end(iteA); ++iteT) {
                if (iteT->pr<=0) continue;
                hasNext = true;
                wTmp = min(wTmp, iteT->w[idxW] + states[iteT->id].w);
            }
            iteS->w = hasNext ? wTmp : 0;
        }
    }
}

// Evaluate the current policy using action weights and the best feasible successor.
void HMDP::CalcPolicyMaxActionWeight(idx idxW) {
    CheckActionWeightsAvailable(idxW);
    flt wTmp;
    flt inner;
    bool hasNext;
    for(state_iterator iteS = state_begin(); iteS!=state_end(); ++iteS) {
        if (GetActionSize(iteS)>0) {
            action_iterator iteA = GetIte(iteS, iteS->pred);
            inner = -INF;
            hasNext = false;
            for (trans_iterator iteT = trans_begin(iteA); iteT!=trans_end(iteA); ++iteT) {
                if (iteT->pr<=0) continue;
                hasNext = true;
                inner = max(inner, states[iteT->id].w);
            }
            wTmp = iteA->w[idxW] + (hasNext ? inner : 0);
            iteS->w = wTmp;
        }
    }
}

// Evaluate the current policy using transition weights and the best feasible successor.
void HMDP::CalcPolicyMaxTransitionWeight(idx idxW) {
    CheckTransitionWeightsAvailable(idxW);
    flt wTmp;
    bool hasNext;
    for(state_iterator iteS = state_begin(); iteS!=state_end(); ++iteS) {
        if (ExternalState(iteS)) throw runtime_error("Transition-level weights are not implemented for external process states.");
        if (GetActionSize(iteS)>0) {
            action_iterator iteA = GetIte(iteS, iteS->pred);
            wTmp = -INF;
            hasNext = false;
            for (trans_iterator iteT = trans_begin(iteA); iteT!=trans_end(iteA); ++iteT) {
                if (iteT->pr<=0) continue;
                hasNext = true;
                wTmp = max(wTmp, iteT->w[idxW] + states[iteT->id].w);
            }
            iteS->w = hasNext ? wTmp : 0;
        }
    }
}

// Evaluate the current policy using action-level average weights.
void HMDP::CalcPolicyActionAverageWeight(idx idxW, flt g, idx idxDur) {
    CheckActionWeightsAvailable(idxW);
    CheckActionWeightsAvailable(idxDur);
    flt wTmp;
    for(state_iterator iteS = state_begin(); iteS!=state_end(); ++iteS) {
        if (GetActionSize(iteS)>0) {
            action_iterator iteA = GetIte(iteS, iteS->pred);
            wTmp = iteA->w[idxW] - iteA->w[idxDur] * g;
            for (trans_iterator iteT = trans_begin(iteA); iteT!=trans_end(iteA); ++iteT) {
                wTmp += iteT->pr * states[iteT->id].w;
            }
            iteS->w = wTmp;
        }
    }
}

// Evaluate the current policy using action-level discounted weights.
void HMDP::CalcPolicyActionDiscountedWeight(idx idxW, idx idxDur, flt discountF) {
    CheckActionWeightsAvailable(idxW);
    CheckActionWeightsAvailable(idxDur);
    flt wTmp;
    for(state_iterator iteS = state_begin(); iteS!=state_end(); ++iteS) {
        if (GetActionSize(iteS)>0) {
            action_iterator iteA = GetIte(iteS, iteS->pred);
            wTmp = 0;
            for (trans_iterator iteT = trans_begin(iteA); iteT!=trans_end(iteA); ++iteT) {
                wTmp += iteT->pr * states[iteT->id].w;
            }
            iteS->w = wTmp * pow(discountF, iteA->w[idxDur]) + iteA->w[idxW];
        }
    }
}

// Evaluate the current policy using transition probabilities.
void HMDP::CalcPolicyActionTransPr() {
    flt wTmp;
    for(state_iterator iteS = state_begin(); iteS!=state_end(); ++iteS) {
        if (GetActionSize(iteS)>0) {
            action_iterator iteA = GetIte(iteS, iteS->pred);
            wTmp = 0;
            for (trans_iterator iteT = trans_begin(iteA); iteT!=trans_end(iteA); ++iteT) {
                wTmp += iteT->pr * states[iteT->id].w;
            }
            iteS->w = wTmp;
        }
    }
}

// Evaluate the current policy using discounted transition probabilities.
void HMDP::CalcPolicyActionDiscountedTransPr(idx idxDur, flt discountF) {
    CheckActionWeightsAvailable(idxDur);
    flt wTmp;
    for(state_iterator iteS = state_begin(); iteS!=state_end(); ++iteS) {
        if (GetActionSize(iteS)>0) {
            action_iterator iteA = GetIte(iteS, iteS->pred);
            wTmp = 0;
            for (trans_iterator iteT = trans_begin(iteA); iteT!=trans_end(iteA); ++iteT) {
                wTmp += iteT->pr * states[iteT->id].w;
            }
            iteS->w = wTmp * pow(discountF, iteA->w[idxDur]);
        }
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

// ----------------------------------------------------------------------------

void HMDP::Save2Binary(string prefix){
    HMDPSave hmdpSave(prefix, this);
    ResetLog();
    log << hmdpSave.log.str();
}

// ----------------------------------------------------------------------------

HMDPSave::HMDPSave(string prefix, HMDP * pHMDP){
    string stateIdxFileN = prefix + "stateIdx.bin";
    string stateIdxLblFileN = prefix + "stateIdxLbl.bin";
    string actionIdxFileN = prefix + "actionIdx.bin";
    string actionIdxLblFileN = prefix + "actionIdxLbl.bin";
    string actionWFileN = prefix + "actionWeight.bin";
    string actionWLblFileN = prefix + "actionWeightLbl.bin";
    string transProbFileN = prefix + "transProb.bin";
    string transWFileN = prefix + "transWeight.bin";
    string transWLblFileN = prefix + "transWeightLbl.bin";
    string externalProcessesFileN = prefix + "externalProcesses.bin";
    this->pHMDP = pHMDP;

    pStateIdxFile = fopen(stateIdxFileN.c_str(), "wb");
    pStateIdxLblFile = fopen(stateIdxLblFileN.c_str(), "wb");
    pActionIdxFile = fopen(actionIdxFileN.c_str(), "wb");
    pActionIdxLblFile = fopen(actionIdxLblFileN.c_str(), "wb");
    pActionWFile = fopen(actionWFileN.c_str(), "wb");
    pActionWLblFile = fopen(actionWLblFileN.c_str(), "wb");
    pTransProbFile = fopen(transProbFileN.c_str(), "wb");
    pTransWFile = fopen(transWFileN.c_str(), "wb");
    pTransWLblFile = fopen(transWLblFileN.c_str(), "wb");
    pExternalProcessesFile = fopen(externalProcessesFileN.c_str(), "wb");

    CreateBinaryFiles();
}

// ----------------------------------------------------------------------------

void HMDPSave::CreateBinaryFiles() {
    timer.StartTimer();
    HMDP::state_iterator iteS;
    sId = 0;
    if (pHMDP->timeHorizon>=INFINT) {   // assume that level "1" starts with sId=0!!
        pair<idx,idx> pN = pHMDP->stages["1"];
        iteS = pHMDP->state_begin() + pN.second;   // drop the first states at stage "1"
        sId = pN.second;
    }
    else {
        iteS = pHMDP->state_begin();
    }
    idx startSId = sId;

    aId = 0;
    for(; iteS!=pHMDP->state_end(); ++iteS, ++sId) {
        WriteBinary(pStateIdxFile, string2vec<int>(pHMDP->GetStateStr(sId)) );
        WriteBinary(pStateIdxFile, (int)-1);
        if (iteS->label.length()>0) {
            WriteBinary(pStateIdxLblFile, ToString<int>(sId));
            WriteBinary(pStateIdxLblFile, iteS->label);
        }
        for (HMDP::action_iterator iteA = pHMDP->action_begin(iteS); iteA!=pHMDP->action_end(iteS); ++iteA, ++aId) {
            WriteBinary(pActionIdxFile, sId);
            for (HMDP::trans_iterator iteT = pHMDP->trans_begin(iteA); iteT!=pHMDP->trans_end(iteA); ++iteT) {
                if (iteT->id>=startSId) {
                    WriteBinary(pActionIdxFile, (int)3);     // use scope 3
                    WriteBinary(pActionIdxFile, (int)iteT->id-(int)startSId);
                }
                else {
                    vector<idx> iHMDP = string2vec<idx>(pHMDP->GetStateStr(iteT->id));
                    int level=pHMDP->GetLevel(iHMDP);
                    if (level==0) WriteBinary(pActionIdxFile, (int)1);
                    if (level==1) WriteBinary(pActionIdxFile, (int)0);
                    WriteBinary(pActionIdxFile, (int)iteT->id);
                }
                WriteBinary(pTransProbFile, iteT->pr);
                WriteBinary(pTransWFile, iteT->w);
            }
            WriteBinary(pActionIdxFile, (int)-1);
            WriteBinary(pTransProbFile, (flt)-1);
            WriteBinary(pTransWFile, (flt)-1);
            if (iteA->label.length()>0) {
                WriteBinary(pActionIdxLblFile, ToString<int>(aId));
                WriteBinary(pActionIdxLblFile, iteA->label);
            }
            WriteBinary(pActionWFile, iteA->GetW());
        }
    }
    wLblLth=pHMDP->weightActionNames.size();
    for (idx i=0;i<pHMDP->weightActionNames.size();i++) WriteBinary(pActionWLblFile, pHMDP->weightActionNames[i]);
    for (idx i=0;i<pHMDP->weightTransNames.size();i++) WriteBinary(pTransWLblFile, pHMDP->weightTransNames[i]);
    map<string,string>::iterator it;
    for (it=pHMDP->external.begin(); it!=pHMDP->external.end(); ++it) {
        WriteBinary(pExternalProcessesFile,it->first);
        WriteBinary(pExternalProcessesFile,it->second);
    }
    timer.StopTimer();
    log << "Create binary files of HMDP in memory ...\n\n";
    log << "  Statistics:\n";
    log << "    states : " << sId << "\n";
    log << "    actions: " << aId << "\n";
    log << "    weights: " << wLblLth << "\n\n";
    log << "  Closing binary MDP writer.\n\n";
    log << "  Total time for writing to binary files: " << timer.ElapsedTime("sec") << " sec.\n\n";
}

// ----------------------------------------------------------------------------

HMDPSave::~HMDPSave() {
  fclose(pStateIdxFile);
  fclose(pStateIdxLblFile);
  fclose(pActionIdxFile);
  fclose(pActionIdxLblFile);
  fclose(pActionWFile);
  fclose(pActionWLblFile);
  fclose(pTransProbFile);
  fclose(pTransWFile);
  fclose(pTransWLblFile);
  fclose(pExternalProcessesFile);
}

// ----------------------------------------------------------------------------

vector<flt> HMDP::CalcSteadyStatePr() {
	log.str("");
	int rows = GetStateSize("0");
	vector<flt> v(rows,0);
	if (timeHorizon<INFINT) {
		log << "Stady state probabilities can only be done be calculated on infinite time-horizon HMDPs!" << endl;
		return v;
	}
	arma::vec b(rows),    // Vector left hand side
              w(rows);    // Vector of weights (the unknown)
	arma::mat P(rows, rows); // Matrix of prob values

	log << "Calculate steady state probabilities:";
	FounderPr(BellmanOp::TransPr,P);
	//P.Print();
    // Now solve equations wP = w and w1=1 -> w(P-I) = 0 and w1=1 where P have been
    // calculated for the founder. This is equvivalent to solving
    // Qw=b where Q=(P-I)' and b=(0,...,0,1)' where last col in
    // (P-I) is replaced with 1.
    P.diag() -= 1.0;
    P.col(rows-1).ones();
    b.zeros();
    b(rows-1) = 1;
    if (!arma::solve(w, P.t(), b)) log << " Error: can not solve system equations. Is the model fulfilling the model assumptions (e.g. unichain)? " << endl;
    v.assign(w.memptr(), w.memptr()+rows);
    //cout << "r=" << endl << r << endl << "P=" << endl << P << endl << "w=" << endl << w << endl;
	log << " finished." << endl;
    return v;
}
