#include "hmdp.hh"

// ----------------------------------------------------------------------------

string HMDPState::StageStr() {
	string str;
	idx size = idxHMDP.size();
	for(idx i=0; i<size-1; i++) {
		if (i<size-2) str.append(ToString(idxHMDP[i])+",");
		else str.append(ToString(idxHMDP[i]));
	}
	return str;
}

// ----------------------------------------------------------------------------

string HMDPState::StateStr() {
	string str;
	idx size = idxHMDP.size();
	for(idx i=0; i<size; i++) {
		if (i<size-1) str.append(ToString(idxHMDP[i])+",");
		else str.append(ToString(idxHMDP[i]));
	}
	return str;
}

// ----------------------------------------------------------------------------

string HMDPState::NextStageStr() {
	string str;
	idx size = idxHMDP.size();
	for(idx i=0; i<size-1; i++) {
		if (i<size-2) str.append(ToString(idxHMDP[i])+",");
		else str.append(ToString(idxHMDP[i]+1));    // increase by one
	}
	return str;
}

// ----------------------------------------------------------------------------

string HMDPState::NextChildStageStr(idx iAction) {
	string str = StateStr();
	str.append(","+ToString(iAction)+",0");
	return str;
}

// ----------------------------------------------------------------------------

string HMDPState::NextFatherStageStr() {
	string str;
	idx size = idxHMDP.size();
	if (size==2) return "NA";
	for(idx i=0; i<size-4; i++) {
		if (i<size-5) str.append(ToString(idxHMDP[i])+",");
		else str.append(ToString(idxHMDP[i]+1));
	}
	return str;
}

// ----------------------------------------------------------------------------

uSInt HMDPAction::Check(flt eps, ostringstream & log) {
	flt sum = 0;
	uSInt msg;
	if (idxStates.size() != transPr.size() & scope.size() != transPr.size()) {
		log << "Error: In action " << label << " sizes of scope, index and probability do not have the same size!" << endl;
		return 2;
	}
	for (idx i=0;i<idxStates.size();i++) {
		if (idxStates[i]<0 | transPr[i]<0 | scope[i]<0) {
			log << "Error: In action " << label << " scope, index or probability is negative!" << endl;
			log << "  (scp,idx,pr) = (" << scope[i] << "," << idxStates[i] << "," << transPr[i] << ")" << endl;
			return 2;
		}
	}
	msg = 0;
	for (idx j=0; j<transPr.size(); j++) sum += transPr[j];
	if (abs(sum-1)>eps) {
		log << "Warning: In action " << label << " probabilities do not sum to one! ";
		log << "Sum equals " << sum << endl;
		msg = 1;
	}
	return msg;
}

// ----------------------------------------------------------------------------

void HMDPAction::Print() {
	cout << "  action: " << label << " ";
	for (idx i=0; i<scope.size(); i++) {
		cout << "(i,p,s)=(" << idxStates[i] << "," << transPr[i] << "," << scope[i] << ") ";
	}
	cout << "w=(";
	for (idx i=0; i<weights.size()-1; i++) cout << weights[i] << ",";
	cout << weights[weights.size()-1] << ")";
	cout << endl;
}

// ----------------------------------------------------------------------------

/*HMDP::HMDP(uInt levels, uInt timeHorizon, flt rate, flt rateBase){
	this->levels = levels;
	this->timeHorizon = timeHorizon;
	this->rate = rate;
	this->rateBase = rateBase;
}*/

// ----------------------------------------------------------------------------

HMDP::HMDP(uInt levels, uInt timeHorizon){
	this->levels = levels;
	this->timeHorizon = timeHorizon;
}

// ----------------------------------------------------------------------------

void HMDP::AddState(const vector<idx> &iHMDP, const string &label) {
	HMDPState state(iHMDP, label);
	//cout<< states.size()-1 << endl;
	states.push_back(state);
	//cout << &states[states.size()-1] << endl;
	string stage = state.StageStr();
	//cout << stage << endl;
	stages.insert(pair< string, int >(stage,states.size()-1));
}

// ----------------------------------------------------------------------------

void HMDP::AddState(const vector<idx> &iHMDP) {
	HMDPState state(iHMDP);
	//cout<< states.size()-1 << endl;
	states.push_back(state);
	//cout << &states[states.size()-1] << endl;
	string stage = state.StageStr();
	//cout << stage << endl;
	stages.insert(pair< string, int >(stage,states.size()-1));
}

// ----------------------------------------------------------------------------

string HMDP::StateActionsToHgf(idx iState, bool & findValidOdr) {
	string sUp, sDown, sNext;
	ostringstream hgf;
	hgf.setf(ios::fixed);
	idx iS = 0;
	pair< multimap<string, int >::iterator, multimap<string, int >::iterator > pairUp;
	pair< multimap<string, int >::iterator, multimap<string, int >::iterator > pairDown;
	pair< multimap<string, int >::iterator, multimap<string, int >::iterator > pairNext;
	multimap<string, int >::iterator ite;

	sUp = states[iState].NextFatherStageStr();
	pairUp = stages.equal_range(sUp);
	sNext = states[iState].NextStageStr();
	pairNext = stages.equal_range(sNext);
	//cout << endl << sUp << " - " <<sNext << endl;
	/*for (ite = pairUp.first; ite != pairUp.second; ++ite)
	{
	   cout << "  [" << (*ite).first << ", " << (*ite).second << "]" << endl;
	}*/

	for (idx a=0; a<states[iState].tmpActions.size(); a++) {
		hgf << iState+1 << " "; // add one since hgf start from node 1
		sDown = states[iState].NextChildStageStr(a);
		pairDown = stages.equal_range(sDown);
		for (idx j=0; j<states[iState].tmpActions[a].transPr.size(); j++) {
			if (states[iState].tmpActions[a].scope[j]==1) { // next stage
				ite = pairNext.first;
				for (idx i=0; i<states[iState].tmpActions[a].idxStates[j]; i++) ++ite;
				iS = ite->second;
			}
			if (states[iState].tmpActions[a].scope[j]==0) { // next father stage
				ite = pairUp.first;
				for (idx i=0; i<states[iState].tmpActions[a].idxStates[j]; i++) ++ite;
				iS = ite->second;
				//cout << "iS: "<< iS << endl;
			}
			if (states[iState].tmpActions[a].scope[j]==2) { // next child stage
				ite = pairDown.first;
				for (idx i=0; i<states[iState].tmpActions[a].idxStates[j]; i++) ++ite;
				iS = ite->second;
			}
			if (states[iState].tmpActions[a].scope[j]==3) { // specify state index/id
				iS = states[iState].tmpActions[a].idxStates[j];
				findValidOdr = true;    // possible that have to create new valid ordering.
			}
			hgf << "-" << iS+1 << " ";
		}
		hgf << 0 << " ";
		for (idx w=0; w<states[iState].tmpActions[a].weights.size(); w++) {
			hgf << states[iState].tmpActions[a].weights[w] << " ";
		}
		for (idx j=0; j<states[iState].tmpActions[a].transPr.size(); j++) {
			hgf << (int)(states[iState].tmpActions[a].transPr[j]*10000000) << " ";
		}
		hgf << endl;
	}
	//cout << hgf.str();
	return hgf.str();
}

// ----------------------------------------------------------------------------

bool HMDP::CheckIdx() {
	string sUp, sDown, sNext;
	idx iS = 0;
	uInt ctrUp, ctrNext, ctrDown, ctr = 0;
	pair< multimap<string, int >::iterator, multimap<string, int >::iterator > pairUp;
	pair< multimap<string, int >::iterator, multimap<string, int >::iterator > pairDown;
	pair< multimap<string, int >::iterator, multimap<string, int >::iterator > pairNext;
	multimap<string, int >::iterator ite;

	for (idx iState=0; iState<states.size(); ++iState) {
		sUp = states[iState].NextFatherStageStr();
		pairUp = stages.equal_range(sUp);
		ctrUp = stages.count(sUp);
		sNext = states[iState].NextStageStr();
		pairNext = stages.equal_range(sNext);
		ctrNext = stages.count(sNext);
		for (idx a=0; a<states[iState].tmpActions.size(); a++) {
			if (states[iState].tmpActions[a].weights.size() != weightNames.size()) {
				log << "Error: wrong number of weights in action number " <<
					a << ", state " << iState << " (sId).\n";
				return false;
			}
			sDown = states[iState].NextChildStageStr(a);
			pairDown = stages.equal_range(sDown);
			ctrDown = stages.count(sDown);
			for (idx j=0; j<states[iState].tmpActions[a].idxStates.size(); j++) {
				if (states[iState].tmpActions[a].scope[j]==1) ctr = ctrNext;
				if (states[iState].tmpActions[a].scope[j]==0) ctr = ctrUp;
				if (states[iState].tmpActions[a].scope[j]==2) ctr = ctrDown;
				if (states[iState].tmpActions[a].scope[j]==3) ctr = states.size();
				if (states[iState].tmpActions[a].idxStates[j]>=ctr) {
					log << "Error: specify transition to nonexisting state in state "
						<< iState << " (sId) action index " << a << ".\n";
					return false;
				}

				if (states[iState].tmpActions[a].scope[j]==1) { // next stage
					ctr = ctrNext;
					ite = pairNext.first;
					for (idx i=0; i<states[iState].tmpActions[a].idxStates[j]; i++) ++ite;
					iS = ite->second;
				}
				if (states[iState].tmpActions[a].scope[j]==0) { // next father stage
					ctr = ctrUp;
					ite = pairUp.first;
					for (idx i=0; i<states[iState].tmpActions[a].idxStates[j]; i++) ++ite;
					iS = ite->second;
				}
				if (states[iState].tmpActions[a].scope[j]==2) { // next child stage
					ctr = ctrDown;
					ite = pairDown.first;
					for (idx i=0; i<states[iState].tmpActions[a].idxStates[j]; i++) ++ite;
					iS = ite->second;
				}
				if (states[iState].tmpActions[a].scope[j]==3) { // specify state index/id
					iS = states[iState].tmpActions[a].idxStates[j];
				}
				if (iS<=iState & states[iState].tmpActions[a].scope[j]!=3) {
					log << "Error: action " << a << " in state " << iState
						<< " (sId) seems to be looping.\n";
					states[iState].Print();
					return false;
				}
				if (iS>states.size() | iS<1) {
					log << "Error: specify transition to nonexisting state in state "
						<< iState << " (sId) action number " << a << ".\n";
					states[iState].Print();
					return false;
				}
			}
		}
	}
	return true;
}

// ----------------------------------------------------------------------------

void HMDP::Print() {
	cout << "HMDP - levels: " << levels << ", time-horizon: ";
	if (timeHorizon>=INFINT) cout << "infinite";
	else cout << timeHorizon << " (finite)";
	cout << endl << "Weights: ";
	for (idx i=0; i<weightNames.size(); i++) cout << weightNames[i] << ", ";
	cout << endl;
	for (idx i=0; i<states.size(); i++) states[i].Print();
}

// ----------------------------------------------------------------------------

/** Print the number of states at next level of the father, current and child. */
void HMDP::PrintCount() {
	uInt sUp,sNext,sDown;  // number of states at next stage of father, current and child.
	cout << "Count the number of states at next stage of the father, current and child process:" << endl;
	for (idx i=0; i<states.size(); i++) {
		sUp=CountFather(i);
		sNext = CountNext(i);
		cout << "state: " << i << " " << states[i].StateStr() << " up:" << sUp << " next:" << sNext << endl;
		for (idx a=0; a<states[i].tmpActions.size(); a++) {
			sDown = CountChild(i,a);
			if (sDown>0) {
				cout << "  a: " << a << " (" << states[i].tmpActions[a].label << ") define a child process with " << sDown << " state(s) at stage 0." << endl;
			}
		}
	}
}

// ----------------------------------------------------------------------------

void HMDP::CalcHgfSizes(uInt &n, uInt &ma, uInt &mh, uInt &d, uInt &hsize,
	uInt &sizeW, uInt &sizeWTmp, uInt &sizePred, uInt &sizeMult) {
	n = states.size();
	ma = 0;
	for (idx i=0; i<states.size(); i++) ma += states[i].CountArcActions();  // Count the number of deterministic actions (actions corresponding to an arc.
	mh = 0;
	for (idx i=0; i<states.size(); i++) mh += states[i].CountHArcActions(); // Count the number of non-deterministic actions (actions corresponding to a hyperarc.
	d = 0;
	for (idx i=0; i<states.size(); i++)     // Find the max size of the transition probabilities used.
		d = max(d,states[i].MaxTransPrSize());
	hsize = 0;
	for (idx i=0; i<states.size(); i++) hsize += states[i].CountActionSize(); // Count the size of true hyperarcs.
	sizeW = sizeWTmp = weightNames.size();
	sizePred = 1;
	sizeMult = 1;
	//cout << "var: " << n << " " << ma << " " << mh << " " << d << " " << hsize << " " << sizeW << " " << sizeWTmp << " " << sizePred << " " << sizeMult << endl << endl;;
}

// ----------------------------------------------------------------------------

void HMDP::BuildHMDP() {
	log.str("");
	uInt n, ma, mh, d, hsize, sizeW, sizeWTmp, sizePred, sizeMult;
	string str;
	NodePtr pNode;
	bool findValidOdr = false;

    cpuTime.Reset(0);
	cpuTime.StartTime(0);
	CalcHgfSizes(n, ma, mh, d, hsize, sizeW, sizeWTmp, sizePred, sizeMult);
	H.Initialize(n, ma, mh, d, hsize, sizeW, sizeWTmp, sizePred, sizeMult);     // Allocate mem for the hgf
	for (idx i=0; i<states.size(); i++) {   // add hyperarcs to tmp hgf memory
		str = StateActionsToHgf(i,findValidOdr);
		//log << str << endl;
		H.AddHyperarcs(str);
	}
	cout << "Cpu time loading MDP into tmp hgf arrays" << cpuTime.TimeDiff(0) << endl;
	H.BuildHgf();   // create the hypergraph
	cout << "Cpu time after building hgf " << cpuTime.TimeDiff(0) << endl;

	// TODO LRE Virker dette ikke kun hvis alle states har en label!!
	for (idx i=0; i<states.size(); i++) {   // set pointers to labels
		pNode =  H.itsNodes + i+1;
		int arcNumb = -1, hArcNumb = -1;
		for (idx j=0; j<states[i].tmpActions.size(); ++j) {  // add pointers to action labels
			if (states[i].tmpActions[j].transPr.size()==1) {    // an arc
				arcNumb++;
				((pNode->pAFirst)+arcNumb)->pLabel = &states[i].actionLabels[j];
			}
			else {  // a hyperarc
				hArcNumb++;
				((pNode->pHFirst)+hArcNumb)->pLabel = &states[i].actionLabels[j];
			}
		}
	}
	cout << "Cpu time after setting label pointers " << cpuTime.TimeDiff(0) << endl;

	idxPred = idxMult = 0;  // consider first pred and multipliers
	if (!findValidOdr) HT.SetValidOdrToReverseNodeOdr(H);
	else {  // find stage 2 states of founder
		vector<idx> nodes;
		pair< multimap<string, int >::iterator, multimap<string, int >::iterator > pair
			= stages.equal_range(ToString(timeHorizon-1));
		for (multimap<string, int >::iterator ite = pair.first;
			ite != pair.second; ++ite) {
			nodes.push_back( ((*ite).second) + 1);   // add hgf node
			//log << "  [" << (*ite).first << ", " << (*ite).second << "]" << endl;
		}
		HT.FindValidOdr(H,nodes);
	}
	cout << "Cpu time after finding valid odr " << cpuTime.TimeDiff(0) << endl;

	cout<<"Before remove actions";
	cin >> str;

	for (idx i=0; i<states.size(); i++) {   // remove tmpActions
		states[i].RemoveActions();
	}


	cpuTime.StopTime(0);
	log << "Total cpu time for building state-expanded hypergraph " << cpuTime.TimeDiff(0) << "s" << endl;
}

// ----------------------------------------------------------------------------

vector<idx> HMDP::WeightIdx(idx idxW, idx idxDur) {
	vector<idx> v;
	for (idx i=0; i<weightNames.size(); ++i) {
		if (i!=idxW & i!=idxDur) v.push_back(i);
	}
	return v;
}

// ----------------------------------------------------------------------------

flt HMDP::MaxDiffFounder(const idx &idxW,
	const pair< multimap<string, int >::iterator, multimap<string, int >::iterator > &pairZero,
	const pair< multimap<string, int >::iterator, multimap<string, int >::iterator > &pairLast)
{
	flt m = -INF;
	multimap<string, int >::iterator ite, iteZ;

	for (ite=pairLast.first, iteZ=pairZero.first; ite!=pairLast.second; ++ite, ++iteZ)
		m = max(m,abs(H.itsNodes[(iteZ->second)+1].w[idxW] - H.itsNodes[(ite->second)+1].w[idxW]));
	//cout << m << endl;
	return m;
}

// ----------------------------------------------------------------------------

void HMDP::ValueIteInfDiscount(uInt times, flt epsilon, idx idxW, idx idxDur,
	const flt &rate, const flt &rateBase)
{
    idx i;
	log.str("");
	pair< multimap<string, int >::iterator, multimap<string, int >::iterator > pairZero;
	pair< multimap<string, int >::iterator, multimap<string, int >::iterator > pairLast;
	multimap<string, int >::iterator ite, iteZ;

	log << "Run value iteration with epsilon = " << epsilon  << " at most "
		<< times << " time(s)" << endl << "using quantity '" <<
		weightNames[idxW] << "' under expected discounted reward criterion \nwith '" <<
		weightNames[idxDur] << "' as duration using interest rate " << rate <<
		" and rate basis equal " << rateBase << ". \nIterations:";
	cpuTime.Reset(0); cpuTime.StartTime(0);
	H.ResetPred();
	// find founder states at stage zero and last stage
	pairZero = stages.equal_range("0");
	pairLast = stages.equal_range("1");
	for (ite=pairLast.first; ite!=pairLast.second; ++ite) // set last to zero
		H.itsNodes[(ite->second)+1].SetW(idxW,0);

	for (i=0; i<times; ++i) {
		HT.CalcHTacyclic(H,idxW,idxPred,idxMult,idxDur,rate,rateBase);
		if(MaxDiffFounder(idxW,pairZero,pairLast)<epsilon) break;
		if (i<times-1) {    // set next stage to stage zero values
			for (ite=pairLast.first, iteZ=pairZero.first; ite!=pairLast.second; ++ite, ++iteZ)
				H.itsNodes[(ite->second)+1].SetW(idxW,H.itsNodes[(iteZ->second)+1].w[idxW]);
		}
	}
	log << " " << i+1;
	//vector<idx> vW = WeightIdx(idxW, idxDur);
	// TODO LRE: May have idxPred as argument or use the same idx as idxW.
	//HT.CalcOptW(H,vW,idxPred,idxMult);
	cpuTime.StopTime(0);
	log << ". Running time " << cpuTime.TimeDiff(0) << "s." << endl;
}

// ----------------------------------------------------------------------------

void HMDP::ValueIteInfDiscount(uInt times, flt epsilon, idx idxW, idx idxDur,
	const flt &rate, const flt &rateBase, vector<flt> & iniValues)
{
    idx i;
	log.str("");
	pair< multimap<string, int >::iterator, multimap<string, int >::iterator > pairZero;
	pair< multimap<string, int >::iterator, multimap<string, int >::iterator > pairLast;
	multimap<string, int >::iterator ite, iteZ;
	vector<flt>::iterator iteV;

	log << "Run value iteration with epsilon = " << epsilon  << " at most "
		<< times << " time(s)" << endl << "using quantity '" <<
		weightNames[idxW] << "' under expected discounted reward criterion \nwith '" <<
		weightNames[idxDur] << "' as duration using interest rate " << rate <<
		" and rate basis equal " << rateBase << ". \nIterations:";
	cpuTime.Reset(0); cpuTime.StartTime(0);
	H.ResetPred();
	// find founder states at stage zero and last stage
	pairZero = stages.equal_range("0");
	pairLast = stages.equal_range("1");
	if (iniValues.size()!=stages.count("1"))
        log << "Error initial values vector does not have the same size as the states that must be assigned the values!\n";
	for (ite=pairLast.first, iteV=iniValues.begin(); ite!=pairLast.second; ++ite, ++iteV) // set last to zero
		H.itsNodes[(ite->second)+1].SetW(idxW,*(iteV));

	for (i=0; i<times; ++i) {
		HT.CalcHTacyclic(H,idxW,idxPred,idxMult,idxDur,rate,rateBase);
		if(MaxDiffFounder(idxW,pairZero,pairLast)<epsilon) break;
		if (i<times-1) {    // set next stage to stage zero values
			for (ite=pairLast.first, iteZ=pairZero.first; ite!=pairLast.second; ++ite, ++iteZ)
				H.itsNodes[(ite->second)+1].SetW(idxW,H.itsNodes[(iteZ->second)+1].w[idxW]);
		}
	}
	log << " " << i+1;
	//vector<idx> vW = WeightIdx(idxW, idxDur);
	// TODO LRE: May have idxPred as argument or use the same idx as idxW.
	//HT.CalcOptW(H,vW,idxPred,idxMult);
	cpuTime.StopTime(0);
	log << ". Running time " << cpuTime.TimeDiff(0) << "s." << endl;
}

// ----------------------------------------------------------------------------

void HMDP::ValueIteFiniteDiscount(idx idxW, idx idxDur, const flt &rate,
	const flt &rateBase)
{
	pair< multimap<string, int >::iterator, multimap<string, int >::iterator > pairZero;
	pair< multimap<string, int >::iterator, multimap<string, int >::iterator > pairLast;
	multimap<string, int >::iterator ite, iteZ;

	log.str("");
	log << "Run value iteration using quantity '" <<
		weightNames[idxW] << "' under expected discounted reward criterion \nwith '" <<
		weightNames[idxDur] << "' as duration. using interest rate " << rate <<
		" and rate basis equal " << rateBase << ". ";
	cpuTime.Reset(0); cpuTime.StartTime(0);
	H.ResetPred();
	// find founder states at stage zero and last stage
	pairZero = stages.equal_range("0");
	pairLast = stages.equal_range(ToString(timeHorizon-1));
	for (ite=pairLast.first; ite!=pairLast.second; ++ite) // set last to zero
		H.itsNodes[(ite->second)+1].SetW(0);
	HT.CalcHTacyclic(H,idxW,idxPred,idxMult,idxDur,rate,rateBase);
	//vector<idx> vW = WeightIdx(idxW, idxDur);
	//HT.CalcOptW(H,vW,idxPred,idxMult);
	cpuTime.StopTime(0);
	log << "Finished (" << cpuTime.TimeDiff(0) << "s)." << endl;
}

// ----------------------------------------------------------------------------

void HMDP::ValueIteFiniteDiscount(idx idxW, idx idxDur, const flt &rate,
	const flt &rateBase, vector<flt> & termValues)
{
	pair< multimap<string, int >::iterator, multimap<string, int >::iterator > pairZero;
	pair< multimap<string, int >::iterator, multimap<string, int >::iterator > pairLast;
	multimap<string, int >::iterator ite, iteZ;
	vector<flt>::iterator iteV;

	log.str("");
	log << "Run value iteration using quantity '" <<
		weightNames[idxW] << "' under expected discounted reward criterion \nwith '" <<
		weightNames[idxDur] << "' as duration. using interest rate " << rate <<
		" and rate basis equal " << rateBase << ". ";
	cpuTime.Reset(0); cpuTime.StartTime(0);
	H.ResetPred();
	// find founder states at stage zero and last stage
	pairZero = stages.equal_range("0");
	pairLast = stages.equal_range(ToString(timeHorizon-1));
	if (termValues.size()!=stages.count(ToString(timeHorizon-1)))
        log << "Error initial values vector does not have the same size as the states that must be assigned the values!\n";
	for (ite=pairLast.first, iteV=termValues.begin(); ite!=pairLast.second; ++ite, ++iteV) // set last to zero
		H.itsNodes[(ite->second)+1].SetW(*(iteV));
	HT.CalcHTacyclic(H,idxW,idxPred,idxMult,idxDur,rate,rateBase);
	//vector<idx> vW = WeightIdx(idxW, idxDur);
	//HT.CalcOptW(H,vW,idxPred,idxMult);
	cpuTime.StopTime(0);
	log << "Finished (" << cpuTime.TimeDiff(0) << "s)." << endl;
}

// ----------------------------------------------------------------------------

void HMDP::ValueIteFinite(idx idxW, vector<flt> & termValues)
{
	pair< multimap<string, int >::iterator, multimap<string, int >::iterator > pairZero;
	pair< multimap<string, int >::iterator, multimap<string, int >::iterator > pairLast;
	multimap<string, int >::iterator ite, iteZ;
	vector<flt>::iterator iteV;

	log.str("");
	log << "Run value iteration using quantity '" <<
		weightNames[idxW] << "' under expected reward criterion. ";
	cpuTime.Reset(0); cpuTime.StartTime(0);
	H.ResetPred();
	// find founder states at stage zero and last stage
	pairZero = stages.equal_range("0");
	pairLast = stages.equal_range(ToString(timeHorizon-1));
	if (termValues.size()!=stages.count(ToString(timeHorizon-1)))
        log << "Error initial values vector does not have the same size as the states that must be assigned the values!\n";
	for (ite=pairLast.first, iteV=termValues.begin(); ite!=pairLast.second; ++ite, ++iteV) // set last to zero
		H.itsNodes[(ite->second)+1].SetW(*(iteV));
	HT.CalcHTacyclic(H,idxW,idxPred,idxMult);
	//vector<idx> vW = WeightIdx(idxW, weightNames.size()+1); // hack so idxDur is just greater than the index
	//HT.CalcOptW(H,vW,idxPred,idxMult);
	cpuTime.StopTime(0);
	log << "Finished (" << cpuTime.TimeDiff(0) << "s)." << endl;
}

// ----------------------------------------------------------------------------

void HMDP::SetR(MatSimple<double> &r, const idx &idxW,
	const pair< multimap<string, int >::iterator, multimap<string, int >::iterator > &pairZero)
{
	idx i;
	multimap<string, int >::iterator ite;
	for (ite=pairZero.first, i=0; ite!=pairZero.second; ++ite, ++i) { // save values to r
		r(i,0) = H.itsNodes[(ite->second)+1].w[idxW];
		//cout << "i:" << i << " " << H.itsNodes[(ite->second)+1].w[idxW] << endl;
	}
}

// ----------------------------------------------------------------------------

void HMDP::FounderRewardDiscount(MatSimple<double> &r, const idx &idxW, const idx &idxDur,
	const flt &rate, const flt &rateBase,
	const pair< multimap<string, int >::iterator, multimap<string, int >::iterator > &pairZero,
	const pair< multimap<string, int >::iterator, multimap<string, int >::iterator > &pairLast)
{
	multimap<string, int >::iterator ite;

	for (ite=pairLast.first; ite!=pairLast.second; ++ite) // set last to zero
		H.itsNodes[(ite->second)+1].SetW(idxW,0);
	HT.CalcOptWDiscount(H,idxW,idxPred,idxMult,idxDur,rate,rateBase);
	SetR(r,idxW,pairZero);
}

// ----------------------------------------------------------------------------

void HMDP::FounderW(MatSimple<double> &w, const idx &idxW,
	const pair< multimap<string, int >::iterator, multimap<string, int >::iterator > &pairZero,
	const pair< multimap<string, int >::iterator, multimap<string, int >::iterator > &pairOne)
{
	multimap<string, int >::iterator ite;
	for (ite=pairOne.first; ite!=pairOne.second; ++ite) // set last to zero
		H.itsNodes[(ite->second)+1].SetW(idxW,0);
	HT.CalcOptW(H,idxW,idxPred,idxMult);
	SetR(w,idxW,pairZero);
}

// ----------------------------------------------------------------------------

void HMDP::FounderPrDiscount(MatSimple<double> &P, const idx &idxW, const idx &idxDur,
	const flt &rate, const flt &rateBase,
	const pair< multimap<string, int >::iterator, multimap<string, int >::iterator > &pairZero,
	const pair< multimap<string, int >::iterator, multimap<string, int >::iterator > &pairLast)
{
	multimap<string, int >::iterator ite, itePrev, iteZ;
	idx r,c;

	for (ite=pairLast.first; ite!=pairLast.second; ++ite) // set last to zero
		H.itsNodes[(ite->second)+1].f = 0;

	for (ite=itePrev=pairLast.first, c=0; ite!=pairLast.second; ++ite, ++c) {
		H.itsNodes[(ite->second)+1].f = 1;
		if (c!=0) H.itsNodes[(itePrev->second)+1].f = 0;    // restore previous
		HT.CalcSubTreeValues(H,idxPred,idxMult,idxDur,rate,rateBase);
		for (iteZ=pairZero.first, r=0; iteZ!=pairZero.second; ++iteZ, ++r) { // save values to r
			P(r,c) = H.itsNodes[(iteZ->second)+1].f;
			//cout << "r,c:" << r << "," << c << " " << H.itsNodes[(ite->second)+1].w[idxW] << endl;
		}
		itePrev = ite;
	}
}

// ----------------------------------------------------------------------------

void HMDP::FounderPr(MatSimple<double> &P, const idx &idxW,
	const pair< multimap<string, int >::iterator, multimap<string, int >::iterator > &pairZero,
	const pair< multimap<string, int >::iterator, multimap<string, int >::iterator > &pairLast)
{
	multimap<string, int >::iterator ite, itePrev, iteZ;
	idx r,c;

	for (ite=pairLast.first; ite!=pairLast.second; ++ite) // set last to zero
		H.itsNodes[(ite->second)+1].f = 0;

	for (ite=itePrev=pairLast.first, c=0; ite!=pairLast.second; ++ite, ++c) {
		H.itsNodes[(ite->second)+1].f = 1;
		if (c!=0) H.itsNodes[(itePrev->second)+1].f = 0;    // restore previous
		HT.CalcSubTreeValues(H,idxPred,idxMult);
		for (iteZ=pairZero.first, r=0; iteZ!=pairZero.second; ++iteZ, ++r) { // save values to r
			P(r,c) = H.itsNodes[(iteZ->second)+1].f;
			//cout << "r,c:" << r << "," << c << " " << H.itsNodes[(ite->second)+1].w[idxW] << endl;
		}
		itePrev = ite;
	}
}

// ----------------------------------------------------------------------------

void HMDP::PolicyIteDiscount(const idx idxW, const idx idxDur, const flt &rate,
		const flt &rateBase)
{
	MatAlg matAlg;  // Matrix routines

	log.str("");
	if (timeHorizon<INFINT) {
		log << "Policy iteration can only be done on infinite time-horizon HMDPs!" << endl;
		return;
	}
	pair< multimap<string, int >::iterator, multimap<string, int >::iterator > pairZero;
	pair< multimap<string, int >::iterator, multimap<string, int >::iterator > pairLast;
	multimap<string, int >::iterator ite, iteZ;
	H.ResetPred();
	bool newPred, firstRun;
	int rows = stages.count("0");
	MatSimple<double> r(rows,1),   // Matrix of founder rewards of action
				   w(rows,1),   // Matrix of weights (the unknown)
				   P(rows,rows);    // Matrix of prob values
	MatSimple<double> I(rows,true); // identity
	idx i,k = 0;

	log << "Run policy iteration using quantity '" <<
		weightNames[idxW] << "' under discounting criterion \nwith '" <<
		weightNames[idxDur] << "' as duration using interest rate " << rate <<
		" and a rate basis equal " << rateBase << ". \nIteration(s):";

	pairZero = stages.equal_range("0");
	pairLast = stages.equal_range("1");
	for (ite=pairLast.first; ite!=pairLast.second; ++ite) // set last to zero
		H.itsNodes[(ite->second)+1].SetW(0);

	firstRun = true;
	do {
		k++;
		newPred = HT.CalcHTacyclic(H,idxW,idxPred,idxMult,idxDur,rate,rateBase);
		log << " " << k;
		if (!firstRun) {
			if (!newPred) break;    // optimal strategy found
			FounderRewardDiscount(r, idxW, idxDur, rate, rateBase, pairZero, pairLast);
		}
		else {
			firstRun = false;
			SetR(r,idxW,pairZero);
		}
		FounderPrDiscount(P,idxW,idxDur,rate,rateBase,pairZero,pairLast);
		// Now solve equations w = r + Pw -> (I-P)w = r
		matAlg.IMinusP(P);
		matAlg.LASolve(P,w,r);
		//cout << "r=" << endl << r << endl << "P=" << endl << P << endl << "w=" << endl << w << endl;
		for (ite=pairLast.first, i=0; ite!=pairLast.second; ++ite, ++i) // set last to w values
			H.itsNodes[(ite->second)+1].SetW(idxW,w(i,0));
		//if (k==10) break;
	} while (true);
	log << " finished." << endl;
}

// ----------------------------------------------------------------------------

flt HMDP::PolicyIteAve(const idx idxW, const idx idxD) {
	log.str("");
	if (timeHorizon<INFINT) {
		log << "Policy iteration can only be done on infinite time-horizon HMDPs!" << endl;
		return INF;
	}

	MatAlg matAlg; // Matrix routines
	pair< multimap<string, int >::iterator, multimap<string, int >::iterator > pairZero;
	pair< multimap<string, int >::iterator, multimap<string, int >::iterator > pairLast;
	multimap<string, int >::iterator ite, iteZ;
	H.ResetPred();
	bool newPred, firstRun;
	int rows = stages.count("0");
	MatSimple<double> r(rows,1),   // Matrix of founder rewards
				   w(rows,1),   // Matrix of weights (the unknown)
				   d(rows,1),    // Matrix of denominator values
				   P(rows,rows);    // Matrix of prob values
	MatSimple<double> I(rows,true); // identity
	idx i,k=0;
	flt g = 0;

	log << "Run policy iteration under average reward criterion using \nreward '" <<
		weightNames[idxW] << "' over '" <<
		weightNames[idxD] << "'. Iterations (g):" << endl;

	pairZero = stages.equal_range("0");
	pairLast = stages.equal_range("1");
	for (ite=pairLast.first; ite!=pairLast.second; ++ite) // set last to zero
		H.itsNodes[(ite->second)+1].SetW(0);

	firstRun = true;
	do {
		k++;
		newPred = HT.CalcHTacyclicAve(H,idxW,idxD,idxPred,idxMult,g);
		if (!firstRun & !newPred) {
			log << k <<  " (" << g << ") ";
			break;    // optimal strategy found
		}
		if (!firstRun) {
			FounderW(r, idxW, pairZero, pairLast);
		}
		else {
			firstRun = false;
			SetR(r,idxW,pairZero);
		}

		FounderPr(P,idxW,pairZero,pairLast);
		FounderW(d,idxD,pairZero,pairLast);
		//cout << "r=" << endl << r << endl << "P=" << endl << P << endl << "d=" << endl << d << endl;
		// Now solve equations h = r - dg + Ph where r, d and P have been
		// calculated for the founder. This is equvivalent to solving
		// (I-P)h + dg = r -> (I-P,d)(h,g)' = r which is equvivalent to
		// solving Qw = r (equation (8.6.8) in Puterman) where last col in
		// (I-P) replaced with d.
		matAlg.IMinusP(P);  // Set P := I-P
		//cout << "I-P=" << endl << P << endl;
		for(idx j=0; j<(idx)rows; ++j) P(j,rows-1) = d(j,0);   // set implicit h_{rows-1}=0 and calc g here.
		//cout << "I-P with d=" << endl << P << endl;
		matAlg.LASolve(P,w,r);
		//cout << "w=" << endl << w << endl;
		g = w(rows-1,0);
		// set last to w values
		for (ite=pairLast.first, i=0; ite!=pairLast.second; ++ite, ++i) {
			if (i<(idx)rows-1) H.itsNodes[(ite->second)+1].SetW(idxW,w(i,0));   // last state always has value zero
		}
		log << k <<  " (" << g << ") ";
		//if (!firstRun & !newPred) break;    // optimal strategy found
	} while (true);
	log << "finished." << endl;
	return g;
}

// ----------------------------------------------------------------------------

string HMDP::PolicyInfoIdx(idx idxW) {
	ostringstream oS;
	int a;

	oS.setf(ios::fixed);
	oS.precision(3);
	oS << "Optimal policy:" << endl;
	for (idx i=0; i<states.size(); ++i) {
		oS << i+1 << ": (" << states[i].StateStr() << ",";
		a = H.itsNodes[i+1].pred[idxPred];
		if (a<0) oS << H.itsArcs[-a].pLabel - &states[i].actionLabels[0];
		if (a>0) oS << H.itsHArcs[a].pLabel - &states[i].actionLabels[0];
		oS << ") (" << states[i].label << ") w:" << H.itsNodes[i+1].w[idxW] << " ";
		if (a<0) oS << *H.itsArcs[-a].pLabel;
		if (a>0) oS << *H.itsHArcs[a].pLabel;
		oS << endl;
		//if (a<0) H.PrintArc(-a);
		//if (a>0) H.PrintHArc(a);
	}
	oS << endl;
	return oS.str();
}

// ----------------------------------------------------------------------------

string HMDP::PolicyInfoLabel(idx idxW) {
	ostringstream oS;
	int a;

	oS << "Optimal policy:" << endl;
	for (idx i=0; i<states.size(); ++i) {
		oS << i+1 << ": (" << states[i].label << ",";
		a = H.itsNodes[i+1].pred[idxPred];
		if (a<0) oS << states[i].actionLabels[H.itsArcs[-a].pLabel - &states[i].actionLabels[0]];
		if (a>0) oS << states[i].actionLabels[H.itsHArcs[a].pLabel - &states[i].actionLabels[0]];
		oS << ") " << H.itsNodes[i+1].w[idxW] << endl;
	}
	oS << endl;
	return oS.str();
}

// ----------------------------------------------------------------------------

int HMDP::FindAction(idx iS, idx idxA)
{
	NodePtr pHnode = NULL;
	ArcPtr pArcNow,
	pLastArc;           // pointer to the arc we have to examine to (not with)
	HArcPtr pHNow,
	pLastHArc;
	bool found = false; // found the (h)arc corresponding to idxA?
	int idxHArc = 0;

	pHnode = H.GetNodesPtr()+iS+1;
	for (pArcNow=pHnode->pAFirst, pLastArc=(pHnode+1)->pAFirst;
			pArcNow!=pLastArc; pArcNow++) {
		if (pArcNow->inSubHgf) {
			if (idxA == (idx)(pArcNow->pLabel-&states[iS].actionLabels[0])) {
				found = true;
				idxHArc = ArcIndexPred(pArcNow);
				break;
			}
		}
	}
	for (pHNow=pHnode->pHFirst, pLastHArc=(pHnode+1)->pHFirst;
			pHNow!=pLastHArc; pHNow++) {
		if (found) break;
		if (pHNow->inSubHgf) {
			if (idxA == (idx)(pHNow->pLabel - &states[iS].actionLabels[0])) {
				found = true;
				idxHArc = HArcIndex(pHNow);
				break;
			}
		}
	}
	return idxHArc;
}

// ----------------------------------------------------------------------------

void HMDP::SetTerminalValues(idx idxW, vector<flt> & termValues) {
    pair< multimap<string, int >::iterator, multimap<string, int >::iterator > pairLast;
    multimap<string, int >::iterator ite;
    vector<flt>::iterator iteV;
    bool error=false;

    if (timeHorizon<INFINT) {
        pairLast = stages.equal_range(ToString(timeHorizon-1));
        if (termValues.size()!=stages.count(ToString(timeHorizon-1))) error=true;
    }
    else {
        pairLast = stages.equal_range("1");
        if (termValues.size()!=stages.count("1")) error=true;
    }
    if (error) {
        log << "Error initial values vector does not have the same size as the states that must be assigned the values!\n";
        return;
    }
    for (ite=pairLast.first, iteV=termValues.begin(); ite!=pairLast.second; ++ite, ++iteV) // set last to zero
        H.itsNodes[(ite->second)+1].SetW(idxW,*(iteV));
}

// ----------------------------------------------------------------------------
