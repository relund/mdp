//#include "hmdpReader.hpp"
#include "hmdp.hh"

// -----------------------------------------------------------------------------

template <class T>
idx HMDPReader::ReadBinary(string fileName, T *&p) {
	ifstream::pos_type fileSize;
	ifstream file;

	// read idx
	file.open(fileName.c_str() ,ios::in|ios::binary|ios::ate);    // open binary file for reading with pointer at end of file to get filesize
	if(!file) {
		cerr << "Problems opening " << fileName << "\nExiting..." << endl;
		exit(1);
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
	int * sIdx;    // raw idx data
	char * lbl;    // raw labels
	uInt numb = 0;     // max number of state idx
	//cout << "ss:" << sIdx << endl;

	idx sIdxSize = ReadBinary<int>(stateIdxFile,sIdx);
	//cout << "sss:" << sIdx << endl;
	// now scan sIdx and generate index vectors for each state
	vector<idx> s;  // vector of index
	idx prev=0;
	for(idx i=0; i<sIdxSize; i++) {
		if (sIdx[i]== -1) {
			s.assign(sIdx+prev, sIdx+i);
			numb=MAX(numb,s.size());
			prev=i+1;
			pHMDP->AddState(s);
		}
	}

	idx lblSize = ReadBinary<char>(stateIdxLblFile,lbl);
	// add labels to a string vector
	vector<string> labels;
	char * ptr = lbl;
	for (int i=0;;++i) {
		labels.push_back(ptr);
		ptr = strrchr(ptr,'\0');
		if (ptr==0 | ptr-lbl>=(int)lblSize) break;
		++ptr;
	}
	// now add labels to states
	idx sId;
	for(idx i=0;i<labels.size();++i) {
		if (i % 2 == 0) from_string<idx>(sId, labels[i], std::dec); // if i is even
		else pHMDP->states[sId].label = labels[i];
	}
	pHMDP->levels = numb/3 + 1;   // set number of levels
	// set time horizon
	for (uInt s=1;;++s) {
		if (pHMDP->stages.find(ToString(s)) == pHMDP->stages.end()) {  // if stage s not found
			if (s==1) pHMDP->timeHorizon = INFINT;
			else pHMDP->timeHorizon = s;    // since idx start from 0 the set of decision epochs is s
			break;
		}
	}

	delete [] sIdx;
	delete [] lbl;
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
	vector<idx> sIds;  // vector of sId's
	idx prev;
	vector<HMDPAction> actions; // vector to store the actions in
	vector<string> labels;
	char * ptr;

	idx aIdxSize = ReadBinary(actionIdxFile,aIdx);
	idx lblSize = ReadBinary(actionIdxLblFile,lbl);
	ReadBinary(actionWFile,aW);
	idx wLblSize = ReadBinary(actionWLblFile,wLbl);
	idx tPrSize = ReadBinary(transProbFile,tPr);
	// note that all arrays (except the label arrays) have the same number of rows (same number of -1's).

	// add wight labels to HMDP
	ptr = wLbl;
	for (int i=0;;++i) {
		labels.push_back(ptr);
		ptr = strrchr(ptr,'\0');
		if (ptr==0 | ptr-wLbl>=(int)wLblSize) break;
		++ptr;
	}
	labels.pop_back();  // the last element is a dummy
	for (idx i=0; i<labels.size(); ++i) pHMDP->AddWeight(labels[i]);
	wLblSize = labels.size();   // number of weights

	// scan aIdx
	vector<idx> a;  // vector of index
	prev=0;
	for(idx i=0; i<aIdxSize; i++) {
		if (aIdx[i]== -1) {
			a.assign(aIdx+prev, aIdx+i);    // a now contains the sId and (scp,idx) pairs
			sIds.push_back(a[0]);
			HMDPAction action;
			for (idx j=1;j<a.size();j++) {  // add scp and idx
				/*if (a[j]<0) {
					cout << "In action scope, index or probability is negative!" << endl;
					exit(1);
				}*/
				if (j%2==0) action.idxStates.push_back(a[j]);
				if (j%2==1) action.scope.push_back(a[j]);
			}
			prev=i+1;
			actions.push_back(action);
		}
	}   // now actions contain all the actions of the model
	// scan aW
	vector<double> b;  // vector of doubles
	idx aId;
	for(aId=0; aId<actions.size(); aId++) {
		b.assign(aW + aId*wLblSize, aW + (aId+1)*wLblSize);
		for (idx j=0;j<b.size();j++) {
			actions[aId].weights.push_back((flt)b[j]);
		}
	}
	// scan tPr
	prev=0;
	aId = 0;
	for(idx i=0; i<tPrSize; i++) {
		if (tPr[i]== -1) {
			b.assign(tPr+prev, tPr+i);
			for (idx j=0;j<b.size();j++) {
				actions[aId].transPr.push_back((flt)b[j]);
			}
			prev=i+1;
			aId++;
		}
	}
	// add labels to a string vector
	labels.clear();
	ptr = lbl;
	for (int i=0;;++i) {
		labels.push_back(ptr);
		ptr = strrchr(ptr,'\0');
		if (ptr==0 | ptr-lbl>=(int)lblSize) break;
		++ptr;
	}
	labels.pop_back();  // the last element is a dummy
	// add labels to actions
	for(idx i=0;i<labels.size();++i) {
		if (i % 2 == 0) from_string<idx>(aId, labels[i], std::dec); // if i is even
		else actions[aId].label = labels[i];
	}
	// save actions to HMPD
	for(idx i=0;i<actions.size();++i) {
		pHMDP->states[sIds[i]].tmpActions.push_back(actions[i]);
		pHMDP->states[sIds[i]].actionLabels.push_back(actions[i].label);
	}

	delete [] aIdx;
	delete [] lbl;
	delete [] aW;
	delete [] wLbl;
	delete [] tPr;
}

// -----------------------------------------------------------------------------

void HMDPReader::Compile() {
	vector<idx> s;  // vector of index
	s.push_back(1);
	if (pHMDP->timeHorizon>=INFINT) {   // add second stage at founder level
		uInt ctr = pHMDP->stages.count("0");  // states at founder level
		for (idx i=0;i<ctr;++i) {
			s.push_back(i);
			pHMDP->AddState(s);
			s.pop_back();
		}
	}
}

// -----------------------------------------------------------------------------


