#include "../src/hmdp.hh"
#include "../src/matrix.hh"
#include <string>
#include <fstream>
#include <vector>
#include "stdio.h"
using namespace std;



//----------------------------------------------------------------------------
// Forward declarations:
void RunPrintHgraph(char filename[]);
idx Ctr(vector<idx> iHMDP);
void ReadBin(string prefix);


//----------------------------------------------------------------------------

// Input for main function see error output in main
int main(int argc, char **argv) {
    string str,prefix = argv[1];

    //cout << "Before\n";
    //cin >> str;

    ReadBin(prefix);

    //cout << "After removing from memory\n";
    //cin >> str;
}

//----------------------------------------------------------------------------

/** Number of stage, states or actions given iHMDP. */
void ReadBin(string prefix) {
    string str;
    HMDP * pHMDP = new HMDP(prefix+"stateIdx.bin", prefix+"stateIdxLbl.bin",
        prefix+"actionIdx.bin", prefix+"actionIdxLbl.bin",
        prefix+"actionWeight.bin", prefix+"actionWeightLbl.bin",
        prefix+"transProb.bin");
    //cout << "After reading bin\n";
    //cin >> str;

    pHMDP->BuildHMDP();
    cout << "Log:" << endl << pHMDP->GetLog() << endl << "----" << endl;

    /*cout << pHMDP->GetIdS("1,0") << endl;
    cout << vec2String<idx>(pHMDP->GetIdSStage("1")) << endl;
    cout << pHMDP->states[pHMDP->GetIdS("1,0")].StateStr() << endl;
    cout << pHMDP->GetActionInfo(3,0) << endl;*/


    idx iW = 1;
    idx iDur = 0;
    flt g = 0;
    pHMDP->PolicyIteAve(iW,iDur,10);


	/*vector<flt> ini;
	ini.assign(1,0);
	pHMDP->ValueIteInfDiscount(1,0.00001,1,0,0.03,365,ini);
    cout << "Log:" << endl << pHMDP->GetLog() << endl << "----" << endl;
    //cout << "After building\n";
    //cin >> str;

    flt rate = 0.03;
    flt rateBase = 365;
    idx iW = 1;
    idx iDur = 2;*/
    /*pHMDP->SetPolicyAction(0,0);
    pHMDP->SetPolicyAction(1,0);
    pHMDP->SetPolicyAction(2,0);
    pHMDP->CalcStadyStatePr();
    (pHMDP->GetTransPr()).Print();
    cout << pHMDP->GetLog() << endl;*/


    //pHMDP->PolicyIteAve(1,0,100);
    //cout << "Log:" << endl << pHMDP->GetLog() << endl << "----" << endl;

    delete pHMDP;
    //hmdp1.Print();

    //hmdp1.Check(0.000001);
    //cout << hmdp1.GetLog() << endl;
    //hmdp1.BuildHMDP();

    /*vector<flt> v(1,0);
	hmdp1.ValueIteFinite(0,v);
    cout << hmdp1.PolicyW(11,0) << endl;
    cout << hmdp1.PolicyW(10,0) << endl;


    rate = 0.03;
    rateBase = 365;
    idx iW = 1;
    idx iDur = 2;
    flt g = 0;*/

    //hmdp1.ValueIteInfDiscount(1,0.000001,1,0,rate,rateBase);
    //hmdp1.CalcRPO(3,1,0);
    //hmdp1.PolicyIteDiscount(1,0,rate,rateBase);
    //g = hmdp1.PolicyIteAve(iW,iDur);
    //hmdp1.PolicyIteAve(1,2);
    //cout << hmdp1.PolicyInfoIdx(1) << endl;

    /*cout << g << endl;
    for (idx iS=0; iS<hmdp1.states.size(); ++iS)
        cout << hmdp1.CalcRPOAve(iS,iW,0,iDur,g) << endl;*/

    //for (i=0; i<hmdp1.states.size(); i++) cout << i << ": " << hmdp1.StateActionsToHgf(i) << endl;
}


//----------------------------------------------------------------------------

/** Number of stage, states or actions given iHMDP. */
idx Ctr(const vector<idx> iHMDP) {
    int level = iHMDP.size() / 3;
    int idxType = iHMDP.size() % 3; // next index specify 0: stage, 1: state, 2: action

    if (level==0) {
        if (idxType==0 | idxType==1) return 2;
        if (iHMDP[0]==1) return 0;  // second stage
        if (idxType==2 & iHMDP[1]==0) return 2;
        if (idxType==2 & iHMDP[1]==1) return 1;

    }
    if (level==1) {   // d0,s0,a0,d1,s1,a1
        // second stage at level 0
        if (iHMDP[0]==1) return 0;
        // stages at level 1
        if (idxType==0) return 3;
        // states at level 1
        if (idxType==1 & iHMDP[3]==0) return 1; // dummey state
        if (idxType==1 & iHMDP[2]==1 & iHMDP[3]==1) return 1;
        if (idxType==1) return 2;
        // actions at level 1
        if (idxType==2 & iHMDP[3]==0) return 1; // dummy action at stage 0
        // process 0,0,0
        if (iHMDP[0]==0 & iHMDP[1]==0 & iHMDP[2]==0 & iHMDP[3]==1) return 2;
        if (iHMDP[0]==0 & iHMDP[1]==0 & iHMDP[2]==0 & iHMDP[3]==2) return 1;
        // process 0,0,1
        if (iHMDP[0]==0 & iHMDP[1]==0 & iHMDP[2]==1 & iHMDP[3]==1) return 2;
        if (iHMDP[0]==0 & iHMDP[1]==0 & iHMDP[2]==1 & iHMDP[3]==2 & iHMDP[4]==0) return 1;
        if (iHMDP[0]==0 & iHMDP[1]==0 & iHMDP[2]==1 & iHMDP[3]==2 & iHMDP[4]==1) return 2;
        // process 0,1,0
        if (iHMDP[0]==0 & iHMDP[1]==1 & iHMDP[2]==0 & iHMDP[3]>=1) return 1;
        cout << "Error in Ctr!\n";
        exit(1);
    }
    return 1;
}


//----------------------------------------------------------------------------

void RunPrintHgraph(char filename[]) {
    cout << "\nPrint hypergraph: " << filename << endl;
    Hypergraph H(filename);     // Reads the hypergraph
    H.PrintArcs();
}


    /*MatDouble I(5,true);
    MatDouble P(5,false);
    P.Set(2);
    P(0,0) = -3;

    I.Print();
    P.Print();
    MatAlg alg;

    alg.IMinusP(P);
    P.Print();*/

    /*MatAlg alg;
    MatDouble P(2,true);
    MatDouble r(2,1);
    MatDouble w(2,1);
    r.Set(2);
    P.Print();
    r.Print();
    w.Print();

    alg.LASolve(P,w,r);
    P.Print();
    r.Print();
    w.Print();
    exit(0);*/


    /*HMDP hmdp(3, INFINT);
    //cout << "time: " << hmdp.timeHorizon << endl;
    hmdp.AddWeight("Duration");
    hmdp.AddWeight("Net reward");
    hmdp.AddWeight("Items");
    vector<idx> iHMDP;
    string str,str1;
    idx d0,s0,a0,d1,s1;         // stage, state and action idx
    idx d0S,s0S,a0S,d1S,s1S;    // stage, state and action count

    // loop for a HMDP with 2 levels to define state indexes
    for (d0=0,d0S=Ctr(iHMDP); d0<d0S; d0++) {   // stages of founder
        iHMDP.push_back(d0);
        for (s0=0,s0S=Ctr(iHMDP); s0<s0S; s0++) {   // states of founder
            iHMDP.push_back(s0);
            //for(idx ii=0; ii < iHMDP.size(); ii++) cout << iHMDP[ii] << " ";cout << endl;
            str = "M" + ToString(s0);
            hmdp.AddState(iHMDP,str);
            for (a0=0,a0S=Ctr(iHMDP); a0<a0S; a0++) {   // actions of founder
                iHMDP.push_back(a0);
                for (d1=0,d1S=Ctr(iHMDP); d1<d1S; d1++) {   // stages of level 1
                    iHMDP.push_back(d1);
                    for (s1=0,s1S=Ctr(iHMDP); s1<s1S; s1++) {   // states of level 1
                        //cout << "s: " << Ctr(iHMDP) << endl;
                        iHMDP.push_back(s1);
                        //for(idx ii=0; ii < iHMDP.size(); ii++) cout << iHMDP[ii] << " " << flush;cout << endl;
                        str1 = "C" + ToString(s1);
                        hmdp.AddState(iHMDP,str1);
                        iHMDP.pop_back();
                        //for(idx ii=0; ii < iHMDP.size(); ii++) cout << iHMDP[ii] << " " << flush;cout<< "pop s1" << endl;
                    }
                    iHMDP.pop_back();
                    //for(idx ii=0; ii < iHMDP.size(); ii++) cout << iHMDP[ii] << " " << flush;cout<< endl;
                }
                iHMDP.pop_back();
                //for(idx ii=0; ii < iHMDP.size(); ii++) cout << iHMDP[ii] << " " << flush;cout<< endl;
            }
            iHMDP.pop_back();
            //for(idx ii=0; ii < iHMDP.size(); ii++) cout << iHMDP[ii] << " " << flush; cout<< endl;
        }
        iHMDP.pop_back();
        //for(idx ii=0; ii < iHMDP.size(); ii++) cout << iHMDP[ii] << " " << flush; cout<< endl;
    }

    vector<HMDPState>::iterator it;
    for (it = hmdp.states.begin(); it != hmdp.states.end(); it++ ) {
        //cout << it->StateStr() << endl;
    }

    for (i=0; i<hmdp.states.size(); i++) {
        //cout << "i:" <<  i << " " << hmdp.states[i].StateStr() << endl;
    }
    cout << endl;

    /*multimap<string, int >::iterator iter;
    for(iter = hmdp.stages.begin(); iter != hmdp.stages.end(); ++iter ) {
        //cout << " Name: " << iter->first << " str: " << hmdp.states[iter->second].label << endl;
    }

    pair< multimap<string, int >::iterator, multimap<string, int >::iterator > ii;
    ii = hmdp.stages.equal_range("  0,0,0,2");

    // we can iterate over a range just like with begin() and end()
    for( iter = ii.first; iter != ii.second; ++iter ) {
        //cout << " Name: " << iter->first << " str: " << hmdp.states[iter->second].StateStr() << endl;
    }*/

    // next add actions
    /*uInt sUp,sNext,sDown;  // number of states at father, current and child.
    for (i=0; i<hmdp.states.size(); i++) {
        sUp=hmdp.CountFather(i);
        sNext = hmdp.CountNext(i);
        //cout << "i:" << i << " " << hmdp.states[i].StateStr() << " up:" << sUp << " next:" << sNext << endl;
        for (idx a=0; a<Ctr(hmdp.states[i].idxHMDP); a++) {
            sDown = hmdp.CountChild(i,a);
            //cout << "  a:" << a << " down:" << sDown << endl;
            string str2 = "A"+ToString(a);
            HMDPAction action(str2);
            if (sDown==1) { // if action def child
                action.AddTransPr(0,1,2);
                action.SetWeightsToZero(3);
                //action.SetTime(0);
                hmdp.states[i].AddAction(action);
                continue;
            }
            if (hmdp.states[i].idxHMDP[3]==0) { // stage 0 (dummy)
                for (idx j=0; j<sNext; j++) action.AddTransPr(j,1/(flt)sNext,1);
                flt tmp[] = {0, 0, 1};
                vector<flt> vTmp(tmp, tmp+sizeof(tmp)/sizeof(flt));
                action.SetWeights(vTmp);
                //action.SetTime(0);
                hmdp.states[i].AddAction(action);
                continue;
            }
            if (sNext==0 & a==0) {   // A0 action to father
                action.AddTransPr(0,1,0);
                flt tmp[] = {1, 4, 0};
                vector<flt> vTmp(tmp, tmp+sizeof(tmp)/sizeof(flt));
                action.SetWeights(vTmp);
                //action.SetTime(1);
                hmdp.states[i].AddAction(action);
                continue;
            }
            if (sNext==0 & a==1) {   // A1 action to father
                action.AddTransPr(0,0.5,0);
                action.AddTransPr(1,0.5,0);
                flt tmp[] = {0, 10, 5};
                vector<flt> vTmp(tmp, tmp+sizeof(tmp)/sizeof(flt));
                action.SetWeights(vTmp);
                //action.SetTime(1);
                hmdp.states[i].AddAction(action);
                continue;
            }
            if (sNext>0 & a==0) {   // A0 action of child
                if (hmdp.states[i].idxHMDP[4]==0) action.AddTransPr(0,1,1);
                if (hmdp.states[i].idxHMDP[4]==1) action.AddTransPr(1,1,1);
                action.SetWeightsToZero(3);
                //action.SetTime(1);
                hmdp.states[i].AddAction(action);
                continue;
            }
            if (sNext>0 & a==1) {   // A1 action of child
                action.AddTransPr(0,0.5,1);
                action.AddTransPr(1,0.5,1);
                flt tmp[] = {1, 2, 1};
                vector<flt> vTmp(tmp, tmp+sizeof(tmp)/sizeof(flt));
                action.SetWeights(vTmp);
                //action.SetTime(1);
                hmdp.states[i].AddAction(action);
                continue;
            }
        }
    }

    //hmdp.Print();
    cout << endl;*/
    //hmdp.PrintCount();

    //for (i=0; i<hmdp.states.size(); i++) cout << i << ": " << hmdp.StateActionsToHgf(i) << endl;

    //hmdp.BuildHMDP();
    //hmdp.ValueIteInfDiscount(100,0.01,1,0,0.1,1);
    //hmdp.PolicyIteDiscount(1,0,0.1,1);
    //hmdp.PolicyIteAve(1,0);
    //hmdp.PolicyIteAve(1,2);


    /*MatSimple<int> mat = hmdp1.HgfMatrix();
    cout << mat.rows << endl << mat.cols << endl;
    mat.Print();
	int rows = mat.rows;
	int cols = mat.cols;

	int * vec = new int[rows*cols];
	for (int k=0; k<rows*cols; ++k) {
        cout << "k:" << k << " ";
	    cout << "r:" << k/rows << " ";
	    cout << "c:" << k-rows*(k/rows) << endl;
	    vec[k] = mat(k-rows*(k/rows),k/rows);
	}
	cout << endl;
	for (int k=0; k<rows*cols; ++k) cout << vec[k] << " ";
	cout << endl;*/
