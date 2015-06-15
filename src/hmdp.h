#ifndef HMDP_H
#define HMDP_H

//-----------------------------------------------------------------------------

#include <fstream>
#include <vector>
#include <string>
#include <map>
#include <set>
#include <queue>
#include <algorithm>
#include <cmath>
#include "timer.h"
#include "basicdt.h"
#include "matrix.h"    // simple matrix class
#include "matalg.h"    // linear equations solver using lapack

#include <stdlib.h>     // For use of exit command
#include <stdio.h>      // For use of scanf and printf

//-----------------------------------------------------------------------------

using namespace std;

//-----------------------------------------------------------------------------

/** Transition in and action.
 * Container for id of state and trans pr.
 */
class HMDPTrans {
    friend class HMDPReader;
    friend class HMDPAction;
    friend class HMDP;

public:
    /** Create new HMDPTrans. */
    HMDPTrans(idx idS, flt prS) {
        id = idS;
        pr = prS;
    }

    /** For comparing HMDPTrans objects when sort them against id. */
    bool operator< (const HMDPTrans & rhs) const {
       return id < rhs.id;
    }

private:
    /** Print the transition. */
    string Print() {
        ostringstream out;
        out << "(" << id << ", " << pr << ")";
        return out.str();
    }

private:
    idx id; ///< Id of transition state.
    flt pr; ///< Transition probability.
};

//-----------------------------------------------------------------------------

/** Action of a state. */
class HMDPAction {
    friend class HMDPReader;
    friend class HMDPState;
    friend class HMDP;

 public:

    /** Print the action. */
    string Print() {
        ostringstream out;
        out << "(" << label << ") trans: ";
        for (idx i=0; i<trans.size(); i++) {
            out << trans[i].Print() << " ";
        }
        out << " w = " << vec2String(w);
        return out.str();
    }

// Get functions --------------------------------------------------------------

    /** Return vector with transition pr. */
    vector<flt> GetTransPr() {
        vector<flt> v;
        for (idx i=0; i<trans.size(); i++) v.push_back(trans[i].pr);
        return v;
    }

    /** Return vector with transition state ids. */
    vector<idx> GetTransIds() {
        vector<idx> v;
        for (idx i=0; i<trans.size(); i++) v.push_back(trans[i].id);
        return v;
    }

    /** Return vector with weights. */
    vector<flt> GetW() {
        return w;
    }

    /** Return label. */
    string GetLabel() {
        return label;
    }

 private:

    /** Create an action. */
    HMDPAction(vector<idx> & iStates, vector<flt> & transPr, vector<flt> & weights, string & lbl) {
        label = lbl;
        w = weights;
        AddTransPr(iStates,transPr);
    }


    /** Add transition probabilities to the action.
     * \param id Index of transition states.
     * \param pr The probabilities.
     */
    void AddTransPr(vector<idx> & id, vector<flt> & pr) {
        for (idx i=0; i<pr.size(); ++i) {
            trans.push_back(HMDPTrans(id[i],pr[i]));
        }
    }

// Set functions --------------------------------------------------------------

    /** Set all trans pr to a value. */
    void SetAllTransPr(flt val) {
        for(idx i=0; i<trans.size(); ++i) trans[i].pr = val;
    }


    /** Set transition probability of an existing trans
     * \param id Index of transition state.
     * \param pr The probability.
     * \pre Trans must have been sorted.
     * \post Search for id and if found change pr.
     * \return Old trans pr value (if not found -1).
     */
    flt SetTransPr(idx & id, flt & pr) {
        pair< vector<HMDPTrans>::iterator, vector<HMDPTrans>::iterator> bounds;
        bounds=equal_range(trans.begin(), trans.end(), HMDPTrans(id,0));
        //cout << "SetTransPr" << endl;
        if (bounds.first == bounds.second) return -1;
        flt old = bounds.first->pr;
        bounds.first->pr = pr;
        return old;
    }

    /** Sort trans pr increasing in id.
     * /pre All trans pr have been added.
     */
    void Sort() {
        sort(trans.begin(), trans.end());
    }




    typedef vector<HMDPTrans>::iterator trans_iterator;
    typedef vector<HMDPTrans>::const_iterator const_trans_iterator;
    trans_iterator begin() { return trans.begin(); }
    trans_iterator end() { return trans.end(); }

private:
    vector<flt> w;    ///< Weights/quantities for the action.
    string label;     ///< Action label.
    vector<HMDPTrans> trans;     ///< Transitions.
};

//-----------------------------------------------------------------------------

/** A state in the HMDP. */
class HMDPState {
    friend class HMDPReader;
    friend class HMDP;

 private:
    HMDPState(const string & lbl) {
        label = lbl;
    }

// Add methods --------------
    void AddAction(vector<flt> & w, vector<idx> & tails, vector<flt> & pr, string & label) {
        actions.push_back(HMDPAction(tails,pr,w,label));
    }

    string Print() {
        ostringstream out;
        out << "(" << label << ")";
        if (actions.size()>0) out << " actions: " << endl;
        for (idx i=0; i<actions.size(); i++) out << "       " << actions[i].Print() << endl;
        return out.str();
    }

    typedef vector<HMDPAction>::iterator action_iterator;
    action_iterator action_begin() { return actions.begin(); }
    action_iterator action_end() { return actions.end(); }

private:
    vector<HMDPAction> actions;  ///< Actions of the state.
    string label;   ///< State label.
    flt w;          ///< Weight/reward for the state.
    int pred;   ///< Index of predecessor action (negative if not allocated).
};

//-----------------------------------------------------------------------------

/** HMDP class.

Contains an vector \code states of HMDPstate objects.

Structure:
    - The \code{states} vector satisfy that 1) states are ordered according to a
      valid ordering 2) states are ordered such that they lie constitutively in
      memory for a given stage.
    - A map \code{stages} is used identify stages. The string of a stage, return
      a pair (first id in \code{states}, number of states (size)), i.e. you may
      scan \code{states} from \code{states[id]} to \code{size-1} to find states
      of the stage.
    - A HMDPstate contains a vector of HMDPActions
    - A HMDPAction contains a vector of HMDPtrans which are sorted according to state id
    - A HMDPTrans contain the id of the stage and the transition pr


NOTE when a HMDP is built from binary files the id's to identify states in the
binary files will not be the same as the id's in \code{states}. After the HMDP
is built it is not a good idea to add new states since this will invalidate
the properties of the \code{states} vector.

Algorithms are include inside the class for easy call. However, only public
methods and variables are used.

\version{2.0}
 */
class HMDP
{
 public:
    friend class HMDPReader;

    enum Crit {DiscountedReward, AverageReward, Reward, TransPr, TransPrDiscounted};    ///< Criterion used.

// Iterators --------------------------------------------------------------
    typedef HMDP* HMDPPtr;
    typedef vector<HMDPState>::iterator state_iterator;
    state_iterator state_begin() { return states.begin(); }
    state_iterator state_end() { return states.end(); }
    state_iterator state_begin(string stageStr) {
        pair<idx,idx> sP = stages[stageStr];
        return states.begin() + sP.first;
    }
    state_iterator state_end(string stageStr) {
        pair<idx,idx> sP = stages[stageStr];
        return states.begin() + sP.first + sP.second;
    }

    typedef vector<HMDPAction>::iterator action_iterator;
    action_iterator action_begin(state_iterator iteS) { return iteS->actions.begin(); }
    action_iterator action_end(state_iterator iteS) { return iteS->actions.end(); }

    typedef vector<HMDPTrans>::iterator trans_iterator;
    trans_iterator trans_begin(action_iterator iteA) { return iteA->trans.begin(); }
    trans_iterator trans_end(action_iterator iteA) { return iteA->trans.end(); }

    typedef map< string, pair<idx,idx> >::iterator stage_iterator;
    stage_iterator stage_begin() { return stages.begin(); }
    stage_iterator stage_end() { return stages.end(); }



    /** Create a HMDP from binary files
     */
    HMDP(vector<string> binNames, bool verbose_)
    {
        verbose = verbose_;
        LoadBin(binNames[0], binNames[1], binNames[2], binNames[3],
                binNames[4],  binNames[5], binNames[6], binNames[7]);
    }


    /** Create a HMDP from binary files using the default names and a prefix
     */
    HMDP(string prefix)
    {
        //cout << "Constructor called." << endl;
        verbose = false;
        string stateIdxFile = prefix + "stateIdx.bin";
        string stateIdxLblFile = prefix + "stateIdxLbl.bin";
        string actionIdxFile = prefix + "actionIdx.bin";
        string actionIdxLblFile = prefix + "actionIdxLbl.bin";
        string actionWFile = prefix +  "actionWeight.bin";
        string actionWLblFile = prefix + "actionWeightLbl.bin";
        string transProbFile = prefix + "transProb.bin";
        string externalFile = prefix + "externalProcesses.bin";
        LoadBin(stateIdxFile, stateIdxLblFile, actionIdxFile, actionIdxLblFile,
                actionWFile,  actionWLblFile, transProbFile, externalFile);
    }

    //~HMDP() {cout << "Deconstructor called." << endl;}

    /** Create a HMDP from binary files.
     */
    void LoadBin(string stateIdxFile, string stateIdxLblFile, string actionIdxFile,
        string actionIdxLblFile, string actionWFile,  string actionWLblFile,
        string transProbFile, string externalFile);


    /** Check the HMDP for errors.
     * The following are checked:
     * - Probabilities sum to one.
     * - That all transitions are to states which exists.
     * \param eps The sum of the probabilities must at most differ eps from one.
     * \return 0 if okay, 1 if warning, 2 if error.
     */
    uSInt Check(flt eps);


//    /** Create a HMDP with no actions and states.*/
//    HMDP(){okay = true;};
//
//    /** Create a HMDP with no actions and states.
//     * \param levels Number of levels in the HMDP.
//     * \param timeHorizon The time-horizon. If infinite use INFINT here.
//     * \param rate The interest rate.
//     * \param rateBase The time-horizon the rate is valid over.
//     * \note Levels are numbered from zero, i.e. we have level <tt>0, ..., levels-1</tt>.
//     */
//    //HMDP(uInt levels, uInt timeHorizon, flt rate, flt rateBase);
//
//
//    /** Create a HMDP with no actions and states.
//     * \param levels Number of levels in the HMDP.
//     * \param timeHorizon The time-horizon. If infinite use INFINT here.
//     * \note Levels are numbered from zero, i.e. we have level <tt>0, ..., levels-1</tt>.
//     */
//    //HMDP(uInt levels, uInt timeHorizon);
//
//

    /** Given a set of external process states corresponding to the first stage in the external process,
     * add the stage label of each external process to the states/nodes as its label.
     */
    void ExternalAddStageStr();


    /** Set external process states corresponding to the first stage in the
     * external process to -INF.
     */
    void ExternalResetStates();


    /** Set the reward, duration and trans pr of external process actions to zero.
     * \param idxW Index of the weight used.
     * \param idxD Index of the duration.
     */
    void ExternalResetActions(const idx & idxW, const idx & idxD);


    /** Update external process states corresponding to the first stage in the external process.
     * \param crit Criterion used (enum type).
     * \param iteS State iterator to state in external stage.
     * \param curPrefix The prefix of the current external process in memory.
     * \param pExt Pointer to the current external process.
     * \param idxW Index of the weight used.
     * \param idxD Index of duration.
     * \param g Current average reward.
     * \param rate The interest rate.
     * \param rateBase The time-horizon the rate is valid over.
     * \return True if a new policy of the external process is found.
     */
    bool ExternalStatesUpdate(Crit crit, state_iterator iteS, string & curPrefix, HMDPPtr & pExt,
        const idx & idxW, const idx & idxD, const flt & g, const flt & rate, const flt & rateBase);


    /** Copy values between the HMDP and the external process.
     * \param stage Stage string of the HMDP.
     * \param stageExt Stage string of the external HMDP.
     * \param pExt Pointer to the current external process.
     * \param toExt True if move values to the external process (false if move from).
     */
    void ExternalCopyWState(string stage, string stageExt, const HMDPPtr & pExt, const bool toExt);


    /** Return true if the state is a external process state corresponding to the first stage in a external process.
     * \param ite State iterator to state.
     */
    bool ExternalState(state_iterator ite) {
        if (externalProc) {
            if (ite->actions.size()==1) {    // could be a node in an external process not calculated yet
                if (external.count(ite->label)>0) return true;
            }
        }
        return false;
    }


    /** Allocate memory for the external process (check if not already allocated).
     * \param prefix Prefix of the external process.
     * \param curPrefix The prefix of the current external process in memory.
     * \param pExt Pointer to the current external process.
     */
    void ExternalAllocteMem(HMDPPtr & pExt, const string & prefix, string & curPrefix);


    /** Set the values of the external actions to the reward, duration and trans pr of the external process
     * \param stageStr Stage string of states corresponding to the first stage in the external process
     * \param pExt Pointer to the current external process.
     * \param idxW Index of the weight used.
     * \param idxD Index of duration.
     * \return True if the values have changed (indicate that the policy has changed).
     */
    bool ExternalSetActions(string stageStr, const HMDPPtr & pExt, const idx & idxW, const idx & idxD);


// Add functions ---------------------

//    /** Add a stage */
//    void AddStage(const string & stageStr) {
//
//    }

//    /** Add an action */
//    void AddAction(const string & stageStr, const idx iS, const vector<idx> & iStates,
//        const vector<flt> & pr, const vector<int> & scp, const string & lbl);
//
//
//    /** Add a new state with no actions defined yet
//     * \param stageStr Stage string in the format "0,1,0,3". Note always of size
//     * 1+3*level, e.g vector "0,1,0,3" says that we consider stage 0,
//     * state 1 and action 0 at the founder and stage 3 at level one.
//     * \param label The label of the state.
//     */
//    StatePtr AddState(const string & stageStr, const string &label) {
//        StatePtr pS;
//        map< string, HMDPStage>::iterator it;
//        it = stages.find(stageStr);
//        if (it != stages.end() ) { // already defined
//            pS = it->second.AddState(label);
//        }
//        else {
//            stages[stageStr] = HMDPStage();
//            pS = stages[stageStr].AddState(label);
//        }
//        return pS;
//    }
//
//
//    /** Add a new state with no actions defined yet.
//     * \param iHMDP The index vector of the HMDP state. Always of size
//     * 2+3*level, e.g vector [0,1,0,3,2] says that we consider stage 0,
//     * state 1 and action 0 at the founder and stage 3 and state 2 at level one.
//     */
//    void AddState(const string & stageStr, const idx iS);
//


// Set functions ---------------------

    /** Set number of weights stored in actions (and their names). */
    void SetActionWeightNames(const vector<string> & names) {
        weightNames = names;
    }


    /** Set the action id of the predecessor action. */
    void SetPred(int id) {
        for (idx i=0; i<states.size(); ++i) {
            if (states[i].actions.size()>0) states[i].pred = id;
            else states[i].pred = -1;   // states with no actions
        }
    }

    /** Set the weights of all states.
     * @param val Value.
     */
    void SetAllStateW(flt & val) {
        for (idx i=0; i<states.size(); ++i) states[i].w = val;
    }


    /** Set the weights of all states.
     * @param val Value.
     */
    void SetStateW(vector<idx> & iS, flt val) {
        for (idx i=0; i<iS.size(); ++i) states[iS[i]].w = val;
    }


    /** Set the weights of all states in a stage.
     * @param stageStr Stage string.
     * @param val Value.
     */
    void SetStateWStage(string stageStr, flt val) {
        pair<idx,idx> pS = stages[stageStr];
        idx iS = pS.first;
        for (idx i=0; i<pS.second; ++i, ++iS) states[iS].w = val;
    }


    /** Set the action weight.
     * \param w The weight to set.
     * \param iS The index of the state we consider in \code states.
     * \param iA The index of the action we consider.
     * \param iW The weight index.
     */
    void SetActionW(const flt & w, const idx & iS, const idx & iA, const idx & iW) {
        states[iS].actions[iA].w[iW] = w;
    }


    /** Set all the transition pr to zero of an action.
     * \param iS Id of the state.
     * \param iA Id of the action.
     */
    void SetActionPrZero(const idx & iS, const idx & iA) {
        states[iS].actions[iA].SetAllTransPr(0);
    }


    /** Set the values in \code r to the weights of the stage.
     * \pre Matrix \code r must have dim (|S|,1) where |S| denote the number
     * of states at the founder level.
     * \param r The matrix.
     * \param stageStr The stage under consideration.
     */
    void SetMatrixVal(MatSimple<double> &r, string stageStr) {
        idx i;
        state_iterator iteS;
        for (iteS = state_begin(stageStr), i=0; iteS!=state_end(stageStr); ++iteS, ++i) {
            r(i,0) = w(iteS);
        }
    }


    /** Set the value of the transition pr.
     * \param pr The transition pr.
     * \param iS Id of the state.
     * \param iA Id of the action.
     * \param iSTail Id the the tail state.
     * \return The old transition pr.
     */
    flt SetGetActionPr(const flt & pr, const idx & iS, const idx & iA, const idx & iSTail);

    /** Set the action of the policy.
     * \param iS The id of the state we consider in \code states.
     * \param iA The action index.
     */
    void SetPolicy(vector<idx> iS, vector<idx> iA) {
        for (idx i=0; i<iS.size(); ++i)
            states[iS[i]].pred = iA[i];
    }

// Print functions -------------------

    /** Print the HMDP, i.e. its states and actions. */
    string Print();


//
//
//    /** Print the number of states at next level of the father, current and child. */
//    void PrintCount();


//    /** Get all information about an action.
//     * \param iS The index of the state we consider in \code states.
//     * \param iA The index of the action we consider.
//     */
//    string GetActionInfo(idx iS, idx iA) {
//        string str;
//        int idxHArc = FindAction(iS,iA);
//        if (idxHArc==0) return str;
//        vector<idx> tails = H.GetHArcTailIdx(idxHArc);
//        for (idx i=0; i<tails.size(); ++i) tails[i] = tails[i]-1;   // so that id start from zero
//        vector<flt> w = H.GetHArcWeights(idxHArc);
//        vector<flt> pr = H.GetHArcM(idxHArc,idxMult);
//        string label = H.GetHArcLabel(idxHArc);
//        str = "trans=" + vec2String<idx>(tails) + " pr=" + vec2String<flt>(pr) + " w=" + vec2String<flt>(w) + " (" + label + ")";
//        return str;
//    }
//
//
//    /** Get all information about an action.
//     * \param iS The index of the state we consider in \code states.
//     * \param iA The index of the action we consider.
//     */
//    vector<flt> GetActionTransPr(idx iS, idx iA) {
//        vector<flt> v;
//        int idxHArc = FindAction(iS,iA);
//        if (idxHArc==0) return v;
//        vector<flt> pr = H.GetHArcM(idxHArc,idxMult);
//        return pr;
//    }
//
//
//    /** Get the state-expanded hypergraph in matrix format. */
//    MatSimple<int> HgfMatrix() {
//        return H.HgfMatrix();
//    }
//
//    /** Get the transition probability matrix P given a policy for the founder. */
//    MatSimple<flt> GetTransPr() {
//        int rows = stages.count("0");
//        pair< multimap<string, int >::iterator, multimap<string, int >::iterator > pairZero;
//        pair< multimap<string, int >::iterator, multimap<string, int >::iterator > pairLast;
//        MatSimple<flt> P(rows,rows);    // Matrix of prob values
//        pairZero = stages.equal_range("0");
//        pairLast = stages.equal_range("1");
//        FounderPr(P,pairZero,pairLast);
//        return P;
//    }




//    /** Calculate the stady state probabilities for the founder chain (infinite time-horizon, ergodic chain).
//     * \return A vector with the probabilities
//     * \post Use \code GetLog to see the log.
//     */
//    vector<flt> CalcStadyStatePr();
//
//

//    /** Find the h(arc) corresponding to an action.
//     * \param iS The index of the state we consider in \code states.
//     * \param idxA The action index of the state we consider
//     * \return An integer of the index in the state-expanded hypergraph
//     * stored as pred (negative if arc, positive if harc). If zero then not found.
//     */
//    int FindAction(idx iS, idx idxA);
//
//
//    /** Calculate rentention payoff (RPO) for a state. Normally run
//     * after an optimal policy has been found.
//     * \param iS The index of the state we consider in \code states.
//     * \param idxW The index of weights to calculate.
//     * \param idxA The action index we calculate the RPO with respect to..
//     * \return A vector of the same size as the states containing the RPO values.
//     */
//    flt CalcRPO(idx iS, idx idxW, idx idxA) {
//        int idxHArc = FindAction(iS,idxA);
//        return HT.CalcRPO(H,idxW,idxMult,idxHArc);
//    }
//
//
//    /** Calculate rentention payoff (RPO) for a state (discount criterion). Normally run
//     * after an optimal policy has been found.
//     * \param iS The index of the state we consider in \code states.
//     * \param idxW The index of weights to calculate.
//     * \param idxA The action index we calculate the RPO with respect to.
//     * \param idxD The denominator we want to calculate the weight over.
//     * \param rate The interest rate.
//     * \param rateBase The time-horizon the rate is valid over.
//     * \return A vector of the same size as the states containing the RPO values.
//     */
//    flt CalcRPODiscount(idx iS, idx idxW, idx idxA, idx idxDur, flt rate,
//        flt rateBase)
//    {
//        int idxHArc = FindAction(iS,idxA);
//        //cout << "iS:" << iS << " a:" << idxHArc << " ";
//        return HT.CalcRPODiscount(H,idxW,idxMult,idxHArc,idxDur,rate,rateBase);
//    }
//
//
//    /** Calculate rentention payoff (RPO) for a state (average criterion). Normally run
//     * after an optimal policy has been found.
//     * \param iS The index of the state we consider in \code states.
//     * \param idxW The index of weights to calculate.
//     * \param idxA The action index we calculate the RPO with respect to.
//     * \param idxD The denominator we want to calculate the weight over.
//     * \param g The average gain.
//     * \return A vector of the same size as the states containing the RPO values.
//     */
//    flt CalcRPOAve(idx iS, idx idxW, idx idxA, idx idxDur, flt g) {
//        int idxHArc = FindAction(iS,idxA);
//        //cout << "iS:" << iS << " a:" << idxHArc << " ";
//        return HT.CalcRPOAve(H,idxW,idxDur,idxMult,idxHArc,g);
//    }
//
//
//    /** Fix an action, i.e. remove all other actions of the state.
//     * \param iS The index of the state we consider in \code states.
//     * \param iA The action index.
//     */
//    void FixAction(idx iS, idx iA) {
//        int idxHArc = FindAction(iS,iA);
//        H.FixHArc(idxHArc);
//    }
//
//
//
//

//    /** Set the state weights of a given stage.
//     * \param stageStr Stage string.
//     * \param iW The weight index.
//     * \param w The weights to set.
//     * \pre Assume that the size of \code w is at least the size of the states in the stage.
//     */
//    void SetStageW(string stageStr, idx iW, vector<flt> w) {
//        vector<idx> ids = GetIdSStage(stageStr);
//        for (idx i=0; i<ids.size(); ++i) {
//            cout << "Set state " << ids[i] << " to " << w[i] << endl;
//            H.itsNodes[ ids[i]+1 ].w[iW] = w[i];
//        }
//    }
//
//
//    /** Set the first (h)arcs weight of the states to \code w.
//     * \param idS Stage ids.
//     * \param iW The weight index.
//     * \param w The weights to set.
//     * \pre Assume that the size of \code w is at least the size of the states.
//     */
//    void SetWActions(vector<idx> idS, idx iW, vector<flt> w) {
//        for (idx i=0; i<idS.size(); ++i) {
//            H.itsNodes[ idS[i]+1 ].w[iW] = w[i];
//        }
//    }





//    /** Get the action weight.
//     * \param iS The index of the state we consider in \code states.
//     * \param iA The index of the action we consider.
//     * \param iW The weight index.
//     */
//    flt GetActionW(idx iS, idx iA, idx iW) {
//        int idxHArc = FindAction(iS,iA);
//		if (idxHArc<0) { // arc
//			ArcPtr pArc = H.GetArcsPtr()-idxHArc;
//			return pArc->w[iW];
//		}
//		if (idxHArc>0) { // hyperarc
//			HArcPtr pHArc = H.GetHArcsPtr() + idxHArc;
//            return pHArc->w[iW];
//		}
//		return -INF;
//    }
//
//
//    /** Remove the action.
//     * \param iS The index of the state we consider in \code states.
//     * \param iA The action index.
//     */
//    void RemoveAction(idx iS, idx iA) {
//        int idxHArc = FindAction(iS,iA);
//        H.RemoveHArc(idxHArc);
//    }
//
//
//    /** Reset actions in the HMDP, i.e. no actions removed/fixed. */
//    void ResetActions() {
//        H.ResetSubHgf();
//    }
//
//
//    /** Count the number of next stage states at current level.
//     * \param iState The index of the state we consider in \code states.
//     */
//    idx CountNext(idx iState) {
//        string str = states[iState].NextStageStr();
//        return stages.count(str);
//    }
//


//    /** Count the number of next stage states.
//     * \param iState The index of the state we consider in states.
//     */
//    idx CountFather(idx iState) {
//        string str = states[iState].NextFatherStageStr();
//        return stages.count(str);
//    }
//
//    /** Count the number of states in the stage.
//     * \param stage The string of the stage, e.g. "0,1,2,1".
//     */
//    idx CountStates(string stage) {
//        return stages.count(stage);
//    }
//
//    /** Return string with optimal policy using indicies. */
//    string PolicyInfoIdx(idx idxW);
//
//
//    /** Return string with optimal policy using labels.
//     * Note only last state label shown.
//     */
//    string PolicyInfoLabel(idx idxW);
//
//
//    /** Return the label of the action of a specific state for the policy.
//     * \param iState The index of the state.
//     */
//    string PolicyLabel(idx iState) {
//        string label;
//        int a;
//        a = H.itsNodes[iState+1].pred[idxPred];
//        if (a<0) label = states[iState].actionLabels[H.itsArcs[-a].pLabel -
//            &states[iState].actionLabels[0]];
//        if (a>0) label = states[iState].actionLabels[H.itsHArcs[a].pLabel -
//            &states[iState].actionLabels[0]];
//        return label;
//    }
//
//





//
//    /** Return actions for the specific state in hypergraph 'f 6' format. */
//    string StateActionsToHgf(const idx & iState, bool & findValidOdr);
//

//    /** Note state[i] corresponds to node i+1 in the state-expanded hypergraph
//     * since hypergraph nodes numbered from one.
//     */
//    idx HgfNodeIdx(idx iState) {return iState+1;}
//
//
//    /** Return actions for the specific state in hypergraph 'f 6' format. */
//    //string StateActionsToHgf(idx iState);

//
//    /** Return all index except idxW and idxDur. */
//    vector<idx> WeightIdx(idx idxW, idx idxDur);
//
//



    /** Calculate the weights of the founder states given a specific policy.
     * \param crit Criterion used.
     * \param w Column matrix storing the calculated weights.
     * \param idxW W  The index we consider.
     * \param pairZero Iterator pair pointing to stage zero at founder level.
     * \param pairOne Iterator pair pointing to stage one at founder level.
     * \note Modify the weights stored in the states of the HMDP.
     */
    void FounderW(Crit crit, MatSimple<double> &w, const idx &idxW, flt g = 0, idx idxD = 0, flt rate = 0, flt rateBase = 1)
    {
        //cout << "FounderW: idxW=" << idxW << " idxD=" << idxD << endl;
        SetStateWStage("1",0);
        CalcPolicy(crit, idxW, g , idxD, rate, rateBase);
        SetMatrixVal(w,"0");
    }


    /** Calculate the transition probabilities of the founder states given a specific policy.
     * \note Modify the state weights.
     */
    void FounderPr(Crit crit, MatSimple<double> &P, idx idxD = 0, flt rate = 0, flt rateBase = 1) {
        idx r,c;
        state_iterator iteS, iteZero;
        SetStateWStage("1", 0);
        for (iteS = state_begin("1"), c=0; iteS!=state_end("1"); ++iteS, ++c) {
            w(iteS) = 1;
            if (c>0) w(iteS-1) = 0; // restore previous
            CalcPolicy(crit,0,0,idxD,rate,rateBase);
            for (iteZero=state_begin("0"), r=0; iteZero!=state_end("0"); ++iteZero, ++r) { //cout << "WiteZ=" << w(iteZero) << " r=" << r << " c=" << c << endl;
                P(r,c) = w(iteZero);
            }
        }
    }


//
//
//    /** Reverse the sign of weight with index idxW. */
//    void ReverseWeight(idx idxW) {H.ReverseW(idxW);}


// ----------------------------------------------------------------------------
// Get methods -------------------

    string GetStageStr(vector<idx> & iState) {
        string str;
        idx size = iState.size();
        for(idx i=0; i<size-1; i++) {
            if (i<size-2) str.append(ToString(iState[i])+",");
            else str.append(ToString(iState[i]));
        }
        return str;
    }


    string GetStageStr(string stateStr) {
        idx pos = stateStr.find_last_of(",");
        return stateStr.substr(0,pos);
    }


    /** State string of state index vector. */
    string GetStateStr(vector<idx> & iState) {
        string str;
        idx size = iState.size();
        for(idx i=0; i<size; i++) {
            if (i<size-1) str.append(ToString(iState[i])+",");
            else str.append(ToString(iState[i]));
        }
        return str;
    }


    /** State string of state id.
     * \note Must search the stages map to find the stage.
    */
    string GetStateStr(idx sId) {
        string stateStr;
        for (stage_iterator iteN = stage_begin(); iteN!=stage_end(); ++iteN) {
            pair<idx,idx> pS = iteN->second;
            if (sId>=pS.first && sId<pS.first+pS.second) {
                stateStr = iteN->first + "," + ToString(sId-pS.first);
                break;
            }
        }
        return stateStr;
    }


    /** State strings of state ids. */
    vector<string> GetStatesStr(vector<idx> & sId) {
        vector<string> v;
        for(idx i=0; i<sId.size(); i++) {
            v.push_back(GetStateStr(sId[i]));
        }
        return v;
    }


    string GetNextStageStr(vector<idx> & iState) {
        string str;
        idx size = iState.size();
        for(idx i=0; i<size-1; i++) {
            if (i<size-2) str.append(ToString(iState[i])+",");
            else str.append(ToString(iState[i]+1));    // increase by one
        }
        return str;
    }


    /** Return the string of the next stage at the current level (do not check if exists).
     * \param curStageStr The string of the current stage (e.g. 'n0,s0,a0,n1').
     */
    string GetNextStageStr(string curStageStr) {
        uSInt found = curStageStr.find_last_of(",");
        int nextStage = atoi(curStageStr.substr(found+1).c_str()) + 1;
        return curStageStr.substr(0,found+1) + ToString<int>(nextStage);
    }


    string GetNextFatherStageStr(vector<idx> & iState) {
        string str;
        idx size = iState.size();
        if (size==2) return "NA";
        for(idx i=0; i<size-4; i++) {
            if (i<size-5) str.append(ToString(iState[i])+",");
            else str.append(ToString(iState[i]+1));
        }
        return str;
    }


    string GetNextChildStageStr(vector<idx> & iState, idx & iAction) {
        string str = GetStateStr(iState);
        str.append(","+ToString(iAction)+",0");
        return str;
    }


    string GetLastStageStr() {
        if (timeHorizon>=INFINT) return "1";
        else return ToString(timeHorizon-1);
    }


    /** Return which level the state is on (starting from zero). */
    int GetLevel(vector<idx> & iState) {
        return (iState.size()-2)/3;
    }


    /** Get the content of the log as a string. */
    string GetLog() {return log.str();}


    /** Get id of state. */
    idx GetId(string stateStr) {
        string stageStr = GetStageStr(stateStr);
        idx pos = stateStr.find_last_of(",");
        idx idxS;
        from_string<idx>(idxS,stateStr.substr(pos), std::dec);
        pair<idx,idx> sP = stages[stageStr];
        idx iS = sP.first + idxS;
        return iS;
    }


    /** Get id of state(s) as a vector. */
    vector<idx> GetIds(string stageStr) {
        vector<idx> v;
        pair<idx,idx> sP = stages[stageStr];
        idx iS = sP.first;
        for (idx i=0; i<sP.second; ++i, ++iS) {
            v.push_back(iS);
        }
        return v;
    }


    /** Return the labels of the actions of current policy.
     * \param iS Vector of state indices.
     */
    vector<string> GetStateLabel(vector<idx> iS) {
        vector<string> val;
        for (idx i=0; i<iS.size(); ++i) {
            state_iterator iteS = GetIte(iS[i]);
            val.push_back( iteS->label );
        }
        return val;
    }


    /** Return the state weights of a given stage.
     * \param stageStr Stage string.
     */
    vector<flt> GetStageW(string stageStr) {
        vector<flt> v;
        pair<idx,idx> sP = stages[stageStr];
        idx iS = sP.first;
        for (idx i=0; i<sP.second; ++i, ++iS) {
            v.push_back(states[iS].w);
        }
        return v;
    }


    /** Number of actions. */
    idx GetActionSize(state_iterator ite) {return ite->actions.size();}

    /** Number of states. */
    idx GetStateSize(string stageStr) {return stages[stageStr].second;}

    /** Number of states. */
    idx GetStateSize() {return states.size();}

    /** Number of actions. */
    idx GetActionSize() {
        idx size = 0;
        for (state_iterator iteS = state_begin(); iteS!=state_end(); iteS++)
            size += iteS->actions.size();
        return size;
    }

    /** Action weight name. */
    string GetWName(idx iW) {return weightNames[iW];}

    /** Id of state */
    idx GetId(state_iterator iteS) {return iteS - states.begin();}

    /** Iterator of a state. */
    state_iterator GetIte(idx iS) {return states.begin() + iS;}

    /** Iterator of an action. */
    action_iterator GetIte(state_iterator iteS, idx iA) {return action_begin(iteS) + iA;}

    /** Index of action */
    idx GetIdx(state_iterator iteS, action_iterator iteA) {return iteA - iteS->actions.begin();}


    /** Return the weight.
     * \param iS Vector of state indices.
     */
    vector<flt> GetPolicyW(vector<idx> iS) {
        vector<flt> val;
        for (idx i=0; i<iS.size(); ++i) {
            state_iterator iteS = GetIte(iS[i]);
            val.push_back( w(iteS) );
        }
        return val;
    }

    /** Return the weight.
     * \param stageStr Stage string.
     */
    vector<flt> GetPolicyWStage(string stageStr) {
        vector<idx> iS = GetIds(stageStr);
        return GetPolicyW(iS);
    }


    /** Return the index of the actions of current policy.
     * \param iS Vector of state indices.
     */
    vector<int> GetPolicy(vector<idx> iS) {
        vector<int> val;
        for (idx i=0; i<iS.size(); ++i) {
            state_iterator iteS = GetIte(iS[i]);
            val.push_back( pred(iteS) );
        }
        return val;
    }


    /** Return the index of the actions of current policy.
     * \param iS Vector of state indices.
     */
    vector<int> GetPolicyStage(string stageStr) {
        vector<idx> iS = GetIds(stageStr);
        return GetPolicy(iS);
    }


    /** Return the labels of the actions of current policy.
     * \param iS Vector of state indices.
     */
    vector<string> GetPolicyLabel(vector<idx> iS) {
        vector<string> val;
        for (idx i=0; i<iS.size(); ++i) {
            state_iterator iteS = GetIte(iS[i]);
            if (pred(iteS)<0) val.push_back( string() );    // if no pred
            else {
                action_iterator iteA = GetIte(iteS, pred(iteS));
                val.push_back( iteA->label );
            }
        }
        return val;
    }


// Accessors (get/set functions for the algorithms, return by reference)

    flt & w(state_iterator iteS) {return iteS->w;}
    flt & w(action_iterator iteA, idx iW) {return iteA->w[iW];}
    flt & w(state_iterator iteS, idx iA, idx iW) {return iteS->actions[iA].w[iW];}
    flt & pr(trans_iterator iteT) {return iteT->pr;}
    int & pred(state_iterator iteS) {return iteS->pred;}






    /** Reset log. */
    void ResetLog() {log.str("");}


// Algorithms -----------------------------------------------------------------

    /** Calculates the optimal policy of a single stage of the founder (a hypertree).
     * \pre Moreover, weights in states with no actions must have been set.
     * \post The policy is defined in pred and weights w[iSW] are calculated in each node.
     * \param crit Criterion used (enum type must be AverageReward, Reward, DiscountedReward, TransPr).
     * \param idxW The action weight index we want to optimize.
     * \param g The average reward (only used in criterion is AverageReward).
     * \param idxDur The action duration index, i.e. the discout rate for duration $d$ is $\exp(-rate/rateBase*d)$.
     * \param rate The interest rate.
     * \param rateBase The time-horizon the rate is valid over.
     *
     * \note The last three parameters is only used if criterion is DiscountedReward.
     * If we are minimizing the weights, i.e. if the goal is to minimize
     * the cost then the rewards at idxW must be multiplied with -1.
     * \return True if a new policy is found. Remember to reset the predecessors if no old policy before
     * running this method!
     */
    bool CalcOptPolicy(Crit crit, idx idxW = 0, flt g = 0, idx idxDur = 0, flt rate = 0, flt rateBase = 1);


    /** Calculates weights based on the current policy of a single stage of the founder.
     * \pre Moreover, weights in states with no actions must have been set.
     * \post The policy is defined in pred and weights w[iSW] are calculated in each node.
     * \param crit Criterion used (enum type must be AverageReward, Reward, DiscountedReward, TransPr).
     * \param idxW The action weight index we want to optimize.
     * \param g The average reward (only used in criterion is AverageReward).
     * \param idxDur The action duration index, i.e. the discout rate for duration $d$ is $\exp(-rate/rateBase*d)$.
     * \param rate The interest rate.
     * \param rateBase The time-horizon the rate is valid over.
     *
     * \note The last three parameters is only used if criterion is DiscountedReward.
     * If we are minimizing the weights, i.e. if the goal is to minimize
     * the cost then the rewards at idxW must be multiplied with -1.
     */
    void CalcPolicy(Crit crit, idx idxW = 0, flt g = 0, idx idxDur = 0, flt rate = 0, flt rateBase = 1);


    /** Policy iteration algorithm (infinite time-horizon).
     * \param crit Criterion used (enum type).
     * \param maxIte The max number of iterations. The model may loop if not unichain.
     * \param idxW Index of the weight used as nominator.
     * \param idxD The denominator we want to maximize the weight over.
     * \param rate The interest rate.
     * \param rateBase The time-horizon the rate is valid over.
     * \return g The gain.
     * \post Use \code GetLog to see the optimization log.
     */
    flt PolicyIte(Crit crit, uSInt maxIte, const idx idxW, const idx idxD, const flt rate=0, const flt rateBase=1);


    /** Policy iteration algorithm (infinite time-horizon) given a fixed policy.
     * \param crit Criterion used (enum type).
     * \param maxIte The max number of iterations. The model may loop if not unichain.
     * \param idxW Index of the weight used as nominator.
     * \param idxD The denominator we want to maximize the weight over.
     * \param rate The interest rate.
     * \param rateBase The time-horizon the rate is valid over.
     * \return g The gain.
     * \post Use \code GetLog to see the optimization log.
     */
    flt PolicyIteFixedPolicy(Crit crit, const idx idxW, const idx idxD, const flt rate=0, const flt rateBase=1);

     /** Value iteration algorithm.
     *
     * \param crit Criterion used (enum type).
     * \param maxIte The max number of iterations.
     * \param epsilon If max(w(t)-w(t+1))<epsilon then stop the algorithm, i.e
     *        the policy becomes epsilon optimal (see Puterman p161).
     * \param idxW Index of the weight used.
     * \param idxDur Index of duration such that discount rates can be calculated.
     * \param termValues Terminal values used at founder level.
     * \param g The average gain.
     * \param rate The interest rate.
     * \param rateBase The time-horizon the rate is valid over.
     * \post Use \code GetLog to see the optimization log.
     */
    void ValueIte(Crit crit, idx maxIte, flt epsilon, const idx idxW,
     const idx idxDur, vector<flt> & termValues,
     const flt g, const flt rate, const flt rateBase);


// Algorithm sub-functions ---------------------------------------------------

    /** Maximal difference between the weights at founder level. */
    flt MaxDiffFounder() {
        string stageZeroStr = "0";
        string stageLastStr = GetLastStageStr();
        flt m = -INF;
        for (state_iterator iteZ = state_begin(stageZeroStr), iteL=state_begin(stageLastStr);
            iteZ!=state_end(stageZeroStr); ++iteZ, ++iteL)
            {
               m = max(m, abs(w(iteZ)-w(iteL)) );
            }
        return m;
    }


public:
    int levels;                     ///< Number of levels in the HMDP, i.e. the levels are 0, ..., levels-1.
    uInt timeHorizon;               ///< INFINT if consider an infinite time horizon; otherwise the number of stages at the founder level.
    vector<string> weightNames;     ///< Names of the weights/quantities stored from index 1 in \code w (of a (hyper)arc in the hypergraph).
    map< string, pair<idx,idx> > stages;   ///< Ordered map of stages. The pair contains (state id to first stage in stages, total number of states at stage).
    vector<HMDPState> states;
    map<string, string> external;     ///< Store the external processes in format <stageIdx, prefix>
    bool okay;                      ///< True if reading was okay. Later used to check if an algorithm works okay.
    bool verbose;                   ///< More output to the log.
    bool externalProc;              ///< True if the HMDP use external processes
    ostringstream log;              ///< Stream to store log messages.
private:
    Timer timer;
};

//-----------------------------------------------------------------------------

/** Class for reading/loading HMDP models.

The HMDP must be represented using the HMDP binary format (v1.0) which is a
collection of 8 binary files:

  Seven binary files are created using the following format:
  - stateIdx.bin: File of integers containing the indexes defining all states in the format
    "d0 s0 -1 d0 s0 a0 d1 s1 -1 d0 s0 a0 d1 s1 a1 d2 s2 -1 d0 s0 ...". Here -1 is
    used to indicate that a new state is considered (new line).
  - stateIdxLbl.bin: File of characters in the format "sIdx label sIdx label ..." Here
    sIdx corresponds to the index/line number in stateIdxLbl.bin (index starts from 0).
    Note no delimiter is used.
  - actionIdx.bin: File of integers containing the indexes defining all actions in the format
    "sIdx scope idx scope idx scope idx -1 sIdx scope idx scope idx -1 sIdx scope -1 ...".
    sIdx corresponds to the index/line number in stateIdx.bin (index starts from 0).
    Next pairs (scope idx) will follow indicating the possible transitions. Scope can be 4 values:
    2 - A transition to a child process (stage zero in the child process), 1 - A transition
    to next stage in the current process, 0 - A transition to the next stage in the father
    process. Here idx in the pair denote the index of the state at the stage considered,
    e.g. if scope=1 and idx=2 we consider state number 3 at next stage in the current
    process. Finally, if scope = 3 then a transition to a state specified by it's state sIdx
    is given. That is, if scope=3 and idx=5 then
    we have a transition to the state specified at line 6 in stateIdxLbl.bin.
    This is use full when considering shared child processes.
  - actionIdxLbl.bin: File of characters in the format "aIdx label aIdx label ..." Here
    aIdx corresponds to the index/line number in actionIdx.bin (index starts from 0).
    Note no delimiter is used.
  - actionWeight.bin: File of doubles containing the weights of the actions in the format
    "c1 c2 c3 c1 c2 c3 ..." assuming three weights for each action.
  - actionWeightLbl.bin: File of characters containing the labels of the
    weights in the format "lable1 label2 label3" assuming three weights for each action.
  - transProb.bin: File of doubles containing the probabilities of the transitions
    defined in actions in actionIdx.bin. The format is
    "p1 p2 p3 -1 p1 -1 p1 p2 -1 ...". Here -1 is
    used to indicate that a new action is considered (new line).
  - externalProcesses.bin: File of characters in the format "stageStr prefix stageStr prefix...".
    Here stageStr corresponds to the index (e.g. n0 s0 a0 n1) of the stage corresponding to the
    first stage in the external process and prefix to the prefix of the external process. Note no
    delimiter is used.

  Note
  - The HMDP defined afterwards do not use the same state ids.
 */
class HMDPReader
{
public:

    /** Default constructor. Do nothing. */
    //HMDPReader():cpuTime(1) {};

    /** Set the pointer to the hypergraph we want to read data to.
     * \param stateIdxFile Filename of the state index file.
     * \param stateIdxLblFile Filename of the state label file.
     * \param actionIdxFile Filename of the action index file.
     * \param actionIdxLblFile Filename of the action label file.
     * \param actionWFile Filename of the action cost file.
     * \param transProbFile Filename of the transition probability file.
     * \param pHMPD Pointer to the HMDP.
     */
    HMDPReader(string stateIdxFile, string stateIdxLblFile, string actionIdxFile,
        string actionIdxLblFile, string actionWFile, string actionWLblFile,
        string transProbFile, string externalFile, HMDP *pHMDP, ostringstream & hmdpLog);

private:

    /** Read a binary file of T's into an array of T's.
        T could for instance be a float.
     * \return The size of the array p.
     */
    template <class T>
    idx ReadBinary(string file, T *&p);


    /** Add the states to the HMDP.
     * \param stateIdxFile Filename of the state index file.
     * \param stateIdxLblFile Filename of the state label file.
     */
    void AddStates(string stateIdxFile, string stateIdxLblFile);


    /** Add the actions to the HMDP.
     * \param actionIdxFile Filename of the action index file.
     * \param actionIdxLblFile Filename of the action label file.
     * \param actionWFile Filename of the action cost file.
     * \param transProbFile Filename of the transition probability file.
     */
    void AddActions(string actionIdxFile, string actionIdxLblFile,
        string actionWFile, string actionWLblFile, string transProbFile);

    /** Add the external processes to the HMDP.
     * Store stage idx and prefix in a map
     * \param externalFile Filename of the external processes file.
     */
    void AddExternal(string externalFile);


    /** Add dummy states at founder level if infinite time-horizon HMDP.
     */
    void Compile();


    /** Find state id of transitions given index and scope of action to iState.
     * State id is stored in the index vector of an TmpAction. The scope vector
     * is cleared after (to reduce memory req.).
     * \pre Changes the value of findValidOdr to true if finds a scope = 3.
     */
    void SetSIds(const idx & iState, bool & findValidOdr);


    /** Find a valid ordering of the states in stateVec.
     * \pre The seq. of order is a valid ordering.
     */
    void FindValidOdr(vector<idx> & order);


    /** Convert a state idx vector to a stage string. */
    string StageStr(vector<idx> iHMDP) {
        string str;
        idx size = iHMDP.size();
        for(idx i=0; i<size-1; i++) {
            if (i<size-2) str.append(ToString(iHMDP[i])+",");
            else str.append(ToString(iHMDP[i]));
        }
        return str;
    }

public:
    bool okay;            ///< True if reading was okay.
private:

    bool foundScp3;       ///< True if found a scope 3 (a valid odr must be found).
    multimap<string, int> stagesMap;   ///< Multimap to quickly find the different stages (state string -> sId).

	class TmpAction {   // to store sId for actions loaded from the binary file
	    public:
        void Clear() {
            index.clear(); pr.clear(); scp.clear(); w.clear(); label.clear();
        }
        idx sId;
        vector<idx> index;  ///< State indexes.
        vector<flt> pr;   ///< Transition probabilities.
        vector<idx> scp;  ///< The scope of the index. If 1 next stage in current process, if 0 next stage in father process, if 2 next stage in child process (i.e. stage 0) and if 3 a transition to a state specified by it's state id. That is, if scope=3 and idx=5 then we have a transition to the state[5]..
        vector<flt> w;    ///< Weights/quantities for the action.
        string label;     ///< Action label.
	};

	class TmpState {   // to store states loaded from the binary file
	    public:
        //TmpState() {fStarSize=0;}
//        void Clear() {
//            iHMDP.clear(); actions.clear(); label.clear();
//        }
        vector<idx> iHMDP;
        string label;
        vector<TmpAction> actions;
        vector< pair<idx,idx> > fStar;  ///< (iS,iA) pairs in the forward star.
        //idx fStarSize;
	};
    vector<TmpState> stateVec;  ///< Vector of all states with stateVec[sId] according to file definitions.

    HMDP * pHMDP;         ///< Pointer to the HMDP.
    Timer timer;

};

// -----------------------------------------------------------------------------



#endif // HMDP_H
