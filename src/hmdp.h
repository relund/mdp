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
#include <stdexcept>
#include <RcppArmadillo.h>
#include "timer.h"
#include "basicdt.h"

#include <stdlib.h>     // For use of exit command
// #include <stdio.h>      // For use of scanf

//-----------------------------------------------------------------------------

using namespace std;

//-----------------------------------------------------------------------------

class HMDPBuilder;

/** 
 * @brief Transition in an action.
 * 
 * Container for id of state and transition probability.
 */
class HMDPTrans {
    friend class HMDPReader;
    friend class HMDPAction;
    friend class HMDP;
    friend class HMDPSave;

public:
    /**
     * @brief Create new HMDPTrans. 
     * @param idS Id of state.
     * @param prS Transition probability.
     * @param weights Transition-level weights.
     */
    HMDPTrans(idx idS, flt prS, const vector<flt> & weights = vector<flt>()) {
        id = idS;
        pr = prS;
        w = weights;
    }

    /** 
     * @brief For comparing HMDPTrans objects when sorting them against id. 
     * @param rhs The other HMDPTrans object.
     * @return True if this id is smaller.
     */
    bool operator< (const HMDPTrans & rhs) const {
       return id < rhs.id;
    }

private:
    /** 
     * @brief Print the transition. 
     * @return String representation.
     */
    string Print() {
        ostringstream out;
        out << "(" << id << ", " << pr << ", w = " << vec2String(w) << ")";
        return out.str();
    }

private:
    idx id; ///< Id of transition state.
    flt pr; ///< Transition probability.
    vector<flt> w; ///< Transition-level weights r(s,a,s').
};

//-----------------------------------------------------------------------------

/** 
 * @brief Action of a state. 
 */
class HMDPAction {
    friend class HMDPReader;
    friend class HMDPState;
    friend class HMDP;
    friend class HMDPSave;

 public:

    /** 
     * @brief Print the action. 
     * @return String representation.
     */
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

    /** 
     * @brief Return vector with transition probabilities. 
     * @return Vector of probabilities.
     */
    vector<flt> GetTransPr() {
        vector<flt> v;
        for (idx i=0; i<trans.size(); i++) v.push_back(trans[i].pr);
        return v;
    }

    /** 
     * @brief Return vector with transition state ids. 
     * @return Vector of ids.
     */
    vector<idx> GetTransIds() {
        vector<idx> v;
        for (idx i=0; i<trans.size(); i++) v.push_back(trans[i].id);
        return v;
    }

    /** 
     * @brief Return vector with action weights. 
     * @return Vector of weights.
     */
    vector<flt> GetW() {
        return w;
    }

    /** 
     * @brief Return transition-level weights for all transitions. 
     * @return Vector of weights.
     */
    vector<flt> GetTransW() {
        vector<flt> v;
        for (idx i=0; i<trans.size(); i++) {
            v.insert(v.end(), trans[i].w.begin(), trans[i].w.end());
        }
        return v;
    }

    /** 
     * @brief Return action label. 
     * @return The label.
     */
    string GetLabel() {
        return label;
    }

 private:

    /** 
     * @brief Create an action. 
     * @param iStates Transition states.
     * @param transPr Transition probabilities.
     * @param weights Action weights.
     * @param transWeights Transition-level weights.
     * @param lbl Action label.
     */
    HMDPAction(vector<idx> & iStates, vector<flt> & transPr, vector<flt> & weights,
               vector< vector<flt> > & transWeights, string & lbl) {
        label = lbl;
        w = weights;
        AddTransPr(iStates,transPr,transWeights);
    }


    /** 
     * @brief Add transition probabilities to the action.
     * @param id Index of transition states.
     * @param pr The probabilities.
     * @param transWeights Transition-level weights.
     */
    void AddTransPr(vector<idx> & id, vector<flt> & pr, vector< vector<flt> > & transWeights) {
        for (idx i=0; i<pr.size(); ++i) {
            if (i<transWeights.size()) trans.push_back(HMDPTrans(id[i],pr[i],transWeights[i]));
            else trans.push_back(HMDPTrans(id[i],pr[i]));
        }
    }

// Set functions --------------------------------------------------------------

    /** 
     * @brief Set all transition probabilities to a value. 
     * @param val The value.
     */
    void SetAllTransPr(flt val) {
        for(idx i=0; i<trans.size(); ++i) trans[i].pr = val;
    }

    /** 
     * @brief Set transition probability of an existing transition.
     * @param id Index of transition state.
     * @param pr The probability.
     * @return Old transition probability value (if not found -1).
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

    /** 
     * @brief Sort transition probabilities increasing in id.
     */
    void Sort() {
        sort(trans.begin(), trans.end());
    }

    typedef vector<HMDPTrans>::iterator trans_iterator;
    typedef vector<HMDPTrans>::const_iterator const_trans_iterator;
    trans_iterator begin() { return trans.begin(); }
    trans_iterator end() { return trans.end(); }

private:
    vector<flt> w;    ///< Action-level weights r(s,a).
    string label;     ///< Action label.
    vector<HMDPTrans> trans;     ///< Transitions.
};

//-----------------------------------------------------------------------------

/** 
 * @brief A state in the HMDP. 
 */
class HMDPState {
    friend class HMDPReader;
    friend class HMDP;
    friend class HMDPSave;

 private:
    /** 
     * @brief Create a state with a label. 
     * @param lbl The label.
     */
    HMDPState(const string & lbl) {
        label = lbl;
        pred = -1;
        w = 0;
    }

    /** 
     * @brief Default constructor. 
     */
    HMDPState() {
        pred = -1;
        w = 0;
    }

// Add methods --------------
    /** 
     * @brief Add an action to the state.
     * @param w Action weights.
     * @param tails Transition states.
     * @param pr Transition probabilities.
     * @param transW Transition-level weights.
     * @param label Action label.
     */
    void AddAction(vector<flt> & w, vector<idx> & tails, vector<flt> & pr,
                   vector< vector<flt> > & transW, string & label) {
        actions.push_back(HMDPAction(tails,pr,w,transW,label));
    }

    /** 
     * @brief Print the state. 
     * @return String representation.
     */
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
    flt w;          ///< Weight for the state.
    int pred;   ///< Index of predecessor action (negative if not allocated).
};

//-----------------------------------------------------------------------------

/** 
 * @brief HMDP class.
 *
 * Contains a vector of HMDPState objects.
 *
 * Structure:
 *   - The states vector satisfy that 1) states are ordered according to a
 *     valid ordering 2) states are ordered such that they lie constitutively in
 *     memory for a given stage.
 *   - A map stages is used to identify stages. The string of a stage, returns
 *     a pair (first id in states, number of states (size)).
 *   - A HMDPState contains a vector of HMDPAction objects.
 *   - A HMDPAction contains a vector of HMDPTrans objects which are sorted according to state id.
 *   - A HMDPTrans contains the id of the stage, transition weights (if any) and the transition probability.
 *
 * NOTE when a HMDP is built from binary files the id's to identify states in the
 * binary files will not be the same as the id's in states. After the HMDP
 * is built it is not a good idea to add new states since this will invalidate
 * the properties of the states vector.
 */
class HMDP
{
 public:
    friend class HMDPReader;
    friend class HMDPSave;

    /** 
     * @brief Bellman operator used by the specialized dynamic programming routines.
     *
     * The operator is dispatched once before entering the state/action/transition
     * loops. This avoids virtual calls, function objects, and per-transition
     * switches in the hot path.
     */
    enum class BellmanOp {
        Discounted = 0,        ///< Discounted expected weight.
        Average = 1,           ///< Long-run average expected weight.
        Expected = 2,          ///< Total expected weight.
        TransPr = 3,           ///< Transition probability Bellman operator.
        DiscountedTransPr = 4, ///< Discounted transition probability Bellman operator.
        Min = 5,               ///< Inner minimum over feasible successor states.
        Max = 6,               ///< Inner maximum over feasible successor states.
        SecondMoment = 7,      ///< Second moment of total weight.
        Variance = 8           ///< Variance of total accumulated weight under a fixed policy.
    };

    /**
     * @brief Optimization direction.
     *
     * BellmanOp describes the value recursion; OptSense controls whether policy
     * improvement chooses the largest or smallest Bellman value.
     */
    enum class OptSense {
        Maximize, ///< Choose the action with largest Bellman value.
        Minimize  ///< Choose the action with smallest Bellman value.
    };

    /**
     * @brief Storage level of the optimized weight.
     *
     * Action weights are stored on HMDPAction and represent r(s,a).
     * Transition weights are stored on HMDPTrans and represent r(s,a,s').
     */
    enum class WeightLevel {
        Action,     ///< Action-level weight r(s,a).
        Transition  ///< Transition-level weight r(s,a,s').
    };

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



    /**
     * @brief Create an empty HMDP.
     * @param verbose_ Verbose output.
     */
    HMDP(bool verbose_)
    {
        verbose = verbose_;
        okay = true;
        externalProc = false;
        levels = 0;
        timeHorizon = 0;
    }

    /**
     * @brief Create an empty HMDP.
     */
    HMDP()
    {
        verbose = false;
        okay = true;
        externalProc = false;
        levels = 0;
        timeHorizon = 0;
    }

    /**
     * @brief Create a HMDP from binary files.
     * @param binNames Vector of binary filenames.
     * @param verbose_ Verbose output.
     */
    HMDP(vector<string> binNames, bool verbose_)
    {
        verbose = verbose_;
        string transWFile = binNames.size()>8 ? binNames[8] : "";
        string transWLblFile = binNames.size()>9 ? binNames[9] : "";
        LoadBin(binNames[0], binNames[1], binNames[2], binNames[3],
                binNames[4],  binNames[5], binNames[6], binNames[7],
                transWFile, transWLblFile);
    }


    /**
     * @brief Create a HMDP from binary files using the default names and a prefix.
     * @param prefix Filename prefix.
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
        string transWFile = prefix + "transWeight.bin";
        string transWLblFile = prefix + "transWeightLbl.bin";
        LoadBin(stateIdxFile, stateIdxLblFile, actionIdxFile, actionIdxLblFile,
                actionWFile,  actionWLblFile, transProbFile, externalFile,
                transWFile, transWLblFile);
    }

    //~HMDP() {cout << "Deconstructor called." << endl;}

    /**
     * @brief Load HMDP from binary files.
     * @param stateIdxFile State index file.
     * @param stateIdxLblFile State label file.
     * @param actionIdxFile Action index file.
     * @param actionIdxLblFile Action label file.
     * @param actionWFile Action weight file.
     * @param actionWLblFile Action weight label file.
     * @param transProbFile Transition probability file.
     * @param externalFile External processes file.
     * @param transWFile Transition weight file.
     * @param transWLblFile Transition weight label file.
     */
    void LoadBin(string stateIdxFile, string stateIdxLblFile, string actionIdxFile,
        string actionIdxLblFile, string actionWFile,  string actionWLblFile,
        string transProbFile, string externalFile,
        string transWFile = "", string transWLblFile = "");


    /**
     * @brief Check the HMDP for errors.
     *
     * The following are checked:
     * - Probabilities sum to one.
     * - That all transitions are to states which exists.
     *
     * @param eps Maximum allowed difference from one for probabilities sum.
     * @return 0 if okay, 1 if warning, 2 if error.
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

    /**
     * @brief Save the HMDP to binary files.
     * @param prefix Prefix of the binary files.
     */
    void Save2Binary(string prefix);


    /**
     * @brief Add the stage label of each external process to the states/nodes as its label.
     */
    void ExternalAddStageStr();


    /**
     * @brief Set external process states corresponding to the first stage in the external process to -INF.
     */
    void ExternalResetStates();


    /**
     * @brief Set the weight, duration and transition probability of external process actions to zero.
     * @param idxW Index of the weight used.
     * @param idxD Index of the duration.
     */
    void ExternalResetActions(const idx & idxW, const idx & idxD);


    /**
     * @brief Update external process states corresponding to the first stage in the external process.
     * @param op Bellman operator.
     * @param sense Optimization sense.
     * @param iteS State iterator to state in external stage.
     * @param curPrefix The prefix of the current external process in memory.
     * @param pExt Pointer to the current external process.
     * @param idxW Index of the weight used.
     * @param idxD Index of duration.
     * @param g Current average weight.
     * @param discountF The discount factor for one time unit.
     * @return True if a new policy of the external process is found.
     */
    bool ExternalStatesUpdate(BellmanOp op, OptSense sense, state_iterator iteS, string & curPrefix, HMDPPtr & pExt,
        const idx & idxW, const idx & idxD, const flt & g, const flt & discountF);


    /**
     * @brief Copy values between the HMDP and the external process.
     * @param stage Stage string of the HMDP.
     * @param stageExt Stage string of the external HMDP.
     * @param pExt Pointer to the current external process.
     * @param toExt True if move values to the external process (false if move from).
     */
    void ExternalCopyWState(string stage, string stageExt, const HMDPPtr & pExt, const bool toExt);


    /**
     * @brief Return true if the state is an external process state corresponding to the first stage in an external process.
     * @param ite State iterator to state.
     * @return True if external.
     */
    bool ExternalState(state_iterator ite) {
        if (externalProc) {
            if (ite->actions.size()==1) {    // could be a node in an external process not calculated yet
                if (external.count(ite->label)>0) return true;
            }
        }
        return false;
    }


    /**
     * @brief Allocate memory for the external process (check if not already allocated).
     * @param pExt Pointer to the current external process.
     * @param prefix Prefix of the external process.
     * @param curPrefix The prefix of the current external process in memory.
     */
    void ExternalAllocteMem(HMDPPtr & pExt, const string & prefix, string & curPrefix);


    /**
     * @brief Set the values of the external actions to the weight, duration and transition probability of the external process.
     * @param stageStr Stage string of states corresponding to the first stage in the external process.
     * @param pExt Pointer to the current external process.
     * @param idxW Index of the weight used.
     * @param idxD Index of duration.
     * @return True if the values have changed (indicate that the policy has changed).
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

    /**
     * @brief Set number of weights stored in actions (and their names).
     * @param names The names.
     */
    void SetActionWeightNames(const vector<string> & names) {
        weightActionNames = names;
        weightNames = names;
    }

    /**
     * @brief Set number of weights stored in transitions (and their names).
     * @param names The names.
     */
    void SetTransWeightNames(const vector<string> & names) {
        weightTransNames = names;
        weightNames = weightActionNames;
        weightNames.insert(weightNames.end(), weightTransNames.begin(), weightTransNames.end());
    }


    /**
     * @brief Set the action id of the predecessor action.
     * @param id The id.
     */
    void SetPred(int id) {
        for (idx i=0; i<states.size(); ++i) {
            if (states[i].actions.size()>0) states[i].pred = id;
            else states[i].pred = -1;   // states with no actions
        }
    }

    /**
     * @brief Set the weights of all states.
     * @param val Value.
     */
    void SetAllStateW(flt & val) {
        for (idx i=0; i<states.size(); ++i) states[i].w = val;
    }


    /**
     * @brief Set the weights of specified states.
     * @param iS Indices of states.
     * @param val Value.
     */
    void SetStateW(vector<idx> & iS, flt val) {
        for (idx i=0; i<iS.size(); ++i) states[iS[i]].w = val;
    }


    /**
     * @brief Set the weights of all states in a stage.
     * @param stageStr Stage string.
     * @param val Value.
     */
    void SetStateWStage(string stageStr, flt val) {
        pair<idx,idx> pS = stages[stageStr];
        idx iS = pS.first;
        for (idx i=0; i<pS.second; ++i, ++iS) states[iS].w = val;
    }

    /**
     * @brief Set terminal state weights on the last founder stage.
     * @param values Terminal weights.
     * @throw runtime_error If the vector length does not match the last-stage size.
     */
    void SetTerminalW(vector<flt> values) {
        string stageLastStr = GetLastStageStr();
        if (values.size()!=GetStateSize(stageLastStr)) throw runtime_error("Terminal values vector length does not match the last-stage state count.");
        vector<flt>::iterator iteV;
        state_iterator iteS;
        for (iteS = state_begin(stageLastStr), iteV=values.begin(); iteS!=state_end(stageLastStr); ++iteS, ++iteV) {
            w(iteS) = *iteV;
        }
    }


    /**
     * @brief Set the action weight.
     * @param w The weight to set.
     * @param iS The index of the state.
     * @param iA The index of the action.
     * @param iW The weight index.
     */
    void SetActionW(const flt & w, const idx & iS, const idx & iA, const idx & iW) {
        CheckActionWIdx(iW);
        states[iS].actions[iA].w[iW] = w;
    }


    /**
     * @brief Set all the transition probabilities to zero for an action.
     * @param iS Id of the state.
     * @param iA Id of the action.
     */
    void SetActionPrZero(const idx & iS, const idx & iA) {
        states[iS].actions[iA].SetAllTransPr(0);
    }


    /**
     * @brief Set the values in vector r to the weights of the stage.
     * @note Vector r must have length |S| where |S| is the number of states.
     * @param r The vector.
     * @param stageStr The stage string.
     */
    void SetMatrixVal(arma::vec &r, string stageStr) {
        idx i;
        state_iterator iteS;
        for (iteS = state_begin(stageStr), i=0; iteS!=state_end(stageStr); ++iteS, ++i) {
            r(i) = w(iteS);
        }
    }


    /**
     * @brief Set the value of a transition probability.
     * @param pr The transition probability.
     * @param iS Id of the state.
     * @param iA Id of the action.
     * @param iSTail Id of the tail state.
     * @return The old transition probability.
     */
    flt SetGetActionPr(const flt & pr, const idx & iS, const idx & iA, const idx & iSTail);

    /**
     * @brief Set the action of the policy.
     * @param iS Vector of state indices.
     * @param iA Vector of action indices.
     */
    void SetPolicy(vector<idx> iS, vector<idx> iA) {
        for (idx i=0; i<iS.size(); ++i)
            states[iS[i]].pred = iA[i];
    }

// Print functions -------------------

    /**
     * @brief Print the HMDP (states and actions).
     * @return String representation.
     */
    string Print();


    /**
     * @brief Calculate the steady state probabilities for the founder chain.
     * 
     * Assumes infinite time-horizon and ergodic chain.
     *
     * @return A vector with the probabilities.
     */
    vector<flt> CalcSteadyStatePr();



//    /** Find the h(arc) corresponding to an action.
//     * \param iS The index of the state we consider in \code states.
//     * \param idxA The action index of the state we consider
//     * \return An integer of the index in the state-expanded hypergraph
//     * stored as pred (negative if arc, positive if harc). If zero then not found.
//     */
//    int FindAction(idx iS, idx idxA);




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



    /**
     * @brief Calculate the weights of the founder states given a specific policy.
     * @note Modifies the weights stored in the states of the HMDP.
     * @param op Bellman operator.
     * @param w Column matrix storing the calculated weights.
     * @param idxW The weight index.
     * @param g The average weight.
     * @param idxD The duration index.
     * @param discountF The discount factor.
     */
    void FounderW(BellmanOp op, arma::vec &w, const idx &idxW, flt g = 0, idx idxD = 0, flt discountF = 1)
    {
        //cout << "FounderW: idxW=" << idxW << " idxD=" << idxD << endl;
        SetStateWStage("1",0);
        CalcPolicy(op, idxW, g , idxD, discountF);
        SetMatrixVal(w,"0");
    }


    /**
     * @brief Calculate the transition probabilities of the founder states given a specific policy.
     * @note Modifies the state weights.
     * @param op Bellman operator.
     * @param P The transition probability matrix.
     * @param idxD The duration index.
     * @param discountF The discount factor.
     */
    void FounderPr(BellmanOp op, arma::mat &P, idx idxD = 0, flt discountF = 1) {
        idx r,c;
        state_iterator iteS, iteZero;
        SetStateWStage("1", 0);
        for (iteS = state_begin("1"), c=0; iteS!=state_end("1"); ++iteS, ++c) {
            w(iteS) = 1;
            if (c>0) w(iteS-1) = 0; // restore previous
            CalcPolicy(op,0,0,idxD,discountF);
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

    /**
     * @brief Get stage string of state index vector.
     * @param iState State index vector.
     * @return The stage string.
     */
    string GetStageStr(vector<idx> & iState) {
        string str;
        idx size = iState.size();
        for(idx i=0; i<size-1; i++) {
            if (i<size-2) str.append(ToString(iState[i])+",");
            else str.append(ToString(iState[i]));
        }
        return str;
    }


    /**
     * @brief Get stage string of state string.
     * @param stateStr The state string.
     * @return The stage string.
     */
    string GetStageStr(string stateStr) {
        idx pos = stateStr.find_last_of(",");
        return stateStr.substr(0,pos);
    }


    /**
     * @brief Get state string of state index vector.
     * @param iState State index vector.
     * @return The state string.
     */
    string GetStateStr(vector<idx> & iState) {
        string str;
        idx size = iState.size();
        for(idx i=0; i<size; i++) {
            if (i<size-1) str.append(ToString(iState[i])+",");
            else str.append(ToString(iState[i]));
        }
        return str;
    }


    /**
     * @brief Get state string of state id.
     * @param sId State id.
     * @return The state string.
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

    /**
     * @brief Get state strings of state ids.
     * @param sId Vector of state ids.
     * @return Vector of state strings.
     */
    vector<string> GetStatesStr(vector<idx> & sId) {
        vector<string> v;
        for(idx i=0; i<sId.size(); i++) {
            v.push_back(GetStateStr(sId[i]));
        }
        return v;
    }


    /**
     * @brief Get next stage string of state index vector.
     * @param iState State index vector.
     * @return The next stage string.
     */
    string GetNextStageStr(vector<idx> & iState) {
        string str;
        idx size = iState.size();
        for(idx i=0; i<size-1; i++) {
            if (i<size-2) str.append(ToString(iState[i])+",");
            else str.append(ToString(iState[i]+1));    // increase by one
        }
        return str;
    }


    /**
     * @brief Return the string of the next stage at the current level.
     * @param curStageStr The string of the current stage.
     * @return The next stage string.
     */
    string GetNextStageStr(string curStageStr) {
        uSInt found = curStageStr.find_last_of(",");
        int nextStage = atoi(curStageStr.substr(found+1).c_str()) + 1;
        return curStageStr.substr(0,found+1) + ToString<int>(nextStage);
    }


    /**
     * @brief Get next father stage string of state index vector.
     * @param iState State index vector.
     * @return The next father stage string.
     */
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


    /**
     * @brief Get next child stage string of state index vector and action index.
     * @param iState State index vector.
     * @param iAction Action index.
     * @return The next child stage string.
     */
    string GetNextChildStageStr(vector<idx> & iState, idx & iAction) {
        string str = GetStateStr(iState);
        str.append(","+ToString(iAction)+",0");
        return str;
    }


    /**
     * @brief Get the last stage string.
     * @return The last stage string.
     */
    string GetLastStageStr() {
        if (timeHorizon>=INFINT) return "1";
        else return ToString(timeHorizon-1);
    }


    /**
     * @brief Return which level the state is on.
     * @param iState State index vector.
     * @return The level (starting from zero).
     */
    int GetLevel(vector<idx> & iState) {
        return (iState.size()-2)/3;
    }


    /**
     * @brief Get the content of the log.
     * @return The log string.
     */
    string GetLog() {return log.str();}


    /**
     * @brief Get id of state.
     * @param stateStr State string.
     * @return The state id.
     */
    idx GetId(string stateStr) {
        string stageStr = GetStageStr(stateStr);
        idx pos = stateStr.find_last_of(",");
        idx idxS;
        from_string<idx>(idxS,stateStr.substr(pos+1), std::dec);
        pair<idx,idx> sP = stages[stageStr];
        //cout << "stageStr:" << stageStr << " idxS:" << idxS << endl;
        idx iS = sP.first + idxS;
        return iS;
    }


    /**
     * @brief Get id of states in a stage.
     * @param stageStr Stage string.
     * @return Vector of state ids.
     */
    vector<idx> GetIds(string stageStr) {
        vector<idx> v;
        pair<idx,idx> sP = stages[stageStr];
        idx iS = sP.first;
        for (idx i=0; i<sP.second; ++i, ++iS) {
            v.push_back(iS);
        }
        return v;
    }


    /**
     * @brief Return the labels of the states.
     * @param iS Vector of state indices.
     * @return Vector of labels.
     */
    vector<string> GetStateLabel(vector<idx> iS) {
        vector<string> val;
        for (idx i=0; i<iS.size(); ++i) {
            state_iterator iteS = GetIte(iS[i]);
            val.push_back( iteS->label );
        }
        return val;
    }


    /**
     * @brief Return the state weights of a given stage.
     * @param stageStr Stage string.
     * @return Vector of weights.
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


    /**
     * @brief Number of actions for a state.
     * @param ite State iterator.
     * @return Number of actions.
     */
    idx GetActionSize(state_iterator ite) {return ite->actions.size();}

    /**
     * @brief Number of states in a stage.
     * @param stageStr Stage string.
     * @return Number of states.
     */
    idx GetStateSize(string stageStr) {return stages[stageStr].second;}

    /**
     * @brief Total number of states.
     * @return Total number of states.
     */
    idx GetStateSize() {return states.size();}

    /**
     * @brief Total number of actions.
     * @return Total number of actions.
     */
    idx GetActionSize() {
        idx size = 0;
        for (state_iterator iteS = state_begin(); iteS!=state_end(); iteS++)
            size += iteS->actions.size();
        return size;
    }

    /**
     * @brief Get action weight name.
     * @param iW Weight index.
     * @return The weight name.
     */
    string GetWName(idx iW) {
        if (IsActionWIdx(iW)) return weightActionNames[iW];
        if (IsTransWIdx(iW)) return weightTransNames[TransWIdx(iW)];
        throw runtime_error("Global weight index out of range.");
    }

    /** @brief Get all action weight names. */
    vector<string> GetActionWNames() {return weightActionNames;}

    /** @brief Get all transition weight names. */
    vector<string> GetTransWNames() {return weightTransNames;}

    /**
     * @brief Id of state.
     * @param iteS State iterator.
     * @return State id.
     */
    idx GetId(state_iterator iteS) {return iteS - states.begin();}

    /**
     * @brief Iterator of a state.
     * @param iS State index.
     * @return State iterator.
     */
    state_iterator GetIte(idx iS) {return states.begin() + iS;}

    /** Iterator of an action. */
    action_iterator GetIte(state_iterator iteS, idx iA) {return action_begin(iteS) + iA;}

    /** Index of action */
    idx GetIdx(state_iterator iteS, action_iterator iteA) {return iteA - iteS->actions.begin();}


    /**
     * @brief Return the state weight.
     * @param iS Vector of state indices.
     * @return Vector of weights.
     */
    vector<flt> GetPolicyW(vector<idx> iS) {
        vector<flt> val;
        for (idx i=0; i<iS.size(); ++i) {
            state_iterator iteS = GetIte(iS[i]);
            val.push_back( w(iteS) );
        }
        return val;
    }

    /**
     * @brief Return the state weight for a stage.
     * @param stageStr Stage string.
     * @return Vector of weights.
     */
    vector<flt> GetPolicyWStage(string stageStr) {
        vector<idx> iS = GetIds(stageStr);
        return GetPolicyW(iS);
    }


    /**
     * @brief Return the index of the actions of current policy.
     * @param iS Vector of state indices.
     * @return Vector of action indices.
     */
    vector<int> GetPolicy(vector<idx> iS) {
        vector<int> val;
        for (idx i=0; i<iS.size(); ++i) {
            state_iterator iteS = GetIte(iS[i]);
            val.push_back( pred(iteS) );
        }
        return val;
    }


    /**
     * @brief Return the index of the actions of current policy in a stage.
     * @param stageStr Stage string.
     * @return Vector of action indices.
     */
    vector<int> GetPolicyStage(string stageStr) {
        vector<idx> iS = GetIds(stageStr);
        return GetPolicy(iS);
    }


    /**
     * @brief Return the labels of the actions of current policy.
     * @param iS Vector of state indices.
     * @return Vector of action labels.
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

    /**
     * @brief Return the external processes info.
     * @return Vector of strings in format (stageStr, external proc prefix, ...).
     */
    vector<string> GetExternalInfo() {
        vector<string> val;
        map<string,string>::iterator it;
        for (it=external.begin(); it!=external.end(); ++it) {
            val.push_back(it->first);
            val.push_back(it->second);
        }
        return val;
    }


// Accessors (get/set functions for the algorithms, return by reference)

    /** @brief Reference to state weight. */
    flt & w(state_iterator iteS) {return iteS->w;}

    /** @brief Reference to action weight. */
    flt & w(action_iterator iteA, idx iW) {CheckActionWIdx(iW); return iteA->w[iW];}

    /** @brief Reference to action weight for a state. */
    flt & w(state_iterator iteS, idx iA, idx iW) {CheckActionWIdx(iW); return iteS->actions[iA].w[iW];}

    /** @brief Reference to transition-level weight. */
    flt & transW(trans_iterator iteT, idx iW) {CheckTransWIdx(iW); return iteT->w[iW];}

    /** @brief Reference to transition probability. */
    flt & pr(trans_iterator iteT) {return iteT->pr;}

    /** @brief Reference to predecessor action index. */
    int & pred(state_iterator iteS) {return iteS->pred;}

    /** @brief Reference to state label. */
    string & label(state_iterator iteS) {return iteS->label;}





    /** @brief Reset log. */
    void ResetLog() {log.str("");}


// Algorithms -----------------------------------------------------------------

    /**
     * @brief Calculates the optimal policy of a single stage of the founder (a hypertree).
     *
     * The policy is defined in pred and weights w are calculated in each node.
     * Weights in states with no actions must have been set.
     *
     * @param op Bellman operator.
     * @param sense Optimization direction used for policy improvement.
     * @param idxW The action weight index we want to optimize.
     * @param g The average weight (only used when op is BellmanOp::Average).
     * @param idxDur The action duration index.
     * @param discountF The discount factor for one time unit.
     * @return True if a new policy is found.
     */
    bool CalcOptPolicy(BellmanOp op, OptSense sense, idx idxW = 0, flt g = 0, idx idxDur = 0, flt discountF = 1);

    /** Calculates the optimal policy using a specialized Bellman operator.
     *
     * This entry point dispatches once on \p op and \p level, then calls a
     * specialized implementation with tight transition loops. No operator or
     * weight-level checks are performed inside the transition loops.
     *
     * \param op Bellman operator to apply.
     * \param level Whether \p idxW refers to action-level or transition-level weights.
     * \param idxW Local weight index at the selected \p level.
     * \param g Average weight, used by average expected-weight operators.
     * \param idxDur Action-level duration index used by discounted and average operators.
     * \param discountF Discount factor for one time unit.
     *
     * \return True if a new policy is found.
     *
     * \throw runtime_error If the operator/level combination is not implemented
     * or requested weight values are missing.
     */
    bool CalcOptPolicy(BellmanOp op, OptSense sense, WeightLevel level, idx idxW = 0, flt g = 0, idx idxDur = 0, flt discountF = 1);


    /**
     * @brief Calculates state weights based on the current policy of a single stage of the founder.
     *
     * The policy is defined in pred and weights w are calculated in each node.
     * Weights in states with no actions must have been set.
     *
     * @param op Bellman operator.
     * @param idxW The action weight index we want to optimize.
     * @param g The average weight (only used when op is BellmanOp::Average).
     * @param idxDur The action duration index.
     * @param discountF The discount factor for one time unit.
     */
    void CalcPolicy(BellmanOp op, idx idxW = 0, flt g = 0, idx idxDur = 0, flt discountF = 1);

    /** Calculates state weights under the current policy using a specialized Bellman operator.
     *
     * The method mirrors \code CalcOptPolicy(BellmanOp, WeightLevel, ...), but
     * evaluates the already stored policy in \code pred rather than optimizing
     * over all actions.
     *
     * \param op Bellman operator to apply.
     * \param level Whether \p idxW refers to action-level or transition-level weights.
     * \param idxW Local weight index at the selected \p level.
     * \param g Average weight, used by average expected-weight operators.
     * \param idxDur Action-level duration index used by discounted and average operators.
     * \param discountF Discount factor for one time unit.
     *
     * \throw runtime_error If the operator/level combination is not implemented
     * or requested weight values are missing.
     */
    void CalcPolicy(BellmanOp op, WeightLevel level, idx idxW = 0, flt g = 0, idx idxDur = 0, flt discountF = 1);



    /**
     * @brief Calculate retention payoff (RPO) for a state.
     * 
     * Normally run after an optimal policy has been found.
     *
     * @param op Bellman operator.
     * @param sense Optimization direction.
     * @param iS The id of the state we consider in states.
     * @param idxW The index of weights to calculate.
     * @param idxA The action index we calculate the RPO with respect to (same size as iS).
     * @param g The average weight (only used when op is BellmanOp::Average).
     * @param idxDur The action duration index.
     * @param discountF The discount factor for one time unit.
     * @return A vector of the same size as the states containing the RPO values.
     */
    vector<flt> CalcRPO(BellmanOp op, OptSense sense, vector<idx> & iS, idx idxW, vector<idx> & idxA, flt g = 0, idx idxDur = 0, flt discountF = 1);


    /**
     * @brief Policy iteration algorithm (infinite time-horizon).
     *
     * @param op Bellman operator.
     * @param sense Optimization direction.
     * @param maxIte The max number of iterations. The model may loop if not unichain.
     * @param idxW Index of the weight used as nominator.
     * @param idxD The denominator we want to optimize the weight over.
     * @param discountF The discount factor for one time unit.
     * @return The gain g.
     */
    flt PolicyIte(BellmanOp op, OptSense sense, uSInt maxIte, const idx idxW, const idx idxD, const flt discountF = 1);


    /** Policy iteration algorithm (infinite time-horizon) given a fixed policy.
     * \param op Bellman operator.
     * \param maxIte The max number of iterations. The model may loop if not unichain.
     * \param idxW Index of the weight used as nominator.
     * \param idxD The denominator we want to maximize the weight over.
     * \param discountF The discount factor for one time unit.
     *
     * \return g The gain.
     * \post Use \code GetLog to see the optimization log.
     */
    flt PolicyIteFixedPolicy(BellmanOp op, const idx idxW, const idx idxD, const flt discountF = 1);

     /** Value iteration algorithm.
     *
     * \param op Bellman operator.
     * \param maxIte The max number of iterations.
     * \param epsilon If max(w(t)-w(t+1))<epsilon then stop the algorithm, i.e
     *        the policy becomes epsilon optimal (see Puterman p161).
     * \param idxW Index of the weight used.
     * \param idxDur Index of duration such that discount rates can be calculated.
     * \param termValues Terminal values used at founder level.
     * \param g The average gain.
     * \param discountF The discount factor for one time unit.
     *
     * \post Use \code GetLog to see the optimization log.
     */
    void ValueIte(BellmanOp op, OptSense sense, idx maxIte, flt epsilon, const idx idxW,
     const idx idxDur, vector<flt> & termValues,
     const flt g, const flt discountF);


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
    vector<string> weightNames;     ///< Backward compatible concatenation of action and transition weight names.
    vector<string> weightActionNames; ///< Names of action-level weights r(s,a).
    vector<string> weightTransNames;  ///< Names of transition-level weights r(s,a,s').
    map< string, pair<idx,idx> > stages;   ///< Ordered map of stages. The pair contains (state id to first stage in stages, total number of states at stage).
    vector<HMDPState> states;
    map<string, string> external;     ///< Store the external processes in format <stageIdx, prefix>
    bool okay;                      ///< True if reading was okay. Later used to check if an algorithm works okay.
    bool verbose;                   ///< More output to the log.
    bool externalProc;              ///< True if the HMDP use external processes
    ostringstream log;              ///< Stream to store log messages.
private:
    Timer timer;

    /** Return true if \p iW is an action-level weight index. */
    bool IsActionWIdx(idx iW) const {return iW < weightActionNames.size();}

    /** Return true if \p iW is a global transition-level weight index. */
    bool IsTransWIdx(idx iW) const {return iW >= weightActionNames.size() && iW < weightActionNames.size() + weightTransNames.size();}

    /** Convert a global transition weight index to its local transition index. */
    idx TransWIdx(idx iW) const {return iW - weightActionNames.size();}

    /** Human-readable Bellman operator name for diagnostics. */
    string BellmanOpName(BellmanOp op) const;

    /** Human-readable optimization sense name for diagnostics. */
    string OptSenseName(OptSense sense) const;

    /** Throw when a global weight index is invalid or unsupported for \p op. */
    WeightLevel ValidateGlobalWeightForOp(BellmanOp op, idx iW) const;

    /** Throw if \p iW is not a valid action-level weight index. */
    void CheckActionWIdx(idx iW) const {
        if (iW >= weightActionNames.size()) throw runtime_error("Action weight index out of range.");
    }

    /** Throw if \p iW is not a valid transition-level weight index. */
    void CheckTransWIdx(idx iW) const {
        if (iW >= weightTransNames.size()) throw runtime_error("Transition weight index out of range.");
    }

    /** Infer whether a backward-compatible global weight index is action or transition level. */
    WeightLevel WeightLevelFromGlobalIdx(idx iW) const {
        if (IsActionWIdx(iW)) return WeightLevel::Action;
        if (IsTransWIdx(iW)) return WeightLevel::Transition;
        throw runtime_error("Global weight index out of range.");
    }

    /** Validate and return the local weight index for a given weight level. */
    idx LocalWeightIdx(WeightLevel level, idx iW) const {
        if (level==WeightLevel::Action) {
            CheckActionWIdx(iW);
            return iW;
        }
        if (!IsTransWIdx(iW)) throw runtime_error("Transition weight index out of range.");
        return TransWIdx(iW);
    }

    /** Validate that all actions contain action weight \p idxW. */
    void CheckActionWeightsAvailable(idx idxW) const;

    /** Validate that all transitions contain transition weight \p idxW. */
    void CheckTransitionWeightsAvailable(idx idxW) const;

    /**
     * @brief Calculate expected total action weight under the current policy.
     * @param idxW The action weight index.
     * @param mean Vector storing expected total weights by state.
     */
    void CalcPolicyActionMean(idx idxW, vector<flt> &mean);

    /**
     * @brief Calculate expected total transition weight under the current policy.
     * @param idxW The transition weight index.
     * @param mean Vector storing expected total weights by state.
     */
    void CalcPolicyTransitionMean(idx idxW, vector<flt> &mean);

    /**
     * @brief Optimize a policy under the second-moment Bellman operator.
     * @param op Bellman operator, expected to be BellmanOp::SecondMoment.
     * @param sense Optimization direction.
     * @param level Weight storage level.
     * @param idxW Local weight index.
     * @param mean Expected total weights by state, updated for selected actions.
     * @return True if a new policy is found.
     */
    bool CalcOptPolicySecondMoment(BellmanOp op, OptSense sense, WeightLevel level, idx idxW, vector<flt> &mean);

    /**
     * @brief Calculate RPO using action weights r(s,a).
     * @param iS Vector of state indices.
     * @param idxW The weight index.
     * @param idxA Vector of action indices.
     * @return Vector of RPO values.
     */
    vector<flt> CalcRPOActionExpectedMax(vector<idx> & iS, idx idxW, vector<idx> & idxA);

    /**
     * @brief Calculate RPO using action weights r(s,a) by minimization.
     * @param iS Vector of state indices.
     * @param idxW The weight index.
     * @param idxA Vector of action indices.
     * @return Vector of RPO values.
     */
    vector<flt> CalcRPOActionExpectedMin(vector<idx> & iS, idx idxW, vector<idx> & idxA);

    /**
     * @brief Calculate RPO using transition weights r(s,a,s').
     * @param iS Vector of state indices.
     * @param idxW The weight index.
     * @param idxA Vector of action indices.
     * @return Vector of RPO values.
     */
    vector<flt> CalcRPOTransitionExpectedMax(vector<idx> & iS, idx idxW, vector<idx> & idxA);

    /**
     * @brief Calculate RPO using transition weights r(s,a,s') by minimization.
     * @param iS Vector of state indices.
     * @param idxW The weight index.
     * @param idxA Vector of action indices.
     * @return Vector of RPO values.
     */
    vector<flt> CalcRPOTransitionExpectedMin(vector<idx> & iS, idx idxW, vector<idx> & idxA);

    /**
     * @brief Calculate RPO using action-level average weights.
     * @param iS Vector of state indices.
     * @param idxW The weight index.
     * @param idxA Vector of action indices.
     * @param g The average weight.
     * @param idxDur The duration index.
     * @return Vector of RPO values.
     */
    vector<flt> CalcRPOActionAverageMax(vector<idx> & iS, idx idxW, vector<idx> & idxA, flt g, idx idxDur);

    /**
     * @brief Calculate RPO using action-level average weights by minimization.
     * @param iS Vector of state indices.
     * @param idxW The weight index.
     * @param idxA Vector of action indices.
     * @param g The average weight.
     * @param idxDur The duration index.
     * @return Vector of RPO values.
     */
    vector<flt> CalcRPOActionAverageMin(vector<idx> & iS, idx idxW, vector<idx> & idxA, flt g, idx idxDur);

    /**
     * @brief Calculate RPO using action-level discounted weights.
     * @param iS Vector of state indices.
     * @param idxW The weight index.
     * @param idxA Vector of action indices.
     * @param idxDur The duration index.
     * @param discountF The discount factor.
     * @return Vector of RPO values.
     */
    vector<flt> CalcRPOActionDiscountedMax(vector<idx> & iS, idx idxW, vector<idx> & idxA, idx idxDur, flt discountF);

    /**
     * @brief Calculate RPO using action-level discounted weights by minimization.
     * @param iS Vector of state indices.
     * @param idxW The weight index.
     * @param idxA Vector of action indices.
     * @param idxDur The duration index.
     * @param discountF The discount factor.
     * @return Vector of RPO values.
     */
    vector<flt> CalcRPOActionDiscountedMin(vector<idx> & iS, idx idxW, vector<idx> & idxA, idx idxDur, flt discountF);

    /**
     * @brief Calculate RPO using transition probabilities.
     * @param iS Vector of state indices.
     * @param idxA Vector of action indices.
     * @return Vector of RPO values.
     */
    vector<flt> CalcRPOActionTransPrMax(vector<idx> & iS, vector<idx> & idxA);

    /**
     * @brief Calculate RPO using transition probabilities by minimization.
     * @param iS Vector of state indices.
     * @param idxA Vector of action indices.
     * @return Vector of RPO values.
     */
    vector<flt> CalcRPOActionTransPrMin(vector<idx> & iS, vector<idx> & idxA);

    /**
     * @brief Calculate RPO using discounted transition probabilities.
     * @param iS Vector of state indices.
     * @param idxA Vector of action indices.
     * @param idxDur The duration index.
     * @param discountF The discount factor.
     * @return Vector of RPO values.
     */
    vector<flt> CalcRPOActionDiscountedTransPrMax(vector<idx> & iS, vector<idx> & idxA, idx idxDur, flt discountF);

    /**
     * @brief Calculate RPO using discounted transition probabilities by minimization.
     * @param iS Vector of state indices.
     * @param idxA Vector of action indices.
     * @param idxDur The duration index.
     * @param discountF The discount factor.
     * @return Vector of RPO values.
     */
    vector<flt> CalcRPOActionDiscountedTransPrMin(vector<idx> & iS, vector<idx> & idxA, idx idxDur, flt discountF);

    /**
     * @brief Optimize a finite-stage policy using action weights r(s,a).
     * 
     * Implements V(s) = max_a { r(s,a) + sum_s' P(s'|s,a)V(s') }.
     *
     * @param idxW The weight index.
     * @return True if a new policy is found.
     */
    bool CalcOptPolicyActionExpectedMax(idx idxW);

    /**
     * @brief Optimize a finite-stage policy using action weights r(s,a) by minimization.
     * @param idxW The weight index.
     * @return True if a new policy is found.
     */
    bool CalcOptPolicyActionExpectedMin(idx idxW);

    /**
     * @brief Optimize a finite-stage policy using transition weights r(s,a,s').
     * 
     * Implements V(s) = max_a { sum_s' P(s'|s,a) [r(s,a,s') + V(s')] }.
     *
     * @param idxW The weight index.
     * @return True if a new policy is found.
     */
    bool CalcOptPolicyTransitionExpectedMax(idx idxW);

    /**
     * @brief Optimize a finite-stage policy using transition weights r(s,a,s') by minimization.
     * @param idxW The weight index.
     * @return True if a new policy is found.
     */
    bool CalcOptPolicyTransitionExpectedMin(idx idxW);

    /**
     * @brief Optimize second moment using action weights; outer action choice maximizes.
     * @param idxW The weight index.
     * @param mean Expected total weights by state.
     * @return True if a new policy is found.
     */
    bool CalcOptPolicyActionSecondMomentMax(idx idxW, vector<flt> &mean);

    /**
     * @brief Optimize second moment using action weights; outer action choice minimizes.
     * @param idxW The weight index.
     * @param mean Expected total weights by state.
     * @return True if a new policy is found.
     */
    bool CalcOptPolicyActionSecondMomentMin(idx idxW, vector<flt> &mean);

    /**
     * @brief Optimize second moment using transition weights; outer action choice maximizes.
     * @param idxW The weight index.
     * @param mean Expected total weights by state.
     * @return True if a new policy is found.
     */
    bool CalcOptPolicyTransitionSecondMomentMax(idx idxW, vector<flt> &mean);

    /**
     * @brief Optimize second moment using transition weights; outer action choice minimizes.
     * @param idxW The weight index.
     * @param mean Expected total weights by state.
     * @return True if a new policy is found.
     */
    bool CalcOptPolicyTransitionSecondMomentMin(idx idxW, vector<flt> &mean);

    /**
     * @brief Optimize a finite-stage policy using action-level average weights.
     * 
     * Specialized loop for the average weight criterion.
     *
     * @param idxW The weight index.
     * @param g The average weight.
     * @param idxDur The duration index.
     * @return True if a new policy is found.
     */
    bool CalcOptPolicyActionAverageMax(idx idxW, flt g, idx idxDur);

    /**
     * @brief Optimize a finite-stage policy using action-level average weights by minimization.
     * @param idxW The weight index.
     * @param g The average weight.
     * @param idxDur The duration index.
     * @return True if a new policy is found.
     */
    bool CalcOptPolicyActionAverageMin(idx idxW, flt g, idx idxDur);

    /**
     * @brief Optimize a finite-stage policy using action-level discounted weights.
     * 
     * Performs discounting outside the transition loop for performance.
     *
     * @param idxW The weight index.
     * @param idxDur The duration index.
     * @param discountF The discount factor.
     * @return True if a new policy is found.
     */
    bool CalcOptPolicyActionDiscountedMax(idx idxW, idx idxDur, flt discountF);

    /**
     * @brief Optimize a finite-stage policy using action-level discounted weights by minimization.
     * @param idxW The weight index.
     * @param idxDur The duration index.
     * @param discountF The discount factor.
     * @return True if a new policy is found.
     */
    bool CalcOptPolicyActionDiscountedMin(idx idxW, idx idxDur, flt discountF);

    /**
     * @brief Optimize a finite-stage policy using transition probabilities.
     * @return True if a new policy is found.
     */
    bool CalcOptPolicyActionTransPrMax();

    /**
     * @brief Optimize a finite-stage policy using transition probabilities by minimization.
     * @return True if a new policy is found.
     */
    bool CalcOptPolicyActionTransPrMin();

    /**
     * @brief Optimize a finite-stage policy using discounted transition probabilities.
     * @param idxDur The duration index.
     * @param discountF The discount factor.
     * @return True if a new policy is found.
     */
    bool CalcOptPolicyActionDiscountedTransPrMax(idx idxDur, flt discountF);

    /**
     * @brief Optimize a finite-stage policy using discounted transition probabilities by minimization.
     * @param idxDur The duration index.
     * @param discountF The discount factor.
     * @return True if a new policy is found.
     */
    bool CalcOptPolicyActionDiscountedTransPrMin(idx idxDur, flt discountF);

    /**
     * @brief Optimize using action weights and an inner successor minimum; outer action choice maximizes.
     * @param idxW The weight index.
     * @return True if a new policy is found.
     */
    bool CalcOptPolicyActionMinMax(idx idxW);

    /**
     * @brief Optimize using action weights and an inner successor minimum; outer action choice minimizes.
     * @param idxW The weight index.
     * @return True if a new policy is found.
     */
    bool CalcOptPolicyActionMinMin(idx idxW);

    /**
     * @brief Optimize using transition weights and an inner successor minimum; outer action choice maximizes.
     * @param idxW The weight index.
     * @return True if a new policy is found.
     */
    bool CalcOptPolicyTransitionMinMax(idx idxW);

    /**
     * @brief Optimize using transition weights and an inner successor minimum; outer action choice minimizes.
     * @param idxW The weight index.
     * @return True if a new policy is found.
     */
    bool CalcOptPolicyTransitionMinMin(idx idxW);

    /**
     * @brief Optimize using action weights and an inner successor maximum; outer action choice maximizes.
     * @param idxW The weight index.
     * @return True if a new policy is found.
     */
    bool CalcOptPolicyActionMaxMax(idx idxW);

    /**
     * @brief Optimize using action weights and an inner successor maximum; outer action choice minimizes.
     * @param idxW The weight index.
     * @return True if a new policy is found.
     */
    bool CalcOptPolicyActionMaxMin(idx idxW);

    /**
     * @brief Optimize using transition weights and an inner successor maximum; outer action choice maximizes.
     * @param idxW The weight index.
     * @return True if a new policy is found.
     */
    bool CalcOptPolicyTransitionMaxMax(idx idxW);

    /**
     * @brief Optimize using transition weights and an inner successor maximum; outer action choice minimizes.
     * @param idxW The weight index.
     * @return True if a new policy is found.
     */
    bool CalcOptPolicyTransitionMaxMin(idx idxW);

    /**
     * @brief Evaluate the current policy using action weights \f$r(s,a)\f$.
     * @param idxW The weight index.
     */
    void CalcPolicyActionWeight(idx idxW);

    /** 
     * @brief Evaluate the current policy using transition weights \f$r(s,a,s')\f$.
     * @param idxW The weight index.
     */
    void CalcPolicyTransitionWeight(idx idxW);

    /**
     * @brief Evaluate the current policy using the second moment of action weights.
     * @param idxW The weight index.
     */
    void CalcPolicyActionSecondMoment(idx idxW);

    /**
     * @brief Evaluate the current policy using the second moment of transition weights.
     * @param idxW The weight index.
     */
    void CalcPolicyTransitionSecondMoment(idx idxW);

    /**
     * @brief Evaluate the current policy using the variance of action weights.
     * @param idxW The weight index.
     */
    void CalcPolicyActionVariance(idx idxW);

    /**
     * @brief Evaluate the current policy using the variance of transition weights.
     * @param idxW The weight index.
     */
    void CalcPolicyTransitionVariance(idx idxW);

    /**
     * @brief Evaluate the current policy using action-level average weights.
     * @param idxW The weight index.
     * @param g The average weight.
     * @param idxDur The duration index.
     */
    void CalcPolicyActionAverageWeight(idx idxW, flt g, idx idxDur);

    /**
     * @brief Evaluate the current policy using action-level discounted weights.
     * @param idxW The weight index.
     * @param idxDur The duration index.
     * @param discountF The discount factor.
     */
    void CalcPolicyActionDiscountedWeight(idx idxW, idx idxDur, flt discountF);

    /** 
     * @brief Evaluate the current policy using transition probabilities.
     */
    void CalcPolicyActionTransPr();

    /**
     * @brief Evaluate the current policy using discounted transition probabilities.
     * @param idxDur The duration index.
     * @param discountF The discount factor.
     */
    void CalcPolicyActionDiscountedTransPr(idx idxDur, flt discountF);

    /**
     * @brief Evaluate the current policy using action weights and an inner successor minimum.
     * @param idxW The weight index.
     */
    void CalcPolicyMinActionWeight(idx idxW);

    /**
     * @brief Evaluate the current policy using transition weights and an inner successor minimum.
     * @param idxW The weight index.
     */
    void CalcPolicyMinTransitionWeight(idx idxW);

    /**
     * @brief Evaluate the current policy using action weights and an inner successor maximum.
     * @param idxW The weight index.
     */
    void CalcPolicyMaxActionWeight(idx idxW);

    /**
     * @brief Evaluate the current policy using transition weights and an inner successor maximum.
     * @param idxW The weight index.
     */
    void CalcPolicyMaxTransitionWeight(idx idxW);
};

//-----------------------------------------------------------------------------

/**
 * @brief Class for reading/loading HMDP models.
 *
 * The HMDP must be represented using the HMDP binary format (v1.0) which is a
 * collection of 8-10 binary files:
 *
 * - stateIdx.bin: File of integers containing the indexes defining all states.
 * - stateIdxLbl.bin: File of characters containing state labels.
 * - actionIdx.bin: File of integers containing the indexes defining all actions.
 * - actionIdxLbl.bin: File of characters containing action labels.
 * - actionWeight.bin: File of doubles containing the weights of the actions.
 * - actionWeightLbl.bin: File of characters containing the labels of the weights.
 * - transProb.bin: File of doubles containing transition probabilities.
 * - externalProcesses.bin: File of characters containing external process info.
 * - transWeight.bin: File of doubles containing transition-level weights.
 * - transWeightLbl.bin: File of characters containing transition-level weight labels.
 */
class HMDPReader
{
public:
    friend class HMDPBuilder;

    /**
     * @brief Create an empty reader used by in-memory builders.
     */
    HMDPReader();

    /**
     * @brief Set the pointer to the HMDP we want to read data to.
     * @param stateIdxFile Filename of the state index file.
     * @param stateIdxLblFile Filename of the state label file.
     * @param actionIdxFile Filename of the action index file.
     * @param actionIdxLblFile Filename of the action label file.
     * @param actionWFile Filename of the action weight file.
     * @param actionWLblFile Filename of the action weight label file.
     * @param transProbFile Filename of the transition probability file.
     * @param externalFile Filename of the external processes file.
     * @param transWFile Filename of the transition-level weight file.
     * @param transWLblFile Filename of the transition-level weight label file.
     * @param pHMDP Pointer to the HMDP.
     * @param hmdpLog Output stream for logging.
     */
    HMDPReader(string stateIdxFile, string stateIdxLblFile, string actionIdxFile,
        string actionIdxLblFile, string actionWFile, string actionWLblFile,
        string transProbFile, string externalFile, string transWFile, string transWLblFile,
        HMDP *pHMDP, ostringstream & hmdpLog);

private:

    /**
     * @brief Read a binary file of type T into an array.
     * @param file Filename.
     * @param p Pointer to the array.
     * @return The size of the array p.
     */
    template <class T>
    idx ReadBinary(string file, T *&p);


    /**
     * @brief Add the states to the HMDP.
     * @param stateIdxFile Filename of the state index file.
     * @param stateIdxLblFile Filename of the state label file.
     */
    void AddStates(string stateIdxFile, string stateIdxLblFile);


    /**
     * @brief Add the actions to the HMDP.
     * @param actionIdxFile Filename of the action index file.
     * @param actionIdxLblFile Filename of the action label file.
     * @param actionWFile Filename of the action weight file.
     * @param actionWLblFile Filename of the action weight label file.
     * @param transProbFile Filename of the transition probability file.
     * @param transWFile Filename of the transition-level weight file.
     * @param transWLblFile Filename of the transition-level weight label file.
     */
    void AddActions(string actionIdxFile, string actionIdxLblFile,
        string actionWFile, string actionWLblFile, string transProbFile,
        string transWFile = "", string transWLblFile = "");

    /**
     * @brief Add the external processes to the HMDP.
     * @param externalFile Filename of the external processes file.
     */
    void AddExternal(string externalFile);


    /**
     * @brief Compile the HMDP model.
     *
     * Adds dummy states at founder level if infinite time-horizon.
     */
    void Compile();


    /**
     * @brief Find state id of transitions given index and scope.
     *
     * State id is stored in the index vector of a TmpAction.
     *
     * @param iState State index.
     * @param findValidOdr Set to true if a scope 3 transition is found.
     * @return True if all transition targets were resolved.
     */
    bool SetSIds(const idx & iState, bool & findValidOdr);


    /**
     * @brief Find a valid ordering of the states.
     * @param order Vector to store the ordering.
     */
    void FindValidOdr(vector<idx> & order);


    /**
     * @brief Convert a state index vector to a stage string.
     * @param iHMDP State index vector.
     * @return The stage string.
     */
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

    bool foundScp3;       ///< True if found a scope 3 (a valid ordering must be found).
    multimap<string, int> stagesMap;   ///< Multimap to quickly find the different stages (state string -> sId).

    /** @brief Temporary action structure for loading. */
	class TmpAction {
	    public:
        /** @brief Clear the action. */
        void Clear() {
            index.clear(); pr.clear(); scp.clear(); w.clear(); label.clear();
            transW.clear();
        }
        idx sId;
        vector<idx> index;  ///< State indexes.
        vector<flt> pr;   ///< Transition probabilities.
        vector<idx> scp;  ///< Scope of transition.
        vector<flt> w;    ///< Action weights.
        vector< vector<flt> > transW; ///< Transition-level weights.
        string label;     ///< Action label.
	};

    /** @brief Temporary state structure for loading. */
	class TmpState {
	    public:
        vector<idx> iHMDP;
        string label;
        vector<TmpAction> actions;
        vector< pair<idx,idx> > fStar;  ///< (iS,iA) pairs in the forward star.
	};
    vector<TmpState> stateVec;  ///< Vector of all states loaded from files.

    HMDP * pHMDP;         ///< Pointer to the HMDP.
    Timer timer;

};

// -----------------------------------------------------------------------------

/**
 * @brief Streaming builder for HMDP models stored directly in C++ memory.
 *
 * The builder accepts the same flat rows written by the R binary writer, but
 * stores them in the temporary compiler structure instead of binary files.
 */
class HMDPBuilder
{
public:
    /**
     * @brief Create an in-memory HMDP builder.
     * @param verbose_ Verbose output.
     */
    HMDPBuilder(bool verbose_);

    /**
     * @brief Delete unfinished model memory.
     */
    ~HMDPBuilder();

    /**
     * @brief Set action-level weight names.
     * @param labels Weight labels.
     */
    void SetWeights(vector<string> labels);

    /**
     * @brief Set transition-level weight names.
     * @param labels Weight labels.
     */
    void SetTransWeights(vector<string> labels);

    /**
     * @brief Add one state row.
     * @param index Hierarchical state index.
     * @param label State label.
     * @return Temporary state row id.
     */
    idx AddState(vector<idx> index, string label);

    /**
     * @brief Add one action row.
     * @param stateRowId Temporary state row id where the action is defined.
     * @param scope Transition scopes.
     * @param id Transition ids.
     * @param pr Transition probabilities.
     * @param weights Action weights.
     * @param transWeights Flat transition weights.
     * @param label Action label.
     */
    void AddAction(idx stateRowId, vector<idx> scope, vector<idx> id,
        vector<flt> pr, vector<flt> weights, vector<flt> transWeights,
        string label);

    /**
     * @brief Compile and return the finished HMDP.
     * @return Pointer to the finished HMDP.
     */
    HMDP* Close();

    /**
     * @brief Get builder log messages.
     * @return Log text.
     */
    string GetLog();

private:
    HMDP *pHMDP;       ///< Model being built.
    HMDPReader reader; ///< Temporary compiler state.
    bool closed;       ///< True after Close has been called.
    bool released;     ///< True after pHMDP ownership has been returned to R.
};

// -----------------------------------------------------------------------------

/**
 * @brief Class for saving the HMDP in memory to binary files.
 */
class HMDPSave
{
public:

    /**
     * @brief Constructor.
     * @param prefix Prefix used for the binary files.
     * @param pHMDP HMDP model.
     */
    HMDPSave(string prefix, HMDP * pHMDP);

    /**
     * @brief Destructor.
     */
    ~HMDPSave();

private:

    /** @brief Write vector of integers to binary file. */
    void WriteBinary(FILE* pFile, const vector<int> &vec) {
        if (vec.empty()) return;
        fwrite(&vec[0], sizeof(int), vec.size(), pFile);
    }

    /** @brief Write vector of floats to binary file. */
    void WriteBinary(FILE* pFile, const vector<flt> &vec) {
        if (vec.empty()) return;
        fwrite(&vec[0], sizeof(flt), vec.size(), pFile);
    }

    /** @brief Write integer to binary file. */
    void WriteBinary(FILE* pFile, const int i) {
        fwrite(&i, sizeof(int), 1, pFile);
    }

    /** @brief Write float to binary file. */
    void WriteBinary(FILE* pFile, const flt i) {
        fwrite(&i, sizeof(flt), 1, pFile);
    }

    /** @brief Write string to binary file. */
    void WriteBinary(FILE* pFile, const string &str) {
        fwrite(str.c_str(), sizeof(char), str.length()+1, pFile);   // add the null character also
    }

    /** @brief Create and write the binary files. */
    void CreateBinaryFiles();

public:
    ostringstream log;    ///< Logging stream.
private:
    FILE* pStateIdxFile;
    FILE* pStateIdxLblFile;
    FILE* pActionIdxFile;
    FILE* pActionIdxLblFile;
    FILE* pActionWFile;
    FILE* pActionWLblFile;
    FILE* pTransProbFile;
    FILE* pTransWFile;
    FILE* pTransWLblFile;
    FILE* pExternalProcessesFile;

    HMDP * pHMDP;         ///< Pointer to the HMDP.
	int sId; ///< Total number of states.
	int aId; ///< Total number of actions.
	int wLblLth; ///< Number of weight labels.
    Timer timer; ///< CPU measurement timer.
};


#endif // HMDP_H
