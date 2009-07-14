#ifndef MDP_HPP
#define MDP_HPP

#include "hypergf.hh"
#include "htacyclic.hh"
#include "hmdpReader.hh"
#include "matrix.hh"    // simple matrix class and linear equations solver using lapack
#include <vector>
#include <deque>
#include <string>
#include <map>
#include <cmath>
using namespace std;

//-----------------------------------------------------------------------------

/** Action of a state. Consists of a label, weights/quantities, duration and
a set of transitions. A transistion is specified using a triple (i, p, s) where
i specify the state number at the next stage (numbering from zero), p is the
transition probability and s is the scope which is 0 if next stage is in the
current process, -1 next stage is in the father process and 1 if next stage is
in the child process (i.e. stage 0). That is, (3, 0.5, -1) is the transistion
to state 3 of the father process with probability 0.5.
*/
class HMDPAction {
 public:
    friend class HMDPState;     // so can access private members in HMDPAction
    friend class HMDP;          // so can access private members in HMDPAction
    friend class HMDPReader;    // so can access private members in HMDPAction

    HMDPAction() {};


    HMDPAction(string lbl) {
        label=lbl;
    };


    /** Create an action. */
    HMDPAction(vector<idx> &iStates, vector<flt> &trans, vector<int> scpe, string lbl) {
        label = lbl;
        transPr = trans;
        idxStates = iStates;
        scope = scpe;
    }


    /** Add a single transition probability to the action.
     * \param iS Index of the state in the scope.
     * \param pr The probability.
     * \param scope The scope, 4 values possible: 2 - A transition to a child process (stage zero in the child process), 1 - A transition to next stage in the current process, 0 - A transition to the next stage in the father process. Here idx in the pair denote the index of the state at the stage considered, e.g. if scope=1 and idx=2 we consider state number 2+1 at next stage in the current process. Finally, if scope = 3 then a transition to a state specified by it's state id is given. That is, if scope=3 and idx=5 then we have a transition to the state at idx 5 in the state vector.
     */
    void AddTransPr(idx iS, flt pr, int scope) {
        idxStates.push_back(iS);
        transPr.push_back(pr);
        (this->scope).push_back(scope);
    }


    /** Set weights of the action.
     * \param w Vector of weights to be used.
     */
    void SetWeights(const vector<flt> &w) {
        weights = w;
    }


    /** Set weights of the action to zero.
     * \param numb Number of weights.
     */
    void SetWeightsToZero(uInt numb) {
        weights.clear();
        weights.resize(numb,0);
    }


    /** Set the times duration.
     * \param times Time duration for the transition to the new states.
     */
    /*void SetTimes(const vector<flt> &times) {
        time = times;
    }*/


    /** Set the time duration for all states
     * \param t A single time duration for all states.
     */
    /*void SetTime(flt t) {
        time.resize(1,t);
    }*/


    /** Check that probabilities sum to one.
     * \param eps The sum of the probablities must at most differ eps from one.
     * \param log Stream storing log messages.
     * \return Zero if ok, 1 if warning, 2 if error.
     */
    uSInt Check(flt eps, ostringstream & log);


    /** Print the action. */
    void Print();


    /** Set the label */
    void SetLabel(string label) {
        this->label = label;
    }

//private:
    // vectors of same size to store a transition
    vector<idx> idxStates;  ///< State indexes.
    vector<flt> transPr;    ///< Transition probabilities.
    vector<int> scope;      ///< The scope of the index. If 1 next stage in current process, if 0 next stage in father process, if 2 next stage in child process (i.e. stage 0) and if 3 a transition to a state specified by it's state id. That is, if scope=3 and idx=5 then we have a transition to the state[5]..
    // ----
    vector<flt> weights;    ///< Weights/quantities for the action.
    //vector<flt> time;       ///< Time used by action. Either a single number or the same size as transPr.
    // TODO (LRE#1#): Currently time are stored as all other weights, i.e. only 1. dim times possible!!.
    string label;           ///< Action label.
};

//-----------------------------------------------------------------------------

/** A state in the HMDP. */
class HMDPState {
 public:
    friend class HMDP;
    friend class HMDPReader;

    HMDPState() {};

    HMDPState(const vector<idx> &idx, const vector<HMDPAction> &actions,
        const string &lbl) {
        label = lbl;
        idxHMDP = idx;
        tmpActions = actions;
    }


    HMDPState(const vector<idx> &idx, const string &lbl) {
        label.assign(lbl);
        idxHMDP = idx;
    }

    HMDPState(const vector<idx> &idx) {
        idxHMDP = idx;
    }


    /** Stage index as a string, e.g. "0,0,0,3". Always contains 1+2*level
     * numbers.
     */
    string StageStr();


    /** State label e.g. "0,0,0,3,1". Always contains 2+2*level numbers. */
    string StateStr();


    void AddAction(const HMDPAction &action) {
        tmpActions.push_back(action);
        actionLabels.push_back(action.label);
        //cout << "s: " << tmpActions.size() << endl;
    }


    /** Create an action and return a pointer to it. */
    HMDPAction * CreateAction(string label) {
        HMDPAction a(label);
        tmpActions.push_back(a);
        actionLabels.push_back(label);
        return &tmpActions[tmpActions.size()-1];
    }


    void Print() {
        cout << "state: " << StateStr() << " (" << label << ") " << endl;
        //cout << tmpActions.size() << endl;
        for (idx i=0; i<tmpActions.size(); i++) tmpActions[i].Print();
    }


    /** Check if state okay.
     * \param eps The sum of the probablities must at most differ eps from one.
     */
    bool Check(flt eps, ostringstream & log) {
        bool okay = true;
        uSInt msg;

        if ( (idxHMDP.size()-2) % 3 != 0 ) {
            log << "In state [" << StateStr() << "] (" << label << ") the " <<
                "index is not of size 2+3*level!" << endl;
            return false;
        }
        for (idx i=0; i<tmpActions.size(); i++) {
            msg = tmpActions[i].Check(eps,log);
            if (msg>0) {
                log << "  at state [" << StateStr() << "] (" << label << ")" << endl;
                if (msg==2) okay = false;
            }
        }
        return okay;
    }

private:
    /** Remove all actions. */
    void RemoveActions() {
        tmpActions.clear();
    }


    /** Find the max size of the transition probabilities for all actions. */
    uInt MaxTransPrSize() {
        uInt ctr = 0;
        for (idx a=0; a<tmpActions.size(); ++a)
            ctr = max(ctr,(uInt)tmpActions[a].transPr.size());
        return ctr;
    }


    /** Count the number of deterministic actions (actions corresponding to an arc). */
    uInt CountArcActions() {
        uInt ctr = 0;
        for (idx a=0; a<tmpActions.size(); ++a)
            if (tmpActions[a].transPr.size()==1) ++ctr;
        return ctr;
    }


    /** Count the number of non-deterministic actions (actions corresponding to a hyperarc). */
    uInt CountHArcActions() {
        uInt ctr = 0;
        for (idx a=0; a<tmpActions.size(); ++a)
            if (tmpActions[a].transPr.size()>1) ++ctr;
        return ctr;
    }


    /** Count the size of non-deterministic actions (trans. pr. + 1 for each action). */
    uInt CountActionSize() {
        uInt ctr = 0;
        for (idx a=0; a<tmpActions.size(); ++a)
            if (tmpActions[a].transPr.size()>1) ctr += tmpActions[a].transPr.size()+1;
        return ctr;
    }


    /** Next stage label. */
    string NextStageStr();


    /** Next stage label.
     * \param iAction The index of the child action.
     */
    string NextChildStageStr(idx iAction);


    /** Stage label of next father stage, e.g. if state is "0,1,0,1,1" then return "0". */
    string NextFatherStageStr();

public:
    string label;                   ///< State label.
    vector<idx> idxHMDP;            ///< Index of the state in the HMDP tree. Always of size 2+3*level.

    vector<string> actionLabels;    ///< Vector of action labels. The size will be equal the number of actions, i.e. store also empty labels because then the action number can be identified.
private:
    vector<HMDPAction> tmpActions;  ///< Tempoary actions need to build the hypergraph. Removed after the hypergraph built.
};

//-----------------------------------------------------------------------------

/** HMDP class.
Contains an vector \code states of HMDPstate objects and a state-expanded
hypergraph \code H. The vector of HMDPstate objects is used to build the HMDP
and afterwards the state-expanded hypergraph can be built from this vector.
Entry \code i in \code states corresponds to node \code i+1 in \code H.
Currently assume that \code H only contains as single predecessor and one set
of multipliers (only one index for predecessor and multipliers)!
 */
class HMDP
{
public:
    friend class HMDPReader;

    /** Create a HMDP from binary files
     */
    HMDP(string stateIdxFile, string stateIdxLblFile, string actionIdxFile,
        string actionIdxLblFile, string actionWFile,  string actionWLblFile,
        string transProbFile)
    {
        HMDPReader reader(stateIdxFile, stateIdxLblFile, actionIdxFile,
            actionIdxLblFile, actionWFile, actionWLblFile, transProbFile, this);
    }


    /** Create a HMDP with no actions and states.
     */
    HMDP(){};

    /** Create a HMDP with no actions and states.
     * \param levels Number of levels in the HMDP.
     * \param timeHorizon The time-horizon. If infinite use INFINT here.
     * \param rate The interest rate.
     * \param rateBase The time-horizon the rate is valid over.
     * \note Levels are numbered from zero, i.e. we have level <tt>0, ..., levels-1</tt>.
     */
    HMDP(uInt levels, uInt timeHorizon, flt rate, flt rateBase);

    /** Create a HMDP with no actions and states.
     * \param levels Number of levels in the HMDP.
     * \param timeHorizon The time-horizon. If infinite use INFINT here.
     * \note Levels are numbered from zero, i.e. we have level <tt>0, ..., levels-1</tt>.
     */
    HMDP(uInt levels, uInt timeHorizon);

    /** Add a state with no actions defined yet.
     * \param iHMDP The index vector of the HMDP state. Always of size
     * 2+3*level, e.g vector [0,1,0,3,2] says that we consider stage 0,
     * state 1 and action 0 at the founder and stage 3 and state 2 at level one.
     * \param label The label of the state.
     */
    void AddState(const vector<idx> &iHMDP, const string &label);


    /** Add a state with no actions defined yet.
     * \param iHMDP The index vector of the HMDP state. Always of size
     * 2+3*level, e.g vector [0,1,0,3,2] says that we consider stage 0,
     * state 1 and action 0 at the founder and stage 3 and state 2 at level one.
     */
    void AddState(const vector<idx> &iHMDP);


    /** Add a new weight to the HMDP. */
    idx AddWeight(string name) {
        weightNames.push_back(name);
        return weightNames.size()-1;
    }


    /** Print the HMDP, i.e. its states and actions. */
    void Print();


    /** Print the number of states at next level of the father, current and child. */
    void PrintCount();


    /** Build the HMDP after states and actions have been added.
     * \post The state-expanded hypergraph has been created. Note that duration
     * times are stored at index zero of \code w at each (hyper)arc.
     * A log over the build can be seen by calling \code GetLog.
     */
    void BuildHMDP();


    /** Get the content of the log. */
    string GetLog() {
        return log.str();
    }


    /** Value iteration algorithm for discounted expected reward (infinite
     * time-horizon).
     * \param times The max number of times value iteration is performed on an
     * infinite time-horizon HMDP.
     * \param epsilon If max(w(t)-w(t+1))<epsilon then stop the algorithm, i.e
     * the policy becomes epsilon optimal (see Puterman p161).
     * \param idxW Index of the weight used.
     * \param idxDur Index of duration such that discount rates can be calculated.
     * \post Use \code GetLog to see the optimization log.
     */
    void ValueIteInfDiscount(uInt times, flt epsilon, idx idxW, idx idxDur,
        const flt &rate, const flt &rateBase);

    /** Value iteration algorithm for discounted expected reward (infinite
     * time-horizon).
     * \param times The max number of times value iteration is performed on an
     * infinite time-horizon HMDP.
     * \param epsilon If max(w(t)-w(t+1))<epsilon then stop the algorithm, i.e
     * the policy becomes epsilon optimal (see Puterman p161).
     * \param idxW Index of the weight used.
     * \param idxDur Index of duration such that discount rates can be calculated.
     * \param iniValues Initial values used at founder level (the values of the dummy nodes).
     * \post Use \code GetLog to see the optimization log.
     */
    void ValueIteInfDiscount(uInt times, flt epsilon, idx idxW, idx idxDur,
        const flt &rate, const flt &rateBase, vector<flt> & iniValues);


    /** Value iteration algorithm for discounted expected reward (finite
     * time-horizon).
     * \param idxW Index of the weight used.
     * \param idxDur Index of duration such that discount rates can be calculated.
     * \post Use \code GetLog to see the optimization log.
     */
    void ValueIteFiniteDiscount(idx idxW, idx idxDur, const flt &rate,
        const flt &rateBase);

    /** Value iteration algorithm for discounted expected reward (finite
     * time-horizon).
     * \param idxW Index of the weight used.
     * \param idxDur Index of duration such that discount rates can be calculated.
     * \param iniValues Initial values used at founder level.
     * \post Use \code GetLog to see the optimization log.
     */
    void ValueIteFiniteDiscount(idx idxW, idx idxDur, const flt &rate,
        const flt &rateBase, vector<flt> & iniValues);

    /** Value iteration algorithm for expected reward (finite
     * time-horizon).
     * \param idxW Index of the weight used.
     * \param iniValues Initial values used at founder level.
     * \post Use \code GetLog to see the optimization log.
     */
    void ValueIteFinite(idx idxW, vector<flt> & iniValues);


    /** Policy iteration algorithm (infinite time-horizon).
     * \param idxW Index of the weight used.
     * \param idxDur Index of duration such that discount rates can be calculated.
     * \post Use \code GetLog to see the optimization log.
     */
    void PolicyIteDiscount(const idx idxW, const idx idxDur, const flt &rate,
        const flt &rateBase);


    /** Policy iteration algorithm average reward criterion (infinite time-horizon).
     * \param idxW Index of the weight used as nominator.
     * \param idxD The denominator we want to maximize the weight over.
     * \return g The gain.
     * \post Use \code GetLog to see the optimization log.
     */
    flt PolicyIteAve(const idx idxW, const idx idxD);


    /** Calculate weights based on current policy. Normally run
     * after an optimal policy has been found.
     * \pre Assume that the policy are stored in idxPred.
     * \param idxW The index of weights to calculate.
     */
    void CalcWeights(idx idxW) {
        HT.CalcOptW(H,idxW,idxPred,idxMult);
    }


    /** Calculate weights based on current policy (discount criterion). Normally run
     * after an optimal policy has been found.
     * \pre Assume that the policy are stored in idxPred.
     * \param idxW The index of weights to calculate.
     */
    void CalcWeightsDiscount(vector<idx> vW, idx idxDur, flt rate, flt rateBase) {
        HT.CalcOptWDiscount(H, vW, idxPred, idxMult, idxDur, rate, rateBase);
    }

    /** Calculate weights based on current policy (average criterion). Normally run
     * after an optimal policy has been found.
     * \pre Assume that the policy are stored in idxPred.
     * \param idxW The index of weights to calculate.
     */
    void CalcWeightsAve(vector<idx> vW, idx idxDur, flt g) {
        HT.CalcOptWAve(H, vW, idxPred, idxMult, idxDur, g);
    }

    /** Find the h(arc) corresponding to an action.
     * \param iS The index of the state we consider in \code states.
     * \param idxA The action index of the state we consider
     * \return An integer of the index in the state-expanded hypergraph
     * stored as pred (negative if arc, positive if harc). If zero then not found.
     */
    int FindAction(idx iS, idx idxA);


    /** Calculate rentention payoff (RPO) for a state. Normally run
     * after an optimal policy has been found.
     * \param iS The index of the state we consider in \code states.
     * \param idxW The index of weights to calculate.
     * \param idxA The action index we calculate the RPO with respect to..
     * \return A vector of the same size as the states containing the RPO values.
     */
    flt CalcRPO(idx iS, idx idxW, idx idxA) {
        int idxHArc = FindAction(iS,idxA);
        return HT.CalcRPO(H,idxW,idxMult,idxHArc);
    }


    /** Calculate rentention payoff (RPO) for a state (discount criterion). Normally run
     * after an optimal policy has been found.
     * \param iS The index of the state we consider in \code states.
     * \param idxW The index of weights to calculate.
     * \param idxA The action index we calculate the RPO with respect to.
     * \param idxD The denominator we want to calculate the weight over.
     * \param rate The interest rate.
     * \param rateBase The time-horizon the rate is valid over.
     * \return A vector of the same size as the states containing the RPO values.
     */
    flt CalcRPODiscount(idx iS, idx idxW, idx idxA, idx idxDur, flt rate,
        flt rateBase)
    {
        int idxHArc = FindAction(iS,idxA);
        //cout << "iS:" << iS << " a:" << idxHArc << " ";
        return HT.CalcRPODiscount(H,idxW,idxMult,idxHArc,idxDur,rate,rateBase);
    }


    /** Calculate rentention payoff (RPO) for a state (average criterion). Normally run
     * after an optimal policy has been found.
     * \param iS The index of the state we consider in \code states.
     * \param idxW The index of weights to calculate.
     * \param idxA The action index we calculate the RPO with respect to.
     * \param idxD The denominator we want to calculate the weight over.
     * \param g The average gain.
     * \return A vector of the same size as the states containing the RPO values.
     */
    flt CalcRPOAve(idx iS, idx idxW, idx idxA, idx idxDur, flt g) {
        int idxHArc = FindAction(iS,idxA);
        cout << "iS:" << iS << " a:" << idxHArc << " ";
        return HT.CalcRPOAve(H,idxW,idxDur,idxMult,idxHArc,g);
    }


    /** Fix an action, i.e. remove all other actions of the state.
     * \param iS The index of the state we consider in \code states.
     * \param iA The action index.
     */
    void FixAction(idx iS, idx iA) {
        int idxHArc = FindAction(iS,iA);
        H.FixHArc(idxHArc);
    }


    /** Set the action of the policy.
     * \param iS The index of the state we consider in \code states.
     * \param iA The action index.
     */
    void SetPolicyAction(idx iS, idx iA) {
        int idxHArc = FindAction(iS,iA);
        H.SetPred(idxHArc,idxPred);
    }


    /** Set the state weight.
     * \param w The weight to set.
     * \param iS The index of the state we consider in \code states.
     * \param iW The weight index.
     */
    void SetStateW(flt w, idx iS, idx iW) {
        H.itsNodes[iS+1].w[iW] = w;
    }


    /** Remove the action.
     * \param iS The index of the state we consider in \code states.
     * \param iA The action index.
     */
    void RemoveAction(idx iS, idx iA) {
        int idxHArc = FindAction(iS,iA);
        H.RemoveHArc(idxHArc);
    }


    /** Reset actions in the HMDP, i.e. no actions removed/fixed. */
    void ResetActions() {
        H.ResetSubHgf();
    }


    /** Count the number of next stage states at current level.
     * \param iState The index of the state we consider in \code states.
     */
    idx CountNext(idx iState) {
        string str = states[iState].NextStageStr();
        return stages.count(str);
    }


    /** Count the number of next child stage states.
     * \param iState The index of the state we consider in states.
     * \param iAction The index of the child action.
     */
    idx CountChild(idx iState,idx iAction) {
        string str = states[iState].NextChildStageStr(iAction);
        return stages.count(str);
    }


    /** Count the number of next stage states.
     * \param iState The index of the state we consider in states.
     */
    idx CountFather(idx iState) {
        string str = states[iState].NextFatherStageStr();
        return stages.count(str);
    }

    /** Count the number of states in the stage.
     * \param stage The string of the stage, e.g. "0,1,2,1".
     */
    idx CountStates(string stage) {
        return stages.count(stage);
    }

    /** Return string with optimal policy using indicies. */
    string PolicyInfoIdx(idx idxW);


    /** Return string with optimal policy using labels.
     * Note only last state label shown.
     */
    string PolicyInfoLabel(idx idxW);


    /** Return the label of the action of a specific state for the policy.
     * \param iState The index of the state.
     */
    string PolicyLabel(idx iState) {
        string label;
        int a;
        a = H.itsNodes[iState+1].pred[idxPred];
        if (a<0) label = states[iState].actionLabels[H.itsArcs[-a].pLabel -
            &states[iState].actionLabels[0]];
        if (a>0) label = states[iState].actionLabels[H.itsHArcs[a].pLabel -
            &states[iState].actionLabels[0]];
        return label;
    }


    /** Return the index of the action of a specific state for the policy.
     * \param iState The index of the state.
     */
    idx PolicyIdx(idx iState) {
        idx index = INFINT;
        int a;
        a = H.itsNodes[iState+1].pred[idxPred];
        if (a<0) index = H.itsArcs[-a].pLabel - &states[iState].actionLabels[0];
        if (a>0) index = H.itsHArcs[a].pLabel - &states[iState].actionLabels[0];
        return index;
    }


    /** Return the weight.
     * \param iState The index of the state.
     * \param idxW The index of the weight.
     */
    flt PolicyW(idx iState, idx idxW) {
        return H.itsNodes[iState+1].w[idxW];
    }


    /** Get the number of actions. */
    uInt TotalActions() {
        return H.ma + H.mh;
    }


    /** Check the HMDP for errors.
     * The following are checked:
     * - A states index is of size 2+3*level.
     * - Probabilities sum to one.
     * - That all transitions are to states which exists.
     * \param eps The sum of the probablities must at most differ eps from one.
     */
    bool Check(flt eps) {
        // TODO (LRE#1#): Currently do not check if the number of weights are correct, e.g. if 10 actions and 3 weights the number must be 30!!.
        log.flush();
        cpuTime.StartTime(0);
        bool okay = true;
        for (idx i=0; i<states.size(); i++) {
            okay = okay & states[i].Check(eps,log);
            if (!okay) break;
        }
        /*if (okay)*/ okay = okay & CheckIdx();
        cpuTime.StopTime(0);
        //if (!okay) log << "Error in HMDP description!" << endl;
        //else log << "Everything seem to be okay." << endl;
        log << "Cpu time for checking MDP " << cpuTime.TimeDiff(0) << "s." << endl;
        return okay;
    }

    /** Return actions for the specific state in hypergraph 'f 6' format. */
    string StateActionsToHgf(idx iState, bool & findValidOdr);

private:
// ----------------------------------------------------------------------------

    /** Check if all actions in tmpActions are defined properly. More precise
     * do the indexes correspond to a hypergraph node.
     */
    bool CheckIdx();



    /** Note state[i] corresponds to node i+1 in the state-expanded hypergraph
     * since hypergraph nodes numbered from one.
     */
    idx HgfNodeIdx(idx iState) {return iState+1;}


    /** Return actions for the specific state in hypergraph 'f 6' format. */
    //string StateActionsToHgf(idx iState);


    /** Calc the hypergraph sizes need for building the hypergraph */
    void CalcHgfSizes(uInt &n, uInt &ma, uInt &mh, uInt &d, uInt &hsize,
        uInt &sizeW, uInt &sizeWTmp, uInt &sizePred, uInt &sizeMult);


    /** Return all index except idxW and idxDur. */
    vector<idx> WeightIdx(idx idxW, idx idxDur);


    /** Maximal difference between the weights at founder level. */
    flt MaxDiffFounder(const idx &idxW,
        const pair< multimap<string, int >::iterator, multimap<string, int >::iterator > &pairZero,
        const pair< multimap<string, int >::iterator, multimap<string, int >::iterator > &pairLast);


    /** Set the reward \code r to the weights of the founder at stage zero.
     * \pre Matrix \code r must have dim (|S|,1) where |S| denote the number
     * of states at the founder level.
     * \param r The reward matrix.
     * \param idxW W The index where the reward is stored.
     * \param pairZero The iterator pair pointing to the founder states at stage zero.
     */
    void SetR(MatDouble &r, const idx &idxW,
        const pair< multimap<string, int >::iterator, multimap<string, int >::iterator > &pairZero);


    /** Calculate the rewards of the founder states given a specific policy.
     *
     */
    void FounderRewardDiscount(MatDouble &r, const idx &idxW, const idx &idxDur,
        const flt &rate, const flt &rateBase,
        const pair< multimap<string, int >::iterator, multimap<string, int >::iterator > &pairZero,
        const pair< multimap<string, int >::iterator, multimap<string, int >::iterator > &pairLast);


    /** Calculate the weights of the founder states given a specific policy.
     * \param w Column matrix storing the calculated weights.
     * \param idxW W The index we consider.
     * \param pairZero Iterator pair pointing to stage zero at founder level.
     * \param pairOne Iterator pair pointing to stage one at founder level.
     * \note Modify the weights stored in the states of the HMDP.
     */
    void FounderW(MatDouble &w, const idx &idxW,
        const pair< multimap<string, int >::iterator, multimap<string, int >::iterator > &pairZero,
        const pair< multimap<string, int >::iterator, multimap<string, int >::iterator > &pairOne);


    /** Calculate the transition probabilities including discount rates of the
     * founder states given a specific policy.
     */
    void FounderPrDiscount(MatDouble &P, const idx &idxW, const idx &idxDur,
        const flt &rate, const flt &rateBase,
        const pair< multimap<string, int >::iterator, multimap<string, int >::iterator > &pairZero,
        const pair< multimap<string, int >::iterator, multimap<string, int >::iterator > &pairLast);


    /** Calculate the transition probabilities of the
     * founder states given a specific policy.
     */
    void FounderPr(MatDouble &P, const idx &idxW,
        const pair< multimap<string, int >::iterator, multimap<string, int >::iterator > &pairZero,
        const pair< multimap<string, int >::iterator, multimap<string, int >::iterator > &pairLast);


    /** Reverse the sign of weight with index idxW. */
    void ReverseWeight(idx idxW) {H.ReverseW(idxW);}

public:
    vector<HMDPState> states;       ///< States in the HMDP.
    int levels;                     ///< Number of levels in the HMDP, i.e. the levels are 0, ..., levels-1.
    uInt timeHorizon;               ///< INFINT if consider an infinite time horizon; otherwise the number of stages at the founder level.
    vector<string> weightNames;     ///< Names of the weights/quantities stored from index 1 in \code w (of a (hyper)arc in the hypergraph).
    //flt rate;                       ///< Intrest rate used to calc discount rates.
    //flt rateBase;                   ///< The time-horizon the rate is valid over. That is, the discout rate for duration $d$ is $\exp(-rate/rateBase*d)$.
    multimap<string, int> stages;   ///< Multimap to quickly find the different stages.

private:

    Hypergraph H;                   ///< Hypergraph representation.
    HTAcyclic HT;                   ///< Shortest hypertree algorithms
    idx idxPred;                    ///< The index in pred (of a node in the hypergraph) storing the predecessor. Currently always zero.
    idx idxMult;                    ///< The index in m (of a hyperarc tail in the hypergraph) storing the transition probabilities. Currently always zero.
    TimeMan cpuTime;                ///< Mesuare cpu time.
    ostringstream log;              ///< Stream to store log messages.
};

#endif
