#ifndef HMPDREADER_HPP
#define HMPDREADER_HPP


//-----------------------------------------------------------------------------
#include <stdlib.h>     // For use of exit command
#include <stdio.h>      // For use of scanf and printf
#include <fstream>
#include <string>
#include <vector>
#include "basicdt.hh"
#include "time.hh"
using namespace std;

class HMDP;   // forward declaration
class HMDPAction;   // forward declaration

// -----------------------------------------------------------------------------

/** Class for reading/loading HMDP models.

The HMDP must be represented using the HMDP binary format (v1.0) which is a
collection of 7 binary files:

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

Currently the files can be created using the MDP package in R.

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
        string transProbFile, HMDP *pHMDP):cpuTime(1)
    {
        this->pHMDP = pHMDP;
        //cpuTime.StartTime(0);
        AddStates(stateIdxFile, stateIdxLblFile);
        AddActions(actionIdxFile, actionIdxLblFile, actionWFile, actionWLblFile,
            transProbFile);
        Compile();
        //cpuTime.StopTime(0);
        //cout << "Cpu for reading the binary files: " << cpuTime.TimeDiff(0) << endl;
    }

    ~HMDPReader() {}

private:

    /** Read a binary file of T's into an array of T's.
        T could for insteance be a float.
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

    /** Add dummy states at founder level if infinite time-horizon HMDP.
     */
    void Compile();

private:
    HMDP * pHMDP;         ///< Pointer to the HMDP.
    TimeMan cpuTime;
};

// -----------------------------------------------------------------------------

#endif
