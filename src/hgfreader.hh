#ifndef HGFREADER_HPP
#define HGFREADER_HPP


//-----------------------------------------------------------------------------
#include <stdlib.h>     // For use of exit command
#include <stdio.h>      // For use of scanf and printf
#include <iostream>
#include "basicdt.hh"
#include "time.hh"
using namespace std;

class Hypergraph;   // forward declaration

// -----------------------------------------------------------------------------

/** Class for reading/loading directed hypergraphs.

Format "f 6" is needed for reading the text file. Hypergraph "f 6" format is
defined as:

    - Comments can be inserted using # at the top of the file, i.e. the rest of the line will be ignored.
    - First line (not including comments) must consist of \n
    f 6 <nodes> <arcs> <harcs> <max tail size of hyperarcs>
    <total number of nodes in true hyperarcs>
    <number of weights> <number of multipliers in each tail>

    ex: f6 100 30 120 5 2000 2 1

    All numbers must be integer.

    - <arcs>+<harcs> lines follows next.

A line describe a hyperarc if contains:

    <head> <tail nodes (negative)> 0 <weights> <mulitipliers>

    ex: 2 -4 -5 -8 0 20 40 60 230 2323

  The tail nodes are separated with space and written with a minus sign.
  Multipliers are integers ordered such that first comes the multipliers in
  tail one, then tail two and so fourth. All numbers must be integers except the
  weights. Note multipliers are not normalized. Important: Nodes must be
  numbered from 1!

A line describe an arc if contains

    <head> <tail node (negative)> 0 <weights>

    ex: 2 -8 0 2340 2323

  The tail nodes are separated with space and written with a minus sign.
  Multipliers are integers ordered such that first comes the multipliers in
  tail one, then tail two and so fourth. All numbers must be integers except the
  weights. Note multipliers are not normalized. Important: Nodes must be
  numbered from 1!

No order of the (hyper)arcs are assumed.

Example:

# Example hypergraph f 6 format
#
#    f 6 <nodes> <arcs> <harcs> <max tail size of hyperarcs>
#    <total number of nodes in true hyperarcs>
#    <number of multipliers in each tail> <number of weights>
#
f 6 4 1 1 3 4 3 1
1 -4 0 2 5 7
1 -2 -3 -4 0 1 2 3 10 15 20
 */
class HgfReader
{
public:

    /** Default constructor. Do nothing. */
    //HgfReader() {};

    /** Set the pointer to the hypergraph we want to read data to. */
    HgfReader(Hypergraph * pHgf):cpuTime(1) {
        pH=pHgf;
    }

    ~HgfReader() {
        DeallocateMem();
    }


    /** Read the first line in the txt file defining the size of the hypergraph.
     * Store these values in the hypergraph the reader is binded to.
     * Format "f 6" is needed for reading the file.
     */
    char ReadSizes(char filename[12]);

    /** Read a text file in 'f 6' format.
     */
    void ReadInHgf();

    /** Add a set of (hyper)arcs in 'f 6' format. */
    void AddHyperarcs(const string &strHgf);

    /** Create the hypergraph after adding hyperarcs. */
    void BuildHgf() {
        cpuTime.StartTime(0);
        CheckDimensions();        // Check if reading ok
        // Build BS arc rep
        BuildArcRepF6();
        BuildFSARep();     // Build FS arc rep
        BuildHarcRepF6();
        BuildFSHRep();
        cpuTime.StopTime(0);
        cout << "Cpu time for building the hypergraph " << cpuTime.TimeDiff(0) << endl;
    }

    void Initialize() {AllocateMem();}

private:

    /** Alocate memory for the arrays. */
    void AllocateMem();

    /** Remove memory for the arrays. */
    void DeallocateMem();

    /** Check if the right number og arcs and harcs have been read. */
    void CheckDimensions();

    void BuildArcRepF6();

    void BuildFSARep();

    void BuildHarcRepF6();

    void BuildFSHRep();

private:
    uInt arcIndex,      ///< Indexcounter for temp arcarrays.
    harcIndex,          ///< Indexcounter for temp harcarrays.
    htailsIndex,        ///< Current index in pTmpHTails.
    tailSizeIndex,      ///< Index of pTmpHarcs.
    narcs;              ///< Number of arc+hyperarcs read.

    Hypergraph * pH;    ///< Pointer to the hypergraph.

    FILE *fstream;      ///< Pointer the file reading from.

    // Tempory arrays for reading. Note store from entry 1!
    intPtr pTmpTails;           ///< For storing arc tail numbers.
    intPtr pTmpHeads;           ///< For storing arc head numbers.
    intPtr pTmpHarcs;           ///< Used when reads one (h)arc.
    fltPtr pTmpW;               ///< Used to store weights of one (h)arc.
    fltPtr * pTmpACosts;        ///< For storing arc costs.
    fltPtr * pTmpHACosts;       ///< For storing harc costs.
    intPtr pTmpTailSize;        ///< For storing the total tail size of hyperarcs in BS(i).
    intPtr pTmpHHeads;          ///< For storing harc heads.
    intPtr pTmpHTails;          ///< For storing harc tail numbers.
    intPtr pTmpHIndex;          ///< For storing the index in pHTails containing the last tail number.
    intPtr * pTmpHMult;         ///< For storing multipliers.

    TimeMan cpuTime;      ///< Mesuare reading and building cpu time.
};

//-----------------------------------------------------------------------------

#endif
