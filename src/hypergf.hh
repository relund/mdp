#ifndef HYPERGF_HPP
#define HYPERGF_HPP


//-----------------------------------------------------------------------------
#include <stdlib.h>     // For use of exit command
#include <stdio.h>      // For use of scanf and printf
#include <iostream>
#include <vector>
#include "basicdt.hh"
#include "time.hh"
#include "hgfreader.hh"
using namespace std;

//-----------------------------------------------------------------------------
//  basic data types: Node, Arc, Tail and Hyperarc
//-----------------------------------------------------------------------------

typedef struct Hyperarc* HArcPtr;
typedef class Node* NodePtr;
typedef struct Arc* ArcPtr;
typedef struct Tail* TailPtr;


class Arc
{
public:
    friend class Hypergraph;
    friend class HgfReader;
    friend class HTAcyclic;
    friend class HMDP;

private:
    void SetSize(int sizeW){w.resize(sizeW,0);}

   NodePtr  pHead,      ///< Pointer to head node.
            pTail;      ///< Pointer to tail node.
   vector<flt> w;       ///< Vector of floating point wights.
   bool inSubHgf;       ///< True if in the subhypergraph.
   string * pLabel;     ///< Direct pointer to label in the HMDPState object.
};

//-----------------------------------------------------------------------------

class Hyperarc
{
public:
    friend class Hypergraph;
    friend class HgfReader;
    friend class HTAcyclic;
    friend class HMDP;

private:
   void SetSize(int sizeW){w.resize(sizeW,0);}

   NodePtr  pHead;      ///< Pointer to head node.
   vector<flt> w;       ///< Vector of floating point weights.
   TailPtr  pTail;		///< Pointer to the fiste item in the tail.
   int      tailSize;   ///< Size of the tail (zero if not used)
   bool inSubHgf;       ///< True if in the subhypergraph
   string * pLabel;     ///< Direct pointer to label in the HMDPState object.
   // TODO (LRE#1#): How is labels best stored? Maybe just as an vector in HMDPState?
};

//-----------------------------------------------------------------------------

class Tail
{
public:
    friend class Hypergraph;
    friend class HgfReader;
    friend class HTAcyclic;

private:
   void SetSize(int sizeMult){m.resize(sizeMult,0);}

   NodePtr pTail;   ///< Pointer to the tail node.
   vector<flt> m;   ///< Vector of multipliers.
};

//-----------------------------------------------------------------------------

class Node
{
public:
    friend class Hypergraph;
    friend class HgfReader;
    friend class HTAcyclic;
    friend class HMDP;
    friend class CompareNodes;

    Node():temp(0),BSsize(0),FSsize(0){} //

private:
    void SetSize(int sizeW, int sizePred, int sizeWTmp){
        w.resize(sizeW,0);
        pred.resize(sizePred,0);
        //wTmp.resize(sizeWTmp,0);
    }

	~Node() {}

	/** Set all weights w to value. */
	void SetW(const flt & value) {
        for (idx i=0; i<w.size(); ++i) w[i]=value;
    }

	/** Set weight w to value. */
	void SetW(const idx & idxW, const flt & value) {
        w[idxW]=value;
    }

	/** Set weight w to value. */
	void SetPred(const idx & idxP, const int & value) {
        pred[idxP]=value;
    }

   vector<flt> w;       ///< Vector of weights.
   vector<int> pred;    ///< Vector of predecessors. + predecessor index (if hyperarc), - predecessor index (if arc), zero if no predecessor is defined.

   //vector<flt> wTmp;    ///< Vector of tempoary weights.

   ArcPtr   pAFirst;            // Pointer to first Arc in the BS
   ArcPtr*  ppAFirst;           // Pointer to first ArcPtr in the FS
   HArcPtr  pHFirst;            // Pointer to first HArc in the BS
   HArcPtr* ppHFirst;           // Pointer to first HArcPtr ind the FS

   int temp;                    // for temporary labels
   //bool isFixed;                // true if the BS have been fixed

   int BSsize;                  // size of BS
   int BSsizeInSubHgf;          // size of BS in the subhypergraph
   int FSsize;                  // size of FS

   flt f;			            // multiplier sum/product
};

//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------

// This is the header file for the hypergraph class here a backward
// representation of the hypergraph is stored together with a forward
// representation by using pointers. We use the following arrays:
//
//      itsNodes:   Array of size n+2 of Node structures. Indexes 1 to n are
//                  used for storing the nodes. idx 0 is not used, and index
//                  n+1 is used so it is possible to use pointer arithmetic.
//
//      itsArcs:    Array of size ma+2 of Arc structures. Indexes 1 to ma are
//                  used for storing the Arcs in a backward order. idx 0 is
//                  not used, and index n+1 is used so it is possible to
//                  use pointer arithmetic.
//
//      itsHArcs:   Array of size mh+2 of Hyperarc structures. Indexes 1 to mh
//                  are used for storing the Hyperarcs in a backward order.
//                  idx 0 is not used, and index n+1 is used so it is
//                  possible to use pointer arithmetic.
//
//      itsTails:   Array of size htailsize+2 of Tail structures. Indexes 1 to
//                  htailsize are used for storing the Tail struct of the tail
//                  of the harcs. idx 0 is not used, and index n+1 is used so
//                  it is possible to use pointer arithmetic.
//
//      itsFSAs:    Array of size ma+2 of Arc pointers who define the forward
//                  star of the arcs.
//
//      itsFSHs:    Array of size htailsize+2 of Hyperarc pointers who define
//                  the forward star of the hyperarcs.
//
// Note: None of the array use entry 0 and they all have a dummy entry at the
// end so it is possible to use pointer arithmetic.

// TODO (LRE#1#): Need to write dokumentation for the Hypergraph class.

class Hypergraph
{
public:
    friend class HMDP;
    friend class HTAcyclic;
    friend class HgfReader;

    Hypergraph() {
        memAllocated = false;
        pReader = new HgfReader(this);
    }

    /** Read from file. */
    Hypergraph(char filename[12]);

    ~Hypergraph();

    //accessor functions:
    int Getn() const {return n;}
    int Getmh() const {return mh;}
    int Getma() const {return ma;}

    /** Weight of node
     * \param iN Node index.
     * \param iW Weight index.
     */
    flt GetWeight(idx iN,idx iW) const {return itsNodes[iN].w[iW];}

    /*flt GetOptW(int i,int criteria) const {
        if (criteria==1) return (itsNodes[i].optw1);
        else return (itsNodes[i].optw2);}*/

    /** Return a pointer to first element in itsArcs. */
    ArcPtr GetArcsPtr() {return itsArcs;}

    /** Return a pointer to first element in itsHArcs. */
    HArcPtr GetHArcsPtr() {return itsHArcs;}

    /** Return a pointer to first element in itsTails. */
    TailPtr GetTailsPtr() {return itsTails;}

    /** Return a pointer to first element in itsNodes. */
    NodePtr GetNodesPtr() {return itsNodes;}

    /** Return the index in itsNodes array. */
	idx NodeIndex(NodePtr pNode) {return pNode - itsNodes;}

    /** Print all (hyper)arcs in hypergraph. */
    void PrintArcs();

    void PrintSubArcs();

    /** Print the arc. */
    void PrintArc(idx i);

    void PrintHArc(idx i);

    void PrintBSHArc(int i);
    void PrintFSHArc(int i);

    void PrintBSArc(int i);
    void PrintFSArc(int i);

    /** Fix the (h)arc, i.e. remove all other (h)arcs in the BS.
     * \param idxHArc The index of the hyperarc  (store as idxPred).
     */
    void FixHArc(int idxHArc);

    /** Remove the (h)arc.
     * \param idxHArc The index of the hyperarc (store as idxPred).
     */
    void RemoveHArc(int idxHArc);


    /** Set the predecessor.
     * \param idxHArc The index of the hyperarc to set.
     * \param idxPred The index of the predecessor we use.
     */
    void SetPred(int idxHArc, idx idxPred);

    /** Print node info. */
    void PrintNode(idx i) {
        cout << i << " |BS|=" << itsNodes[i].BSsize << " - w: ";
        for(idx j=0; j<sizeW; j++) cout << " " << itsNodes[i].w[j];
    }

    /** Reverse the sign of a weight at all arcs and hyperarcs. */
    void ReverseW(idx idxW) {
        idx i;
        for(i=1;i<=ma;i++) itsArcs[i].w[idxW] = -itsArcs[i].w[idxW];
        for(i=1;i<=mh;i++) itsHArcs[i].w[idxW] = -itsHArcs[i].w[idxW];
    }

    /** Reset the variables in the hypergraph so that e.g. HTAcyclic procedures
     * can be used. Also reset such that no subhypergraph defined.
     */
    void ResetHgf()
    {
        ResetSubHgf();
        ResetWeights(true);
        ResetPred();
    }

    /** Reset the variables related to defining a subhypergraph. */
    void ResetSubHgf()
    {
        idx i;
        for(i=1;i<=ma;i++) itsArcs[i].inSubHgf = true;   // all arcs is in the hypergf
        for(i=1;i<=mh;i++) itsHArcs[i].inSubHgf = true;      // all harcs is in the hypergf
        for(i=1;i<=n;i++)
        {
            //itsNodes[i].isFixed = false;      // no BS have been fixed
            itsNodes[i].BSsizeInSubHgf = itsNodes[i].BSsize;
        }
    }

    /** Reset the weights in the nodes.
     *  This include both ordinary and temponary weights.
     * \param all If true reset in all nodes else reset only in nodes i with
     * |BS(i)| > 0.
     */
    void ResetWeights(bool all);

    /** Reset the weights (at index wIdx) in all the nodes to a specific value.
     * \param wIdx The index of the weights we consider.
     * \param value The value.
     */
    void ResetWeights(idx wIdx, flt value)
    {
        for(idx i=1;i<=n;i++) itsNodes[i].w[wIdx] = value;   // set weights to value
    }

    /** Reset the predecessors in the nodes. */
    void ResetPred() {
        for(idx i=1;i<=n;i++)
            for(idx j=0; j<itsNodes[i].pred.size(); j++) itsNodes[i].pred[j] = 0;
    }

    /** Set each multiplier to 1/tailsize. */
    void SetMultipliersToAverage();

    /** Set each multiplier to one */
    void SetMultipliersToOne();

    /** Swap two weigths on the (hyper)arcs
     * \param i j idx of the two weights to swap
     */
	void SwapW(idx i,idx j);

    //void CreateSubHypgf(idxPtr pArcIndexes,idxPtr pHArcIndexes);
    // create the subhypergf defined by the 2 input arrays where the zero entry is not
    // used. The first entry there is zero in the arrays indicate that no there is no
    // more indexes. Assumes that all indexes satisfy |index|<ma for aArcArray and
    // |index|<mh for itsHArcs. If an index is neg -> remove this (h)arc if pos ->
    // fix this (h)arc. Remember to Reset the Hypergraph before calling this function!

    //void CreateSubHypgf(idx indexBefore,idx indexNow);
    // Precondition: A subhypergraph where indexBefore is removed is defined. Now
    // we want to fix indexBefore and remove indexNow instead
    // Postcondition: A the new subhypergf is defined

    //int BuildAcyclicSubHypgf(idxPtr pArcIndexes,idxPtr pHArcIndexes,
      //  idxPtr pPredIndexes);
    // create the subhypergf defined by the 2 input arrays where the zero entry is not
    // used. The first entry there is zero in the arrays indicate that no there is no
    // more indexes. Assumes that all indexes satisfy |index|<ma for pArcArray and
    // |index|<mh for itsHArcs. If an index is neg -> remove this (h)arc if pos ->
    // fix this (h)arc. Return the minimal node number of nodes where the BS have been
	// changed

    /** Set the variables needed to allocate memory and allocate memory */
    void Initialize(uInt n, uInt ma, uInt mh, uInt d, uInt hSize, uInt sizeW,
        uInt sizeWTmp, uInt sizePred, uInt sizeMult);

    void AddHyperarcs(const string &strHgf) {
        pReader->AddHyperarcs(strHgf);
    }

    void BuildHgf() {
        pReader->BuildHgf();
        delete pReader;
        ResetHgf();
        NormalizeMult();    // TODO (LRE#1#): Shall we normalize or change 'f 6' so load flt?
    }

    /** Print the hypergraph sizes. */
    void PrintSizes();

    /** Normalize multipliers such that sum to one. */
    void NormalizeMult();

private:

    /** Compare two node pointers. */
    bool NodeCompareLess(const NodePtr& p1, const NodePtr& p2) {
        return p1->w[0] < p2->w[0];
    }

    void AllocateMem();

public:
    uInt         n;              // number of nodes
    uInt         m;              // number of hyperarcs total (mh+ma)
    uInt         ma;             // number of arcs
    uInt         mh;             // number of non-arcs
    uInt         d;              // max width of hyperarcs
    uInt         hsize;          // total size of true hyperarcs
    uInt         htailsize;      // total size of hyperarc tails (hsize-mh)
    uInt         size;           // size (hsize+(2*ma))

    uInt sizeW;      ///< Size of weight vector in nodes and (hyper)arcs.
    uSInt sizeWTmp;   ///< Size of temponary wight vector.
    uSInt sizePred;   ///< Size of predecessor vector.
    uInt sizeMult;   ///< Number of multipliers in each tail.

private:
    // hyperarc representation
    HArcPtr     itsHArcs;       // Array of HArc struct
    HArcPtr*    itsFSHs;        // Array of HArcPtr defining the FS
    TailPtr     itsTails;       // Array of Tail struct who def the tails

    // arc  representation
    ArcPtr itsArcs;             // Array of Arc struct
    ArcPtr* itsFSAs;            // Array of ArcPtr defining the FS

    // node  representation
    NodePtr itsNodes;           // Array of nodes

    HgfReader * pReader;

    bool memAllocated;          ///< Have memory been allocated?
};

//-----------------------------------------------------------------------------

#endif
