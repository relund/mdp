#ifndef SBT_HPP
#define SBT_HPP

//-----------------------------------------------------------------------------
#include <stdlib.h>
#include <stdio.h>
#include <vector>
#include <queue>
#include <math.h>
#include "hypergf.hh"
using namespace std;

class HMDP;   // forward declaration

/** The index of the Arc with a negative sign so can be stored as a
 * predecessor in a Node.
 * \param pA Pointer to the Arc in the hypergraph.
 */
#define ArcIndexPred(pA)  - (pA - H.itsArcs)

/** The index of the Arc.
 * \param pA Pointer to the Arc in the hypergraph.
 */
#define ArcIndex(pA)  (pA - H.itsArcs)

/** The index of the HArc.
 * \param pH Pointer to the HArc in the hypergraph.
 */
#define HArcIndex(pH)  (pH - H.itsHArcs)

/** The index of the Node.
 * \param pN Pointer to the Node in the hypergraph.
 */
#define NodeIndex(pN)  (pN - H.itsNodes)

/** Compare class used to compare the weight of two Node pointers stored in
  * weight index zero.
  */
struct CompareNodes
{
	/* binary function predicate for sorting two pointers Node's.
	 * Note compare two flts so may not be total accurate! Maybe LESS most be used (but will slow things down)?
	 * \return True the weight value of the first is less than or equal the second.
	 */
	bool operator() (const NodePtr p1,const NodePtr p2) const
	{
		return p1->w[0]<=p2->w[0];
	}
};


/** Hypertree procedures on an acyclic directed hypergraph (B-hypergraph).
 * \pre Assume that the hypergraph is acyclic and that a valid ordering of the
 * hypergraph is given.
 */
class HTAcyclic {
public:

    /** Constructor. */
    HTAcyclic():cpuTime(1) {}


    /** Deconstructor. */
    ~HTAcyclic() {
        validOdr.clear();
    }


    /** Calkulates the longest hypertree for all the nodes in the hypergraph.
     * \pre Assume that the hypergraph is acyclic and that a valid ordering of
     * the hypergraph is stored in validOdr. Moreover, weights in nodes with
     * backward size 0 must have been set.
     * \post The hypertree is defined pred[idxPred] and weights w[idxW] are
     * calculated in each node.
     * \param H The acyclic hypergraph.
     * \param idxW The weight index we want to optimize.
     * \param idxPred The predecessor index to be used to store the hypertree.
     * \param idxMult The index of the multipliers to use.
     * \note If we are minimizing the weights, i.e. if the goal is to minimize
     * the cost then the rewards at idxW must be multiplied with -1.
     */
    void CalcHTacyclic(Hypergraph& H, idx idxW, idx idxPred, idx idxMult);


    /** Calkulates the longest hypertree for all the nodes in the hypergraph.
     * \pre Assume that the hypergraph is acyclic and that a valid ordering of
     * the hypergraph is stored in validOdr. Moreover, weights in nodes with
     * backward size 0 must have been set. Discount
     * rates are used such that the sum of the tail weights are discounted
     * back with <tt>exp(-rate/rateBase*duration)</tt>.
     * \post The hypertree is defined pred[idxPred] and weights w[idxW] are
     * calculated in each node.
     * \param H The acyclic hypergraph.
     * \param idxW The weight index we want to optimize.
     * \param idxPred The predecessor index to be used to store the hypertree.
     * \param idxMult The index of the multipliers to use.
     * \param idxDur The duration idx of actions, i.e. the discout rate for duration $d$ is $\exp(-rate/rateBase*d)$.
     * \param rate The interest rate.
     * \param rateBase The time-horizon the rate is valid over.
     * \return True if a new hypertree found compared to the old one stored in
     * idxPred. Remember to reset the predecessors if no old hypertree before
     * running this method.
     */
    bool CalcHTacyclic(Hypergraph& H, idx idxW, idx idxPred, idx idxMult,
        idx idxDur, flt rate, flt rateBase);

    /** Calkulates the longest hypertree for all the nodes in the hypergraph
     * based on average reward update equations.
     * \pre Assume that the hypergraph is acyclic and that a valid ordering of
     * the hypergraph is stored in validOdr. Moreover, weights in nodes with
     * backward size 0 must have been set.
     * \post The hypertree is defined pred[idxPred] and weights w[idxW] are
     * calculated in each node.
     * \param H The acyclic hypergraph.
     * \param idxW The weight index used as nominator.
     * \param idxD The denominator we want to maximize the weight over.
     * \param idxPred The predecessor index to be used to store the hypertree.
     * \param idxMult The index of the multipliers to use.
     * \param g The average gain.
     * \param pHMDP Pointer to the HMDP object.
     * \return True if a new hypertree found compared to the old one stored in
     * idxPred. Remember to reset the predecessors if no old hypertree before
     * running this method.
     */
    bool CalcHTacyclicAve(Hypergraph& H, idx idxW, idx idxD, idx idxPred,
        idx idxMult, flt g, HMDP *pHMDP);


    /** Calculate rentention payoff (RPO) for a specific predecessor.
     * RPO of a node is defined as the
     * difference between the weight of the node when using predecessor idxHArc
     * and the maximum weight of the node when using another
     * predecessor different from idxA.
     * \pre Must be called after the longest hypertree have been found.
     * \param H The acyclic hypergraph.
     * \param idxW The index of weights we consider.
     * \param idxMult The index of the multipliers.
     * \param idxHArc The predecessor we calculate the RPO with respect to.
     */
    flt CalcRPO(Hypergraph& H, idx idxW, idx idxMult, int idxHArc);


    /** Calculate rentention payoff (RPO) for a specific predecessor (average
     * criterion).
     * RPO of a node is defined as the
     * difference between the weight of the node when using predecessor idxHArc
     * and the maximum weight of the node when using another
     * predecessor different from idxA.
     * \pre Must be called after the longest hypertree have been found.
     * \param H The acyclic hypergraph.
     * \param idxW The index of weights we consider.
     * \param idxD The denominator we want to calculate the weight over.
     * \param idxMult The index of the multipliers.
     * \param idxHArc The predecessor we calculate the RPO with respect to.
     * \param g The average gain.
     */
    flt CalcRPOAve(Hypergraph& H, idx idxW, idx idxD, idx idxMult,
        int idxHArc, flt g);


    /** Calculate rentention payoff (RPO) for a specific predecessor (discount
     * criterion).
     * RPO of a node is defined as the
     * difference between the weight of the node when using predecessor idxHArc
     * and the maximum weight of the node when using another
     * predecessor different from idxA.
     * \pre Must be called after the longest hypertree have been found.
     * \param H The acyclic hypergraph.
     * \param idxW The index of weights we consider.
     * \param idxMult The index of the multipliers.
     * \param idxHArc The predecessor we calculate the RPO with respect to.
     * \param idxDur The denominator we want to calculate the weight over.
     * \param rate The interest rate.
     * \param rateBase The time-horizon the rate is valid over.
     */
    flt CalcRPODiscount(Hypergraph& H, idx idxW, idx idxMult,
        int idxHArc, idx idxDur, flt rate, flt rateBase);


    /** Calculate weights based on optimal predecessor. Normally run
     * after a hypertree found.
     * \pre Assume that the hypergraph is acyclic and that a valid ordering of
     * the hypergraph is stored in validOdr. Moreover, weights in nodes with
     * backward size 0 must have been set.
     * \param H The hypergraph.
     * \param idxW The index of weights to calculate.
     * \param idxPred The index of the predecessor.
     * \param idxMult The index of the multipliers.
     */
    void CalcOptW(Hypergraph& H,idx idxW, idx idxPred, idx idxMult);


    /** Calculates weights based on optimal predecessor.
     * \pre Assume that the hypergraph is acyclic and that a valid ordering of
     * the hypergraph is stored in validOdr. Moreover, weights in nodes with
     * backward size 0 must have been set.
     * \param H The hypergraph.
     * \param vW A vector of weight indicies to calculate weights for.
     * \param idxPred The index of the predecessor.
     * \param idxMult The index of the multipliers.
     */
    void CalcOptW(Hypergraph& H, vector<idx> vW, idx idxPred, idx idxMult);


    /** Calculates weights based on optimal predecessor. Use discount rates.
     * \pre Assume that the hypergraph is acyclic and that a valid ordering of
     * the hypergraph is stored in validOdr. Moreover, weights in nodes with
     * backward size 0 must have been set. Discount
     * rates are used such that the sum of the tail weights are discounted
     * back with <tt>exp(-rate/rateBase*duration)</tt>.
     * \param H The hypergraph.
     * \param vW A vector of weight indicies to calculate weights for.
     * \param idxPred The index of the predecessor.
     * \param idxMult The index of the multipliers.
     * \param idxDur The duration idx of actions, i.e. the discout rate for duration $d$ is $\exp(-rate/rateBase*d)$.
     * \param rate The interest rate.
     * \param rateBase The time-horizon the rate is valid over.
     */
    void CalcOptWDiscount(Hypergraph& H, idx idxW, idx idxPred, idx idxMult,
        idx idxDur, flt rate, flt rateBase);

    /** Calculates weights based on optimal predecessor. Use average criterion.
     * \pre Assume that the hypergraph is acyclic and that a valid ordering of
     * the hypergraph is stored in validOdr. Moreover, weights in nodes with
     * backward size 0 must have been set.
     * \param H The hypergraph.
     * \param vW A vector of weight indicies to calculate weights for.
     * \param idxPred The index of the predecessor.
     * \param idxMult The index of the multipliers.
     * \param idxDur The duration idx of actions, i.e. the discout rate for duration $d$ is $\exp(-rate/rateBase*d)$.
     * \param rate The interest rate.
     * \param rateBase The time-horizon the rate is valid over.
     * \param rateBase The time-horizon the rate is valid over.
     * \param g The average gain.
     */
    void CalcOptWAve(Hypergraph& H, idx idxW, idx idxPred, idx idxMult,
        idx idxDur, flt g);


    /** Calculate the sub-tree multiplier values including discount rate for a
     * specific root(s) of the hypertrees (see Relund et al. 2005 which calc in the
     * opposite direction).
     * \pre Assume that an optimal hypertree has been calculated and stored at
     * the index idxPred. Moreover, the starting values \code f of the roots we
     * consider must have been set to one. Finally, a valid ordering of
     * the hypergraph is stored in validOdr.
     * \post The values are stored in <tt>flt f</tt> in each node.
     * \param H The hypergraph.
     * \param idxNode The node index of the root of the end-tree.
     * \param idxPred The predecessor index storing the optimal hypertree.
     * \param idxMult The index used for multipliers.
     * \param idxDur The duration idx of actions, i.e. the discout rate for duration $d$ is $\exp(-rate/rateBase*d)$.
     * \param rate The interest rate.
     * \param rateBase The time-horizon the rate is valid over.
     * \note Scan all nodes in the hypergraph so all nodes must be reset to zero
     * except the root(s) which must be set to one before running the method.
     */
    void CalcSubTreeValues(Hypergraph &H, idx idxPred, idx idxMult,
        idx idxDur, flt rate, flt rateBase);


    /** Calculate the sub-tree multiplier values for a
     * specific root(s) of the hypertrees (see Relund et al. 2005 which calc in the
     * opposite direction).
     * \pre Assume that an optimal hypertree has been calculated and stored at
     * the index idxPred. Moreover, the starting values \code f of the roots we
     * consider must have been set to one. Finally, a valid ordering of
     * the hypergraph is stored in validOdr.
     * \post The values are stored in <tt>flt f</tt> in each node.
     * \param H The hypergraph.
     * \param idxNode The node index of the root of the end-tree.
     * \param idxPred The predecessor index storing the optimal hypertree.
     * \param idxMult The index used for multipliers.
     * \note Scan all nodes in the hypergraph so all nodes must be reset to zero
     * except the root(s) which must be set to one before running the method.
     */
    void CalcSubTreeValues(Hypergraph &H, idx idxPred, idx idxMult);


    /** The valid ordering of the hypergraph becomes 1, ..., n. */
    void SetValidOdrToNodeOdr(Hypergraph& H) {
        validOdr.clear();
        for (int i=1; i < H.Getn()+1; i++ ) validOdr.push_back(i);
    }


    /** The valid ordering of the hypergraph becomes n, ..., 1. */
    void SetValidOdrToReverseNodeOdr(Hypergraph& H) {
        validOdr.clear();
        for (int i=H.Getn(); i>0; i--) validOdr.push_back(i);
    }

    /** Find the valid ordering of the hypergraph. This is done by finding the
     * shortest hypertree. The valid ordering is obtained as the order the nodes
     * are taken from the candidate set (heap).
     * \param H The hypergraph.
     * \param nodes The initial candidate set with pointers to the starting nodes.
     *      having no backward star (hyper)arcs.
     */
    void FindValidOdr(Hypergraph& H, vector<idx> & nodes);


    /** Print the optimal weights of the hypertree.
     * \pre Assume that CalcHTacyclic has been called.
     */
    void PrintTree(Hypergraph& H, idx idxPred);


    /** Print the predecessor (hyper)arc of a node. */
    void PrintPred(Hypergraph& H, idx idxNode, idx idxPred);

    /** Print the valid ordering. */
    void PrintValidOdr() {
        cout << "Valid odr: ";
        for (idx i=0;i<validOdr.size();++i) cout << validOdr[i] << " ";
        cout << endl;
    }
private:

private:
    std::vector<idx> validOdr;   ///< A valid ordering of the nodes in the hypergraph (index).
    TimeMan cpuTime;   ///< Functions for measuring computation time.
};

//-----------------------------------------------------------------------------

#endif
