#include "hmdp.hh"
#include <vector>
using namespace std;

#include "Rinternals.h"
#include "Rdefines.h"

#ifdef __cplusplus
extern "C" {
#endif

/*
Note that R store a matrix column wise, i.e. that

    mat[r][c] = vec[r + rows * c] = vec[k]

where rows denote the number of rows in the matrix. If k is known and r and c
unknown then

    c = k/rows              (int division)
    r = k - rows * c        (int division)
*/


static SEXP type_tag;

typedef class HMDP* HMDPPtr;

/** Macro to check if ptr valid */
#define CHECK_PTR(s) do { \
	if (TYPEOF(s) != EXTPTRSXP || \
		R_ExternalPtrTag(s) != type_tag) \
		error("External pointer not valid!"); \
} while (0)

/** Install the type tag */
SEXP MDP_Init(void)
{
	type_tag = install("TYPE_TAG");
	return R_NilValue;
}

/** Create a HMDP object */
SEXP MDP_NewHMDP(SEXP binNames, SEXP fun)
{
	HMDPPtr p = new HMDP((string)CHAR(STRING_ELT(binNames, 0)),
		(string)CHAR(STRING_ELT(binNames, 1)),
		(string)CHAR(STRING_ELT(binNames, 2)),
		(string)CHAR(STRING_ELT(binNames, 3)),
		(string)CHAR(STRING_ELT(binNames, 4)),
		(string)CHAR(STRING_ELT(binNames, 5)),
		(string)CHAR(STRING_ELT(binNames, 6)));
	if (p == NULL)
		return R_NilValue;
	else {
		SEXP val = R_MakeExternalPtr(p, type_tag, R_NilValue);
		R_RegisterFinalizer(val, fun);
		return val;
	}
}

/** Remove the HMDP object */
SEXP MDP_DeleteHMDP(SEXP ptr)
{
	CHECK_PTR(ptr);
	HMDPPtr p = (HMDPPtr)R_ExternalPtrAddr(ptr);
	if (p != NULL) {
		delete p;
		R_ClearExternalPtr(ptr);
	}
	return R_NilValue;
}

/** Build the hypergraph. */
SEXP MDP_BuildHMDP(SEXP ptr)
{
	CHECK_PTR(ptr);
	HMDPPtr p = (HMDPPtr)R_ExternalPtrAddr(ptr);
	if (p == NULL) error("pointer is NULL");
	p->BuildHMDP();
	return R_NilValue;
}

/** Perform value iteration on an infinite time MDP. */
SEXP MDP_ValueIteInfDiscount(SEXP ptr, SEXP times, SEXP eps, SEXP idxW,
	SEXP idxDur, SEXP rate, SEXP rateBase, SEXP iniValues)
{
	CHECK_PTR(ptr);
	HMDPPtr p = (HMDPPtr)R_ExternalPtrAddr(ptr);
	if (p == NULL) error("pointer is NULL");
	vector<flt> ini;
	ini.assign(NUMERIC_POINTER(iniValues),
        NUMERIC_POINTER(iniValues)+GET_LENGTH(iniValues));
	p->ValueIteInfDiscount(INTEGER_POINTER(times)[0],
		NUMERIC_POINTER(eps)[0],
		INTEGER_POINTER(idxW)[0],
		INTEGER_POINTER(idxDur)[0],
		NUMERIC_POINTER(rate)[0],
		NUMERIC_POINTER(rateBase)[0],
		ini);
	return R_NilValue;
}

/** Perform value iteration on an finite time MDP. */
SEXP MDP_ValueIteFiniteDiscount(SEXP ptr, SEXP idxW, SEXP idxDur, SEXP rate,
	SEXP rateBase, SEXP iniValues)
{
	CHECK_PTR(ptr);
	HMDPPtr p = (HMDPPtr)R_ExternalPtrAddr(ptr);
	if (p == NULL) error("pointer is NULL");
	vector<flt> ini;
	ini.assign(NUMERIC_POINTER(iniValues),
        NUMERIC_POINTER(iniValues)+GET_LENGTH(iniValues));
	p->ValueIteFiniteDiscount(INTEGER_POINTER(idxW)[0],
		INTEGER_POINTER(idxDur)[0],
		NUMERIC_POINTER(rate)[0],
		NUMERIC_POINTER(rateBase)[0],
		ini);
	return R_NilValue;
}

/** Perform value iteration on an finite time MDP. */
SEXP MDP_ValueIteFinite(SEXP ptr, SEXP idxW, SEXP iniValues)
{
	CHECK_PTR(ptr);
	HMDPPtr p = (HMDPPtr)R_ExternalPtrAddr(ptr);
	if (p == NULL) error("pointer is NULL");
	vector<flt> ini;
	ini.assign(NUMERIC_POINTER(iniValues),
        NUMERIC_POINTER(iniValues)+GET_LENGTH(iniValues));
	p->ValueIteFinite(INTEGER_POINTER(idxW)[0], ini);
	return R_NilValue;
}

/** Get the time horizon. */
SEXP MDP_GetTimeHorizon(SEXP ptr)
{
	CHECK_PTR(ptr);
	HMDPPtr p = (HMDPPtr)R_ExternalPtrAddr(ptr);
	if (p == NULL) error("pointer is NULL");
	SEXP sexp;
	PROTECT(sexp = NEW_INTEGER(1));
	INTEGER_POINTER(sexp)[0] = (int)(p->timeHorizon);
	UNPROTECT(1);
	return sexp;
}

/** Get number of states.
    \return A vector of size 2 with (states at level 0, states total).
    States total does include dummy states at the founder level when infinite
    time-horizon.
 */
SEXP MDP_GetStates(SEXP ptr)
{
	CHECK_PTR(ptr);
	HMDPPtr p = (HMDPPtr)R_ExternalPtrAddr(ptr);
	if (p == NULL) error("pointer is NULL");
	SEXP sexp;
	PROTECT(sexp = NEW_INTEGER(2));
	if (p->timeHorizon<INFINT)
        INTEGER_POINTER(sexp)[0] = (int)(p->CountStates(ToString((p->timeHorizon)-1)));
    else
        INTEGER_POINTER(sexp)[0] = (int)(p->CountStates("0"));
	INTEGER_POINTER(sexp)[1] = (int)(p->states.size());
	UNPROTECT(1);
	return sexp;
}


/** Get number of actions. */
SEXP MDP_GetActions(SEXP ptr)
{
	CHECK_PTR(ptr);
	HMDPPtr p = (HMDPPtr)R_ExternalPtrAddr(ptr);
	if (p == NULL) error("pointer is NULL");
	SEXP sexp;
	PROTECT(sexp = NEW_INTEGER(1));
	INTEGER_POINTER(sexp)[0] = (int)(p->TotalActions());
	UNPROTECT(1);
	return sexp;
}


/** Get number of levels. */
SEXP MDP_GetLevels(SEXP ptr)
{
	CHECK_PTR(ptr);
	HMDPPtr p = (HMDPPtr)R_ExternalPtrAddr(ptr);
	if (p == NULL) error("pointer is NULL");
	SEXP sexp;
	PROTECT(sexp = NEW_INTEGER(1));
	INTEGER_POINTER(sexp)[0] = (int)(p->levels);
	UNPROTECT(1);
	return sexp;
}


/** Get weight names. */
SEXP MDP_GetWeightNames(SEXP ptr)
{
	CHECK_PTR(ptr);
	HMDPPtr p = (HMDPPtr)R_ExternalPtrAddr(ptr);
	if (p == NULL) error("pointer is NULL");
	SEXP names;
	PROTECT(names = NEW_CHARACTER(p->weightNames.size()));
	for (uInt i=0; i < p->weightNames.size(); ++i)
		SET_STRING_ELT(names, i, mkChar( (p->weightNames[i]).c_str() ));
	UNPROTECT(1);
	return names;
}


/** Get the optimal policy.
 \param iState Integer vector containing the indices of the states we want.
 */
SEXP MDP_GetPolicyIdx(SEXP ptr, SEXP iState)
{
	CHECK_PTR(ptr);
	HMDPPtr p = (HMDPPtr)R_ExternalPtrAddr(ptr);
	if (p == NULL) error("pointer is NULL");

	SEXP policy;
	PROTECT(policy = NEW_INTEGER(GET_LENGTH(iState)));
	for (idx i=0; i < (idx)GET_LENGTH(iState); ++i)
		INTEGER_POINTER(policy)[i] = (int)(p->PolicyIdx(INTEGER_POINTER(iState)[i]));
	UNPROTECT(1);
	return policy;
}

/** Get the optimal policy labels.
 \param iState Integer vector containing the indices of the states we want.
 */
SEXP MDP_GetPolicyLabel(SEXP ptr, SEXP iState)
{
	CHECK_PTR(ptr);
	HMDPPtr p = (HMDPPtr)R_ExternalPtrAddr(ptr);
	if (p == NULL) error("pointer is NULL");
	SEXP policy;
	PROTECT(policy = NEW_CHARACTER(GET_LENGTH(iState)));
	for (idx i=0; i < (idx)GET_LENGTH(iState); ++i)
		SET_STRING_ELT(policy, i, mkChar( (p->PolicyLabel(INTEGER_POINTER(iState)[i])).c_str() ));
	UNPROTECT(1);
	return policy;
}

/** Get the optimal policy weights.
 \param iState Integer vector containing the indices of the states we want.
 \param iW Integer vector containing the indices of the weights we want.
 */
SEXP MDP_GetPolicyW(SEXP ptr, SEXP iState, SEXP iW)
{
	CHECK_PTR(ptr);
	HMDPPtr p = (HMDPPtr)R_ExternalPtrAddr(ptr);
	if (p == NULL) error("pointer is NULL");
	SEXP policy;
	int rows = GET_LENGTH(iState);
	PROTECT(policy = allocMatrix(REALSXP, rows, GET_LENGTH(iW)));
	double * policyPtr = NUMERIC_POINTER(policy);

	for (idx i=0; i < (idx)GET_LENGTH(iState); ++i)
		for (idx j=0; j < (idx)GET_LENGTH(iW); ++j)
			policyPtr[j*rows + i] = (double)(p->PolicyW(INTEGER_POINTER(iState)[i], INTEGER_POINTER(iW)[j]));
	UNPROTECT(1);
	return policy;
}


/** Perform poliy iteration (discount) on an infinite time MDP. */
SEXP MDP_PolicyIteDiscount(SEXP ptr, SEXP idxW,
	SEXP idxDur, SEXP rate, SEXP rateBase)
{
	CHECK_PTR(ptr);
	HMDPPtr p = (HMDPPtr)R_ExternalPtrAddr(ptr);
	if (p == NULL) error("pointer is NULL");
	p->PolicyIteDiscount(INTEGER_POINTER(idxW)[0],
		INTEGER_POINTER(idxDur)[0],
		NUMERIC_POINTER(rate)[0],
		NUMERIC_POINTER(rateBase)[0]);
	return R_NilValue;
}


/** Get the log. */
SEXP MDP_GetLog(SEXP ptr)
{
	CHECK_PTR(ptr);
	HMDPPtr p = (HMDPPtr)R_ExternalPtrAddr(ptr);
	if (p == NULL) error("pointer is NULL");
	string log = p->GetLog();
	SEXP rLog;
	PROTECT(rLog = allocVector(STRSXP, 1));
	SET_STRING_ELT(rLog, 0, mkChar(log.c_str()));
	UNPROTECT(1);
	return rLog;
}


/** Check the MDP */
SEXP MDP_Check(SEXP ptr, SEXP eps)
{
	CHECK_PTR(ptr);
	HMDPPtr p = (HMDPPtr)R_ExternalPtrAddr(ptr);
	if (p == NULL) error("pointer is NULL");
	p->Check(NUMERIC_POINTER(eps)[0]);
	return R_NilValue;
}


/** Perform poliy iteration (average) on an infinite time MDP. */
SEXP MDP_PolicyIteAve(SEXP ptr, SEXP idxW, SEXP idxDur)
{
	CHECK_PTR(ptr);
	HMDPPtr p = (HMDPPtr)R_ExternalPtrAddr(ptr);
	if (p == NULL) error("pointer is NULL");
	double g = p->PolicyIteAve(INTEGER_POINTER(idxW)[0],
		INTEGER_POINTER(idxDur)[0]);
	SEXP gR;
	PROTECT(gR = NEW_NUMERIC(1));
	NUMERIC_POINTER(gR)[0] = g;
	UNPROTECT(1);
	return gR;
}


/** Calc RPO for idxW with respect to idxA. */
SEXP MDP_CalcRPO(SEXP ptr, SEXP idxW, SEXP idxA, SEXP sId)
{
	CHECK_PTR(ptr);
	HMDPPtr p = (HMDPPtr)R_ExternalPtrAddr(ptr);
	if (p == NULL) error("pointer is NULL");
	SEXP rpo;
	int rows = GET_LENGTH(sId);
	PROTECT(rpo = NEW_NUMERIC(rows));
	double * rpoPtr = NUMERIC_POINTER(rpo);
	for (idx i=0; i < (idx)GET_LENGTH(sId); ++i)
		rpoPtr[i] = (double)p->CalcRPO(INTEGER_POINTER(sId)[i],INTEGER_POINTER(idxW)[0], INTEGER_POINTER(idxA)[0]);
	UNPROTECT(1);
	return rpo;
}


/** Calc RPO for idxW with respect to idxA. */
SEXP MDP_CalcRPODiscount(SEXP ptr, SEXP idxW, SEXP idxA, SEXP sId, SEXP idxD,
	SEXP rate, SEXP rateBase)
{
	CHECK_PTR(ptr);
	HMDPPtr p = (HMDPPtr)R_ExternalPtrAddr(ptr);
	if (p == NULL) error("pointer is NULL");
	SEXP rpo;
	int rows = GET_LENGTH(sId);
	PROTECT(rpo = NEW_NUMERIC(rows));
	double * rpoPtr = NUMERIC_POINTER(rpo);
	for (idx i=0; i < (idx)GET_LENGTH(sId); ++i)
		rpoPtr[i] = (double)p->CalcRPODiscount(INTEGER_POINTER(sId)[i],
			INTEGER_POINTER(idxW)[0], INTEGER_POINTER(idxA)[0],
			INTEGER_POINTER(idxD)[0], NUMERIC_POINTER(rate)[0],
			NUMERIC_POINTER(rateBase)[0]);
	UNPROTECT(1);
	return rpo;
}


/** Calc RPO for idxW with respect to idxA. */
SEXP MDP_CalcRPOAve(SEXP ptr, SEXP idxW, SEXP idxA, SEXP sId, SEXP idxD,
	SEXP g)
{
	CHECK_PTR(ptr);
	HMDPPtr p = (HMDPPtr)R_ExternalPtrAddr(ptr);
	if (p == NULL) error("pointer is NULL");
	SEXP rpo;
	int rows = GET_LENGTH(sId);
	PROTECT(rpo = NEW_NUMERIC(rows));
	double * rpoPtr = NUMERIC_POINTER(rpo);
	for (idx i=0; i < (idx)GET_LENGTH(sId); ++i)
		rpoPtr[i] = (double)p->CalcRPOAve(INTEGER_POINTER(sId)[i],
			INTEGER_POINTER(idxW)[0], INTEGER_POINTER(idxA)[0],
			INTEGER_POINTER(idxD)[0], NUMERIC_POINTER(g)[0]);
	UNPROTECT(1);
	return rpo;
}


/** Calc weights for idxW. */
SEXP MDP_CalcWeightsFinite(SEXP ptr, SEXP idxW, SEXP termValues)
{
	CHECK_PTR(ptr);
	HMDPPtr p = (HMDPPtr)R_ExternalPtrAddr(ptr);
	if (p == NULL) error("pointer is NULL");
	vector<flt> term;
	term.assign(NUMERIC_POINTER(termValues),
        NUMERIC_POINTER(termValues)+GET_LENGTH(termValues));
	p->CalcWeightsFinite(INTEGER_POINTER(idxW)[0], term);
	return R_NilValue;
}


/** Calc weights for idxW (discount criterion). */
SEXP MDP_CalcWeightsFiniteDiscount(SEXP ptr, SEXP idxW, SEXP idxD,
	SEXP rate, SEXP rateBase, SEXP termValues)
{
	CHECK_PTR(ptr);
	HMDPPtr p = (HMDPPtr)R_ExternalPtrAddr(ptr);
	if (p == NULL) error("pointer is NULL");
	vector<flt> term;
	term.assign(NUMERIC_POINTER(termValues),
        NUMERIC_POINTER(termValues)+GET_LENGTH(termValues));
	p->CalcWeightsFiniteDiscount(INTEGER_POINTER(idxW)[0], INTEGER_POINTER(idxD)[0],
        NUMERIC_POINTER(rate)[0], NUMERIC_POINTER(rateBase)[0], term);
	return R_NilValue;
}


/** Calc weights for idxW (discount criterion). */
SEXP MDP_CalcWeightsInfDiscount(SEXP ptr, SEXP idxW, SEXP idxD,
	SEXP rate, SEXP rateBase)
{
	CHECK_PTR(ptr);
	HMDPPtr p = (HMDPPtr)R_ExternalPtrAddr(ptr);
	if (p == NULL) error("pointer is NULL");
	p->CalcWeightsInfDiscount(INTEGER_POINTER(idxW)[0], INTEGER_POINTER(idxD)[0],
        NUMERIC_POINTER(rate)[0], NUMERIC_POINTER(rateBase)[0]);
	return R_NilValue;
}


/** Calc weights for idxW (ave criterion). */
SEXP MDP_CalcWeightsInfAve(SEXP ptr, SEXP idxW, SEXP idxD)
{
	CHECK_PTR(ptr);
	HMDPPtr p = (HMDPPtr)R_ExternalPtrAddr(ptr);
	if (p == NULL) error("pointer is NULL");
	double g = p->CalcWeightsInfAve(INTEGER_POINTER(idxW)[0], INTEGER_POINTER(idxD)[0]);
	SEXP gR;
	PROTECT(gR = NEW_NUMERIC(1));
	NUMERIC_POINTER(gR)[0] = g;
	UNPROTECT(1);
	return gR;
}


/** Fix an action. */
SEXP MDP_FixAction(SEXP ptr, SEXP iS, SEXP iA)
{
	CHECK_PTR(ptr);
	HMDPPtr p = (HMDPPtr)R_ExternalPtrAddr(ptr);
	if (p == NULL) error("pointer is NULL");
	p->FixAction(INTEGER_POINTER(iS)[0], INTEGER_POINTER(iA)[0]);
	return R_NilValue;
}


/** Remove an action. */
SEXP MDP_RemoveAction(SEXP ptr, SEXP iS, SEXP iA)
{
	CHECK_PTR(ptr);
	HMDPPtr p = (HMDPPtr)R_ExternalPtrAddr(ptr);
	if (p == NULL) error("pointer is NULL");
	p->RemoveAction(INTEGER_POINTER(iS)[0], INTEGER_POINTER(iA)[0]);
	return R_NilValue;
}


/** Reset actions. */
SEXP MDP_ResetActions(SEXP ptr)
{
	CHECK_PTR(ptr);
	HMDPPtr p = (HMDPPtr)R_ExternalPtrAddr(ptr);
	if (p == NULL) error("pointer is NULL");
	p->ResetActions();
	return R_NilValue;
}


/** Set the action of the policy. */
SEXP MDP_SetPolicyAction(SEXP ptr, SEXP iS, SEXP iA)
{
	CHECK_PTR(ptr);
	HMDPPtr p = (HMDPPtr)R_ExternalPtrAddr(ptr);
	if (p == NULL) error("pointer is NULL");
	p->SetPolicyAction(INTEGER_POINTER(iS)[0], INTEGER_POINTER(iA)[0]);
	return R_NilValue;
}


/** Set the policy.
 \param policy a vector with sId and iA converted columnwise from a matrix.
 */
SEXP MDP_SetPolicy(SEXP ptr, SEXP policy)
{
	CHECK_PTR(ptr);
	HMDPPtr p = (HMDPPtr)R_ExternalPtrAddr(ptr);
	if (p == NULL) error("pointer is NULL");
	int iS,iA;
	int * pP = INTEGER_POINTER(policy);
	int rows = GET_LENGTH(policy)/2;
	for (int i=0;i<rows;++i) {
	    iS=pP[i];
	    iA=pP[rows+i];
	    p->SetPolicyAction(iS, iA);
    }
	return R_NilValue;
}


/** Set the state weight. */
SEXP MDP_SetStateW(SEXP ptr, SEXP w, SEXP iS, SEXP iW)
{
	CHECK_PTR(ptr);
	HMDPPtr p = (HMDPPtr)R_ExternalPtrAddr(ptr);
	if (p == NULL) error("pointer is NULL");
	p->SetStateW(NUMERIC_POINTER(w)[0], INTEGER_POINTER(iS)[0],
		INTEGER_POINTER(iW)[0]);
	return R_NilValue;
}


/** Set the action weight. */
SEXP MDP_SetActionW(SEXP ptr, SEXP w, SEXP iS, SEXP iA, SEXP iW)
{
	CHECK_PTR(ptr);
	HMDPPtr p = (HMDPPtr)R_ExternalPtrAddr(ptr);
	if (p == NULL) error("pointer is NULL");
	p->SetActionW(NUMERIC_POINTER(w)[0], INTEGER_POINTER(iS)[0],
		INTEGER_POINTER(iA)[0], INTEGER_POINTER(iW)[0]);
	return R_NilValue;
}


/** Return the state-expanded hgf as a matrix. */
SEXP MDP_HgfMatrix(SEXP ptr)
{
	CHECK_PTR(ptr);
	HMDPPtr p = (HMDPPtr)R_ExternalPtrAddr(ptr);
	if (p == NULL) error("pointer is NULL");
	MatSimple<int> mat = p->HgfMatrix();
	int rows = mat.rows;
	int cols = mat.cols;
	SEXP vec;
	PROTECT(vec = NEW_INTEGER(rows*cols));
	for (int k=0; k<rows*cols; ++k) INTEGER_POINTER(vec)[k] = mat(k-rows*(k/rows),k/rows);
	UNPROTECT(1);
	return vec;
}


/** Return info about a state and its actions. */
/*SEXP MDP_Info(SEXP ptr, SEXP iS)
{
	CHECK_PTR(ptr);
	HMDPPtr p = (HMDPPtr)R_ExternalPtrAddr(ptr);
	if (p == NULL) error("pointer is NULL");

}*/


/** Return ids for states having index in idxS.
 \param idxS A char vector of index in the form "n0,s0,a0,n1,s1".
 */
SEXP MDP_GetIdS(SEXP ptr, SEXP idxS)
{
	CHECK_PTR(ptr);
	HMDPPtr p = (HMDPPtr)R_ExternalPtrAddr(ptr);
	if (p == NULL) error("pointer is NULL");
    SEXP vec;
    PROTECT(vec = NEW_INTEGER(GET_LENGTH(idxS)));
    for (idx i=0; i < (idx)GET_LENGTH(idxS); ++i)
        INTEGER_POINTER(vec)[i] = p->GetIdS((string)CHAR(STRING_ELT(idxS, i)));
	UNPROTECT(1);
	return vec;
}


/** Return ids for states in stage.
 \param stages A char vector of stage index in the form "n0,s0,a0,n1".
 */
SEXP MDP_GetIdSStage(SEXP ptr, SEXP stages)
{
	CHECK_PTR(ptr);
	HMDPPtr p = (HMDPPtr)R_ExternalPtrAddr(ptr);
	if (p == NULL) error("pointer is NULL");
	vector<idx> v;
	vector<idx> tmp;
    for (idx i=0; i < (idx)GET_LENGTH(stages); ++i) {
        tmp = p->GetIdSStage((string)CHAR(STRING_ELT(stages, i)));
        for (idx j=0; j<tmp.size(); ++j) v.push_back(tmp[j]);
    }
    SEXP vec;
    PROTECT(vec = NEW_INTEGER(v.size()));
    for (idx i=0; i<v.size(); ++i)
        INTEGER_POINTER(vec)[i] = v[i];
	UNPROTECT(1);
	return vec;
}


/** Return idx string for states having id idS.
 \param idS A vector of state ids.
 */
SEXP MDP_GetIdxS(SEXP ptr, SEXP idS)
{
	CHECK_PTR(ptr);
	HMDPPtr p = (HMDPPtr)R_ExternalPtrAddr(ptr);
	if (p == NULL) error("pointer is NULL");
	SEXP idxS;
	PROTECT(idxS = allocVector(STRSXP, GET_LENGTH(idS)));
    for (idx i=0; i < (idx)GET_LENGTH(idS); ++i)
        SET_STRING_ELT(idxS, i, mkChar( (p->states[ INTEGER_POINTER(idS)[i] ].StateStr()).c_str() ));
	UNPROTECT(1);
	return idxS;
}

/** Return label of states having id idS.
 \param idS A vector of state ids.
 */
SEXP MDP_GetLabel(SEXP ptr, SEXP idS)
{
	CHECK_PTR(ptr);
	HMDPPtr p = (HMDPPtr)R_ExternalPtrAddr(ptr);
	if (p == NULL) error("pointer is NULL");
	SEXP idxS;
	PROTECT(idxS = allocVector(STRSXP, GET_LENGTH(idS)));
    for (idx i=0; i < (idx)GET_LENGTH(idS); ++i)
        SET_STRING_ELT(idxS, i, mkChar( (p->states[ INTEGER_POINTER(idS)[i] ].label).c_str() ));
	UNPROTECT(1);
	return idxS;
}


/** Return a vector of char containing action info for a specific state id.
 \param idS The state id.
 */
SEXP MDP_GetActionInfo(SEXP ptr, SEXP idS)
{
	CHECK_PTR(ptr);
	HMDPPtr p = (HMDPPtr)R_ExternalPtrAddr(ptr);
	if (p == NULL) error("pointer is NULL");
	idx iS = INTEGER_POINTER(idS)[0];
	idx size = p->states[iS].actionLabels.size();
	SEXP a;
	PROTECT(a = allocVector(STRSXP, size));
    for (idx iA=0; iA < size; ++iA)
        //Rprintf("%s \n",p->GetActionInfo(iS,iA).c_str());
         SET_STRING_ELT(a, iA, mkChar( (p->GetActionInfo(iS,iA).c_str()) ));
	UNPROTECT(1);
	return a;
}

#ifdef __cplusplus
}
#endif
