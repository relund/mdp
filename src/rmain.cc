#include "hmdp.hh"
#include <vector>
using namespace std;

#include "Rinternals.h"
#include "Rdefines.h"

#ifdef __cplusplus
extern "C" {
#endif

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
	flt g = p->PolicyIteAve(INTEGER_POINTER(idxW)[0],
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
SEXP MDP_CalcWeights(SEXP ptr, SEXP idxW)
{
	CHECK_PTR(ptr);
	HMDPPtr p = (HMDPPtr)R_ExternalPtrAddr(ptr);
	if (p == NULL) error("pointer is NULL");
	p->CalcWeights(INTEGER_POINTER(idxW)[0]);
	return R_NilValue;
}


/** Calc weights for idxW (discount criterion). */
SEXP MDP_CalcWeightsDiscount(SEXP ptr, SEXP idxW, SEXP idxD,
	SEXP rate, SEXP rateBase)
{
	CHECK_PTR(ptr);
	HMDPPtr p = (HMDPPtr)R_ExternalPtrAddr(ptr);
	if (p == NULL) error("pointer is NULL");
	vector<idx> vW(GET_LENGTH(idxW),0);
	for (idx i=0;i<(idx)GET_LENGTH(idxW);i++) vW[i] = INTEGER_POINTER(idxW)[i];
	p->CalcWeightsDiscount(vW, INTEGER_POINTER(idxD)[0],
        NUMERIC_POINTER(rate)[0], NUMERIC_POINTER(rateBase)[0]);
	return R_NilValue;
}


/** Calc weights for idxW (ave criterion). */
SEXP MDP_CalcWeightsAve(SEXP ptr, SEXP idxW, SEXP idxD, SEXP g)
{
	CHECK_PTR(ptr);
	HMDPPtr p = (HMDPPtr)R_ExternalPtrAddr(ptr);
	if (p == NULL) error("pointer is NULL");
	vector<idx> vW(GET_LENGTH(idxW),0);
	for (idx i=0;i<(idx)GET_LENGTH(idxW);i++) vW[i] = INTEGER_POINTER(idxW)[i];
	p->CalcWeightsAve(vW, INTEGER_POINTER(idxD)[0], NUMERIC_POINTER(g)[0]);
	return R_NilValue;
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




#ifdef __cplusplus
}
#endif
