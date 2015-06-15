#include <Rcpp.h>
#include "hmdp.h"

using namespace Rcpp;

/** Function to call ValueIte since Rcpp cannot handle enum types. */
void RunValueIte(HMDP* hmdp, idx crit, idx maxIte, flt epsilon, const idx idxW,
              const idx idxDur, vector<flt> & termValues,
              const flt g, const flt rate, const flt rateBase)
{
   if (crit==0)
      return hmdp->ValueIte(HMDP::DiscountedReward, maxIte, epsilon, idxW, idxDur, termValues, g, rate, rateBase);
   if (crit==1)
      return hmdp->ValueIte(HMDP::AverageReward, maxIte, epsilon, idxW, idxDur, termValues, g, rate, rateBase);
   if (crit==2)
      return hmdp->ValueIte(HMDP::Reward, maxIte, epsilon, idxW, idxDur, termValues, g, rate, rateBase);
   if (crit==3)
      return hmdp->ValueIte(HMDP::TransPr, maxIte, epsilon, idxW, idxDur, termValues, g, rate, rateBase);
   if (crit==4)
      return hmdp->ValueIte(HMDP::TransPrDiscounted, maxIte, epsilon, idxW, idxDur, termValues, g, rate, rateBase);
}

/** Function to call PolicyIte since Rcpp cannot handle enum types. */
flt RunPolicyIte(HMDP* hmdp, idx crit, uSInt maxIte, const idx idxW, const idx idxD, const flt rate, const flt rateBase)
{
   if (crit==0)
      return hmdp->PolicyIte(HMDP::DiscountedReward, maxIte, idxW, idxD, rate, rateBase);
   if (crit==1)
      return hmdp->PolicyIte(HMDP::AverageReward, maxIte, idxW, idxD, rate, rateBase);
   return hmdp->PolicyIte(HMDP::Reward, maxIte, idxW, idxD, rate, rateBase);
}


/** Function to call PolicyIteFixedPolicy since Rcpp cannot handle enum types. */
flt RunPolicyIteFixedPolicy(HMDP* hmdp, idx crit, const idx idxW, const idx idxD, const flt rate, const flt rateBase)
{
   if (crit==1)
      return hmdp->PolicyIteFixedPolicy(HMDP::DiscountedReward, idxW, idxD, rate, rateBase);
   if (crit==0)
      return hmdp->PolicyIteFixedPolicy(HMDP::AverageReward, idxW, idxD, rate, rateBase);
   return -INF;
}

/** Function to call since Rcpp cannot handle enum types. */
void RunCalcPolicy(HMDP* hmdp, idx crit, idx idxW, flt g, idx idxD, flt rate, flt rateBase)
{
   if (crit==0)
      return hmdp->CalcPolicy(HMDP::AverageReward, idxW, 0, idxD);
   if (crit==1)
      return hmdp->CalcPolicy(HMDP::DiscountedReward, idxW, 0, idxD, rate, rateBase);
   if (crit==2)
      return hmdp->CalcPolicy(HMDP::Reward, idxW);
}


idx GetStateSizeStage(HMDP* hmdp, string stageStr) {return hmdp->GetStateSize(stageStr);}
idx GetStateSize(HMDP* hmdp) {return hmdp->GetStateSize();}
idx GetActionSize(HMDP* hmdp) {return hmdp->GetActionSize();}


/** Get state ids of a vector of stage strings. */
vector<idx> GetStateIdsStages(HMDP* hmdp, vector<string> stages) {
   vector<idx> v;
   for (idx i=0; i<stages.size(); ++i) {
      vector<idx> tmp = hmdp->GetIds(stages[i]);
      v.insert(v.end(), tmp.begin(), tmp.end() );
   }
   return v;
}


/** Get state ids of a vector of state strings. */
vector<idx> GetStateIdsStates(HMDP* hmdp, vector<string> states) {
   vector<idx> v;
   for (idx i=0; i<states.size(); ++i) {
      v.push_back(hmdp->GetId(states[i]));
   }
   return v;
}


/** Get the state string of a state given sId. */
vector<string> GetStateStr(HMDP* hmdp, vector<idx> sId) {
   return hmdp->GetStatesStr(sId);
}


/** Get info of actions of a state as a List. */
List GetActionInfo(HMDP* hmdp, idx sId) {
   HMDP::state_iterator iteS = hmdp->GetIte(sId);
   List lst;
   if (sId>=hmdp->states.size()) return lst;
   for (HMDP::action_iterator iteA = hmdp->action_begin(iteS); iteA!=hmdp->action_end(iteS); ++iteA) {
      List tmp;
      tmp["aIdx"] = hmdp->GetIdx(iteS,iteA);
      tmp["label"] = iteA->GetLabel();
      tmp["weights"] = iteA->GetW();
      tmp["trans"] = iteA->GetTransIds();
      tmp["pr"] = iteA->GetTransPr();
      lst.push_back(tmp);
   }
   return lst;
}


// Define the module interface
RCPP_MODULE(HMDPModule){
   using namespace Rcpp;

   class_<HMDP>( "HMDP" )

   .constructor<string>("Load model from default binary files.")
   .constructor< vector<string>, bool >("Load model from binary files.")

   .field_readonly("timeHorizon", &HMDP::timeHorizon)
   .field_readonly("externalProcess", &HMDP::externalProc)
   .field_readonly("okay", &HMDP::okay)
   .field_readonly("levels", &HMDP::levels)
   .field_readonly("wNames", &HMDP::weightNames)
   .field("verbose", &HMDP::verbose)

   .method("getLog", &HMDP::GetLog)
   .method("checkHMDP", &HMDP::Check)
   .method("valueIte", RunValueIte)
   .method("policyIte", RunPolicyIte)
   .method("policyIteFixedPolicy", RunPolicyIteFixedPolicy)
   .method("calcPolicy", RunCalcPolicy)
   .method("getStateSizeStage", GetStateSizeStage)
   .method("getStateSize", GetStateSize)
   .method("getActionSize", GetActionSize)
   .method("getActionInfo", GetActionInfo)
   .method("getIds", &HMDP::GetIds)
   .method("getStateIdsStages", GetStateIdsStages)
   .method("getStateIdsStates", GetStateIdsStates)
   .method("getStateStr", GetStateStr)
   .method("getPolicy", &HMDP::GetPolicy)
   .method("getPolicyLabel", &HMDP::GetPolicyLabel)
   .method("getStateLabel", &HMDP::GetStateLabel)
   .method("getPolicyW", &HMDP::GetPolicyW)
   .method("setPolicy", &HMDP::SetPolicy)
   ;
}
