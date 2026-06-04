// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include "hmdp.h"

using namespace Rcpp;

// Convert the integer R API Bellman-operator code to a Bellman operator.
inline HMDP::BellmanOp ToBellmanOp(idx op) {
   if (op > 8) throw std::runtime_error("Invalid Bellman operator.");
   return static_cast<HMDP::BellmanOp>(op);
}

// Convert the integer R API optimization-sense code to an optimization sense.
HMDP::OptSense ToOptSense(idx sense) {
   switch (sense) {
      case 0: return HMDP::OptSense::Maximize;
      case 1: return HMDP::OptSense::Minimize;
      default: throw std::runtime_error("Invalid optimization sense.");
   }
}

// Function to call ValueIte since Rcpp cannot handle enum types.
void RunValueIte(HMDP* hmdp, idx op, idx sense, idx maxIte, flt epsilon, const idx idxW,
              const idx idxDur, vector<flt> & termValues,
              const flt g, const flt discountF)
{
   hmdp->ValueIte(ToBellmanOp(op), ToOptSense(sense), maxIte, epsilon, idxW, idxDur, termValues, g, discountF);
}

// Function to call PolicyIte since Rcpp cannot handle enum types.
flt RunPolicyIte(HMDP* hmdp, idx op, idx sense, uSInt maxIte, const idx idxW, const idx idxD, const flt discountF)
{
   return hmdp->PolicyIte(ToBellmanOp(op), ToOptSense(sense), maxIte, idxW, idxD, discountF);
}


// Function to call PolicyIteFixedPolicy since Rcpp cannot handle enum types.
flt RunPolicyIteFixedPolicy(HMDP* hmdp, idx op, const idx idxW, const idx idxD, const flt discountF)
{
   return hmdp->PolicyIteFixedPolicy(ToBellmanOp(op), idxW, idxD, discountF);
}

// Function to call since Rcpp cannot handle enum types.
void RunCalcPolicy(HMDP* hmdp, idx op, idx idxW, flt g, idx idxD, flt discountF)
{
   hmdp->CalcPolicy(ToBellmanOp(op), idxW, g, idxD, discountF);
}



// Function to call since Rcpp cannot handle enum types.
vector<flt> RunCalcRPO(HMDP* hmdp, idx op, idx sense, vector<idx> & iS, idx idxW, vector<idx> & idxA, flt g,
                idx idxDur, flt discountF)
{
   return hmdp->CalcRPO(ToBellmanOp(op), ToOptSense(sense), iS, idxW, idxA, g, idxDur, discountF);
}


idx GetStateSizeStage(HMDP* hmdp, string stageStr) {return hmdp->GetStateSize(stageStr);}
idx GetStateSize(HMDP* hmdp) {return hmdp->GetStateSize();}
idx GetActionSize(HMDP* hmdp) {return hmdp->GetActionSize();}

string GetNextStageStr(HMDP* hmdp, string stageStr) {return hmdp->GetNextStageStr(stageStr);}

// Get state ids of a vector of stage strings.
vector<idx> GetStateIdsStages(HMDP* hmdp, vector<string> stages) {
   vector<idx> v;
   for (idx i=0; i<stages.size(); ++i) {
      vector<idx> tmp = hmdp->GetIds(stages[i]);
      v.insert(v.end(), tmp.begin(), tmp.end() );
   }
   return v;
}


// Get state ids of a vector of state strings.
vector<idx> GetStateIdsStates(HMDP* hmdp, vector<string> states) {
   vector<idx> v;
   for (idx i=0; i<states.size(); ++i) {
      v.push_back(hmdp->GetId(states[i]));
   }
   return v;
}


// Get the state string of a state given sId.
vector<string> GetStateStr(HMDP* hmdp, vector<idx> sId) {
   return hmdp->GetStatesStr(sId);
}


// Get info of actions of a state as a List.
List GetActionInfo(HMDP* hmdp, idx sId) {
   HMDP::state_iterator iteS = hmdp->GetIte(sId);
   List lst;
   if (sId>=hmdp->states.size()) return lst;
   for (HMDP::action_iterator iteA = hmdp->action_begin(iteS); iteA!=hmdp->action_end(iteS); ++iteA) {
      List tmp;
      tmp["aIdx"] = hmdp->GetIdx(iteS,iteA);
      tmp["label"] = iteA->GetLabel();
      tmp["weights"] = iteA->GetW();
      tmp["transWeights"] = iteA->GetTransW();
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
   .field_readonly("wActionNames", &HMDP::weightActionNames)
   .field_readonly("wTransNames", &HMDP::weightTransNames)
   .field_readonly("externalProc", &HMDP::externalProc)
   .field("verbose", &HMDP::verbose)

   .method("getLog", &HMDP::GetLog)
   .method("checkHMDP", &HMDP::Check)
   .method("valueIte", RunValueIte)
   .method("policyIte", RunPolicyIte)
   .method("policyIteFixedPolicy", RunPolicyIteFixedPolicy)
   .method("calcPolicy", RunCalcPolicy)
   .method("calcRPO", RunCalcRPO)
   .method("steadyStatePr", &HMDP::CalcSteadyStatePr)
   .method("getNextStageStr", GetNextStageStr)
   .method("getStateSizeStage", GetStateSizeStage)
   .method("getStateSize", GetStateSize)
   .method("getExternalInfo", &HMDP::GetExternalInfo)
   .method("getActionSize", GetActionSize)
   .method("getActionInfo", GetActionInfo)
   .method("getActionWNames", &HMDP::GetActionWNames)
   .method("getTransWNames", &HMDP::GetTransWNames)
   .method("getIds", &HMDP::GetIds)
   .method("getStateIdsStages", GetStateIdsStages)
   .method("getStateIdsStates", GetStateIdsStates)
   .method("getStateStr", GetStateStr)
   .method("getPolicy", &HMDP::GetPolicy)
   .method("getPolicyLabel", &HMDP::GetPolicyLabel)
   .method("getStateLabel", &HMDP::GetStateLabel)
   .method("getPolicyW", &HMDP::GetPolicyW)
   .method("setPolicy", &HMDP::SetPolicy)
   .method("setTerminalW", &HMDP::SetTerminalW)
   .method("save2Binary", &HMDP::Save2Binary)
   ;

   class_<HMDPBuilder>( "HMDPBuilder" )

   .constructor<bool>("Create an in-memory HMDP builder.")

   .method("setWeights", &HMDPBuilder::SetWeights)
   .method("setTransWeights", &HMDPBuilder::SetTransWeights)
   .method("addState", &HMDPBuilder::AddState)
   .method("addAction", &HMDPBuilder::AddAction)
   .method("close", &HMDPBuilder::Close)
   .method("getLog", &HMDPBuilder::GetLog)
   ;
}
