#include <iostream>

#include "../src/hmdp.h"

using namespace std;

int main()
{
//    HMDP hmdp("hmdp_");
//    hmdp.verbose = true;
//    //cout << hmdp.GetLog();
//    //cout << hmdp.Print();
//
//    //hmdp.PolicyIte(HMDP::AverageReward, (idx)10, (idx)0, (idx)1);
//    //hmdp.PolicyIte(HMDP::DiscountedReward, (idx)10, (idx)0, (idx)1, 0.1, 365);
//    idx s = hmdp.GetStateSize("0");
//    cout << s << endl;
//    vector<flt> termV(s,0);
//    hmdp.ValueIte(HMDP::DiscountedReward,20000,1e-03,(idx)0,(idx)1,termV,0,0.1,365);
//    cout << hmdp.GetLog();

    HMDP hmdp("hmdp_");
    hmdp.verbose = true;
    hmdp.Save2Binary("test_");
    cout << hmdp.GetLog();
    //cout << hmdp.Print();
    //hmdp.PolicyIte(HMDP::AverageReward, (idx)10, (idx)0, (idx)1);
    //cout << hmdp.GetLog();
    //hmdp.PolicyIte(HMDP::DiscountedReward, (idx)10, (idx)0, (idx)1, 0.1, 365);

    return 0;
}
