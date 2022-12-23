#ifndef MATALG_H
#define MATALG_H

#include <Rcpp.h>  // for Rcpp::Rcerr

#include "basicdt.h"
#include "matrix.h"
#include <iostream>
#include <R_ext/Lapack.h>
// #include <R_ext/BLAS.h>

/** Few algorithms for manipulating and solving matrices. */
class MatAlg
{
public:
    /** Set P := I-P.
     \pre P is a square matrix.
     */
    void IMinusP(MatSimple<double> &P) const {
        P.MultWithMinusOne();
        for (idx i=0; i<P.rows; i++) P(i,i) = 1+P(i,i);
    }

    /** Set P := P-I.
     \pre P is a square matrix.
     */
    void PMinusI(MatSimple<double> &P) const {
        for (idx i=0; i<P.rows; i++) P(i,i) = P(i,i)-1;
    }

    /** Solve equations Pw = r.
     \return 0 if okay 1 if not.
    */
    int LASolve(const MatSimple<double> &P, MatSimple<double> &w, const MatSimple<double> &r) {
        int rows = P.rows;
        int nrhs = 1;
        int ldp = P.rows;
        int ldr = r.rows;
        //int ipiv[P.rows];  // warning: ISO C++ forbids variable length array ‘ipiv’ [-Wvla]
        //int *ipiv = new int[P.rows];  // use instead (remember to delete ipiv)
        MatSimple<int> ipiv(1,rows);  // use instead
        int info = 0;
        w.Inject(r);    // copy r to w;
        //F77_CALL(dgesv)(&rows, &nrhs, &P(0,0), &ldp, ipiv, &w(0,0), &ldr, &info);  // use with warning
        F77_CALL(dgesv)(&rows, &nrhs, &P(0,0), &ldp, &ipiv(0,0), &w(0,0), &ldr, &info);
        //delete [] ipiv;
        if (info!=0) {
            // cout << "Error in LASolve (dgesv). Info=" << info << endl;
            Rcpp::Rcerr << "Error in LASolve (dgesv). Info=" << info << endl;
            return 1;
        }
        return 0;
    }

    /** Solve equations transpose(P)w = r. */
    int LASolveT(MatSimple<double> &P, MatSimple<double> &w, const MatSimple<double> &r) {
        int rows = P.rows;
        int nrhs = 1;
        int lda = rows;
        int ldb = rows;
        int info = -1;
        MatSimple<int> ipivot(1,rows);
        w.Inject(r);    // copy r to w;
        F77_CALL(dgetrf)(&rows, &rows, &P(0,0), &lda, &ipivot(0,0), &info);
        //ipivot.Print();
        if (info!=0) {
            // cout << "Error in LASolve (dgetrf). Info=" << info << endl;
            Rcpp::Rcerr << "Error in LASolve (dgetrf). Info=" << info << endl;
            return 1;
        }
        F77_CALL(dgetrs)("T", &rows, &nrhs, &P(0,0), &lda, &ipivot(0,0), &w(0,0), &ldb, &info FCONE);
        if (info!=0) {
            // cout << "Error in LASolve (dgetrs). Info=" << info << endl;
            Rcpp::Rcerr << "Error in LASolve (dgetrs). Info=" << info << endl;
            return 1;
        }
        return 0;
    }
};


#endif

