#ifndef ISL_POLYNOMIAL_TYPE_H
#define ISL_POLYNOMIAL_TYPE_H

struct __isl_export isl_qpolynomial;
typedef struct isl_qpolynomial isl_qpolynomial;

struct __isl_export isl_term;
typedef struct isl_term isl_term;

struct __isl_export isl_pw_qpolynomial;
typedef struct isl_pw_qpolynomial isl_pw_qpolynomial;

enum __isl_export isl_fold {
	isl_fold_min,
	isl_fold_max,
	isl_fold_list
};

struct __isl_export isl_qpolynomial_fold;
typedef struct isl_qpolynomial_fold isl_qpolynomial_fold;

struct __isl_export isl_pw_qpolynomial_fold;
typedef struct isl_pw_qpolynomial_fold isl_pw_qpolynomial_fold;

struct __isl_export isl_union_pw_qpolynomial;
typedef struct isl_union_pw_qpolynomial isl_union_pw_qpolynomial;

struct __isl_export isl_union_pw_qpolynomial_fold;
typedef struct isl_union_pw_qpolynomial_fold isl_union_pw_qpolynomial_fold;

#endif
