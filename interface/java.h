#ifndef ISL_INTERFACE_JAVA_H
#define ISL_INTERFACE_JAVA_H

#include "generator.h"

class java_generator : public generator
{
public:
	java_generator(set<RecordDecl *> &types, set<FunctionDecl *> &functions,
		       set<EnumDecl *> &enums);

	virtual void generate();  // overrides method in base class

private:
	void generateClasses();
	void generateEnums();

	void print_additional_val_methods(ostream &os);
	void print_additional_ctx_methods(ostream &os);
	void print_callback(ostream &os, QualType type, const string &arg);
	void prepare_argument(ostream &os, const ParmVarDecl *param);
	void print_argument(ostream &os, ParmVarDecl *param);
	void handle_result_argument(ostream &os, const string &ctx,
				    const ParmVarDecl *param);
	void handle_enum_return(ostream &os, const string &res,
				const isl_enum &enu);
	void handle_return(ostream &os, const FunctionDecl *method,
			   const string &resVar);
	void print_method(ostream &os, isl_class &clazz, FunctionDecl *method,
			  bool subclass, string super);
	void print_constructor(ostream &os, isl_class &clazz,
			       FunctionDecl *cons, bool asNamedConstructor);
	void print_class(isl_class &clazz);
	void print_method_jni(FunctionDecl *method);

	void print_enum(const isl_enum &enu);

	string paramtype2jni_c(QualType ty);
	string paramtype2jni(QualType ty, bool wrapperTypes = false,
			     bool isBool = false);
	string paramtype2jni(const ParmVarDecl *decl);
	string paramtype2java(QualType ty, bool wrapperTypes = false,
			      bool isBool = false);
	string paramtype2java(const ParmVarDecl *decl);
	string rettype2jni_c(QualType ty);
	string rettype2jni(const FunctionDecl *method);
	string rettype2java(const FunctionDecl *method);
	string methodname2java(const isl_class &clazz,
			       const string &methodname);
	string isl_ptr(const string &classname, const string &expression,
		       bool is_takes);

	string javaTypeName(QualType ty);
};

#endif
