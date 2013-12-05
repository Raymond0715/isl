#include "generator.h"

using namespace std;
using namespace clang;

class python_generator : public generator {
private:
	set<string> done;

	/* The output stream the python generator writes the
	 * generated code to.
	 */
	ostream &os;

public:
	python_generator(set<RecordDecl *> &types,
			 set<FunctionDecl *> &functions,
			 set<EnumDecl *> &enums);

	virtual void generate();  // overrides method in base class

private:
	void print_callback(QualType type, int arg);
	void print(const isl_class &clazz);
	void print_constructor(const isl_class &clazz, FunctionDecl *method);
	void print_method(const isl_class &clazz, FunctionDecl *method,
			  bool subclass, string super);
	void print_enum(const isl_enum &e);

	/* Redirect calls to "printf" in the print* methods to the
	 * output stream "os"
	 */
	void printf(const char *fmt, ...) __attribute__((format(printf, 2, 3)));
};
