#ifndef ISL_INTERFACE_GENERATOR_H
#define ISL_INTERFACE_GENERATOR_H

#include <clang/AST/Decl.h>
#include <fstream>
#include <iostream>
#include <map>
#include <set>
#include <sstream>
#include <string>

#include <cerrno>
#include <sys/stat.h>
#include <sys/types.h>

using namespace clang;
using namespace std;

/* isl_class collects all constructors and methods for an isl "class".
 * "name" is the name of the class.
 * "type" is the declaration that introduces the type.
 */
struct isl_class {
	string name;
	RecordDecl *type;
	set<FunctionDecl *> constructors;
	set<FunctionDecl *> named_constructors;
	set<FunctionDecl *> methods;

	/* Return the a method name with the class prefix stripped,
	 * e.g., "isl_set_intersect" becomes "intersect" for class
	 * "isl_set". When the class name is not a prefix of the
	 * method name, only "isl_" is stripped.
	 *
	 * This is needed for "isl_equality_alloc" and "isl_inequality_alloc" in
	 * class "isl_constraint"
	 *
	 * (see also the list of exceptions to the usual naming scheme at
	 * generator::method2class()).
	 */
	string name_without_class(const string &methodname) const;

	bool is_ctx() const;
};

struct isl_enum {
	string name;
	map<string,int> values;

	string name_without_enum(const string &valname) const;
};

/* Base class for interface generators.
 */
class generator {
private:
	/* Mapping from file names (for generated files) to
	 * string output streams for the file contents.
	 */
	map<string,ostringstream*> files;

protected:
	map<string,isl_class> classes;
	map<string,isl_enum>  enums;

public:
	generator(set<RecordDecl *> &types, set<FunctionDecl *> &functions,
		  set<EnumDecl *> &enums);
	virtual ~generator() = 0;

	virtual void generate() = 0;

	/* Write generated files to the given directory. */
	void write_generated_files(const string &directory);

protected:
	/* Obtain the output stream for a given file name. */
	ostream &outputfile(const string &name);

	bool is_subclass(RecordDecl *decl, string &super);
	bool is_constructor(Decl *decl);
	bool takes(Decl *decl);
	bool gives(Decl *decl);
	isl_class &method2class(map<string, isl_class> &classes,
				FunctionDecl *fd);
	bool is_isl_ctx(QualType type);
	bool first_arg_is_isl_ctx(FunctionDecl *fd);
	bool is_isl_type(QualType type);
	bool is_isl_bool(QualType type);
	bool is_callback(QualType type);
	bool is_string(QualType type);
	bool is_isl_class(QualType type);
	bool is_unsigned(QualType type);
	bool is_isl_enum(QualType type);
	string extract_type(QualType type);
	bool is_isl_result_argument(QualType type);
	bool is_callback_with_user(QualType type);
	bool has_user_pointer(FunctionDecl *fd);

	const isl_enum &find_enum(QualType type);

	// Check if there is an isl_printer_print_* method for an isl class
	bool can_be_printed(const isl_class &clazz) const;

	/* Check if the class represents objects that are updaten in-place
	 * (such as isl_band).
	 */
	bool is_inplace(const isl_class &clazz) const;

	// Check if a callback (which is argument 'arg' of 'fd') has
	// an __isl_take annotation at its 'cb_arg'th argument.
	bool is_callback_argument_take(FunctionDecl *fd, unsigned arg, unsigned cb_arg);
};

#endif
