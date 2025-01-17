/*
 * Copyright 2011 Sven Verdoolaege. All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 
 *    1. Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 * 
 *    2. Redistributions in binary form must reproduce the above
 *       copyright notice, this list of conditions and the following
 *       disclaimer in the documentation and/or other materials provided
 *       with the distribution.
 * 
 * THIS SOFTWARE IS PROVIDED BY SVEN VERDOOLAEGE ''AS IS'' AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL SVEN VERDOOLAEGE OR
 * CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA,
 * OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 * 
 * The views and conclusions contained in the software and documentation
 * are those of the authors and should not be interpreted as
 * representing official policies, either expressed or implied, of
 * Sven Verdoolaege.
 */ 

#include "isl_config.h"

#include <stdio.h>
#include <stdarg.h>
#include <iostream>
#include <map>
#include "python.h"

/* Drop the "isl_" initial part of the type name "name".
 */
static string type2python(string name)
{
	return name.substr(4);
}

/* Print python class for an isl enum type.
 */
void python_generator::print_enum(const isl_enum &enu)
{
	string enum_name_str = type2python(enu.name);
	const char *enum_name = enum_name_str.c_str();

	printf("class %s:\n", enum_name);
	printf("    def __init__(self,name,value):\n");
	printf("        self.name  = name\n");
	printf("        self.value = value\n");
	printf("    def __str__(self):\n");
	printf("        return self.name\n");
	printf("    def __repr__(self):\n");
	printf("        return \"<isl.%s.%%s: %%d>\" %% "
	       "(self.name, self.value)\n", enum_name);
	printf("\n");

	map<string,int>::const_iterator it, e = enu.values.end();
	for (it	= enu.values.begin(); it != e; ++it) {
		string name_str = enu.name_without_enum(it->first);
		const char *name = name_str.c_str();
		printf("%s.%s = %s(\"%s\",%d)\n", enum_name, name,
		       enum_name, name, it->second);
	}
	printf("\n");
}

/* Print a constructor with a named static method.
 */
bool python_generator::constructorShouldBeNamed(const isl_class &clazz,
						FunctionDecl *cons)
{
	const string fullname = cons->getName().str();
	return fullname.compare("isl_equality_alloc") == 0 ||
		fullname.compare("isl_inequality_alloc") == 0;
}

/* Construct a wrapper for a callback argument (at position "arg").
 * Assign the wrapper to "cb".  We assume here that a function call
 * has at most one callback argument.
 *
 * The wrapper converts the arguments of the callback to python types.
 * If any exception is thrown, the wrapper keeps track of it in exc_info[0]
 * and returns -1.  Otherwise the wrapper returns 0.
 */
void python_generator::print_callback(QualType type, int arg)
{
	const FunctionProtoType *fn = type->getAs<FunctionProtoType>();
	unsigned n_arg = fn->getNumArgs();

	printf("        exc_info = [None]\n");
	printf("        fn = CFUNCTYPE(c_int");
	for (int i = 0; i < n_arg - 1; ++i) {
		QualType arg_type = fn->getArgType(i);
		assert(is_isl_type(arg_type));
		printf(", c_void_p");
	}
	printf(", c_void_p)\n");
	printf("        def cb_func(");
	for (int i = 0; i < n_arg; ++i) {
		if (i)
			printf(", ");
		printf("cb_arg%d", i);
	}
	printf("):\n");
	for (int i = 0; i < n_arg - 1; ++i) {
		string arg_type;
		arg_type = type2python(extract_type(fn->getArgType(i)));
		printf("            cb_arg%d = %s(ctx=arg0.ctx, "
			"ptr=cb_arg%d)\n", i, arg_type.c_str(), i);
	}
	printf("            try:\n");
	printf("                arg%d(", arg);
	for (int i = 0; i < n_arg - 1; ++i) {
		if (i)
			printf(", ");
		printf("cb_arg%d", i);
	}
	printf(")\n");
	printf("            except:\n");
	printf("                import sys\n");
	printf("                exc_info[0] = sys.exc_info()\n");
	printf("                return -1\n");
	printf("            return 0\n");
	printf("        cb = fn(cb_func)\n");
}

// Reserved words we cannot use as method names.
static const char *keywords[] = {"and", "or", "print", 0};

static string methodname2python(const isl_class &clazz,
				const string &fullname) {
	string pname = clazz.name_without_class(fullname);
	for (const char **p = keywords; *p; ++p) {
		if (pname.compare(*p) == 0) {
			pname += "_";
			break;
		}
	}
	return pname;
}

/* Print a python method corresponding to the C function "method".
 * "subclass" is set if the method belongs to a class that is a subclass
 * of some other class ("super").
 *
 * If the function has a callback argument, then it also has a "user"
 * argument.  Since Python has closures, there is no need for such
 * a user argument in the Python interface, so we simply drop it.
 * We also create a wrapper ("cb") for the callback.
 *
 * For each argument of the function that refers to an isl structure,
 * including the object on which the method is called,
 * we check if the corresponding actual argument is of the right type.
 * If not, we try to convert it to the right type.
 * It that doesn't work and if subclass is set, we try to convert self
 * to the type of the superclass and call the corresponding method.
 *
 * If the function consumes a reference, then we pass it a copy of
 * the actual argument.
 */
void python_generator::print_method(const isl_class &clazz,
				    FunctionDecl *method, bool subclass,
				    string super)
{
	string fullname = method->getName().str();
	string cname = methodname2python(clazz, fullname);
	int num_params = method->getNumParams();
	int drop_user = 0;

	for (int i = 1; i < num_params; ++i) {
		ParmVarDecl *param = method->getParamDecl(i);
		QualType type = param->getOriginalType();
		if (is_callback(type))
			drop_user = 1;
	}

	printf("    def %s(arg0", cname.c_str());
	for (int i = 1; i < num_params - drop_user; ++i)
		printf(", arg%d", i);
	printf("):\n");

	for (int i = 0; i < num_params; ++i) {
		ParmVarDecl *param = method->getParamDecl(i);
		string type;
		if (!is_isl_type(param->getOriginalType()))
			continue;
		type = type2python(extract_type(param->getOriginalType()));
		printf("        try:\n");
		printf("            if not arg%d.__class__ is %s:\n",
			i, type.c_str());
		printf("                arg%d = %s(arg%d)\n",
			i, type.c_str(), i);
		printf("        except:\n");
		if (i > 0 && subclass) {
			printf("            return %s(arg0).%s(",
				type2python(super).c_str(), cname.c_str());
			for (int i = 1; i < num_params - drop_user; ++i) {
				if (i != 1)
					printf(", ");
				printf("arg%d", i);
			}
			printf(")\n");
		} else
			printf("            raise\n");
	}
	for (int i = 1; i < num_params; ++i) {
		ParmVarDecl *param = method->getParamDecl(i);
		QualType type = param->getOriginalType();
		if (!is_callback(type))
			continue;
		print_callback(type->getPointeeType(), i);
	}
	printf("        res = isl.%s(", fullname.c_str());
	if (takes(method->getParamDecl(0))) {
		if (clazz.name.compare("isl_printer") == 0)
			printf("arg0.makePtr0()");
		else
			printf("isl.%s_copy(arg0.ptr)", clazz.name.c_str());
	} else
		printf("arg0.ptr");
	for (int i = 1; i < num_params - drop_user; ++i) {
		ParmVarDecl *param = method->getParamDecl(i);
		QualType type = param->getOriginalType();
		if (is_isl_enum(type))
			printf(", arg%d.value", i);
		else if (is_callback(type))
			printf(", cb");
		else if (is_isl_class(type)) {
			if (takes(param)) {
				string type_s = extract_type(type);
				printf(", isl.%s_copy(arg%d.ptr)",
				       type_s.c_str(), i);
			} else
				printf(", arg%d.ptr", i);
		} else
			printf(", arg%d", i);
	}
	if (drop_user)
		printf(", None");
	printf(")\n");

	QualType rettype = method->getReturnType();
	if (is_isl_class(rettype)) {
		string type;
		type = type2python(extract_type(method->getReturnType()));
		printf("        return %s(ctx=arg0.ctx, ptr=res)\n",
			type.c_str());
	} else if (is_string(rettype)) {
		printf("        strres = str(cast(res, c_char_p).value)\n");
		printf("        libc.free(res)\n");
		printf("        return strres\n");
	} else {
		if (drop_user) {
			printf("        if exc_info[0] != None:\n");
			printf("            raise exc_info[0][0], "
				"exc_info[0][1], exc_info[0][2]\n");
		}
		printf("        return res\n");
	}
}

/* Print the call to a constructor method.
 * The printed code uses "ctxVar" to refer to the isl context
 * and the result of the constructor invocation is stored in
 * the variable named by "resultVar".
 */
void python_generator::print_constructor_call(const isl_class &clazz,
					      FunctionDecl *cons,
					      const string &ctxVar,
					      const string &resultVar)
{
	string fullname = cons->getName().str();
	int num_params = cons->getNumParams();
	int drop_ctx = first_arg_is_isl_ctx(cons);

	printf("        if len(args) == %d", num_params - drop_ctx);
	for (int i = drop_ctx; i < num_params; ++i) {
		ParmVarDecl *param = cons->getParamDecl(i);
		QualType ty = param->getOriginalType();
		if (is_isl_type(ty)) {
			string type;
			type = extract_type(param->getOriginalType());
			type = type2python(type);
			printf(" and args[%d].__class__ is %s",
				i - drop_ctx, type.c_str());
		} else
			printf(" and type(args[%d]) == %s", i - drop_ctx,
			       is_string(ty) ? "str" : "int");
	}
	printf(":\n");
	printf("            %s = isl.%s(", resultVar.c_str(), fullname.c_str());
	if (drop_ctx)
		printf("%s", ctxVar.c_str());
	for (int i = drop_ctx; i < num_params; ++i) {
		ParmVarDecl *param = cons->getParamDecl(i);
		if (i)
			printf(", ");
		QualType ty = param->getOriginalType();
		if (is_isl_class(ty)) {
			if (takes(param)) {
				string type;
				type = extract_type(ty);
				printf("isl.%s_copy(args[%d].ptr)",
					type.c_str(), i - drop_ctx);
			} else
				printf("args[%d].ptr", i - drop_ctx);
		} else if (is_isl_enum(ty))
			printf("args[%d].value", i - drop_ctx);
		else
			printf("args[%d]", i - drop_ctx);
	}
	printf(")\n");
}

/* Print part of the constructor for this isl_class.
 *
 * In particular, check if the actual arguments correspond to the
 * formal arguments of "cons" and if so call "cons" and put the
 * result in self.ptr and a reference to the default context in self.ctx.
 *
 * If the function consumes a reference, then we pass it a copy of
 * the actual argument.
 */
void python_generator::print_constructor(const isl_class &clazz,
					 FunctionDecl *cons)
{
	printf("        self.ctx = Context.getDefaultInstance()\n");
	print_constructor_call(clazz, cons, "self.ctx", "self.ptr");
	printf("            return\n");
}

/* Print a constructor as a named static method. */
void python_generator::print_named_constructor(const isl_class &clazz,
					       FunctionDecl *cons)
{
	const string fullname = cons->getName().str();
	const string cname = clazz.name_without_class(fullname);
	const string p_name = type2python(clazz.name);

	printf("    @staticmethod\n");
	printf("    def %s(*args):\n", cname.c_str());
	printf("        ctx = Context.getDefaultInstance()\n");
	print_constructor_call(clazz, cons, "ctx", "ptr");
	printf("            return %s(ctx=ctx,ptr=ptr)\n", p_name.c_str());
	printf("        raise Error\n");
}

/* Print out the definition of this isl_class.
 *
 * We first check if this isl_class is a subclass of some other class.
 * If it is, we make sure the superclass is printed out first.
 *
 * Then we print a constructor with several cases, one for constructing
 * a Python object from a return value and one for each function that
 * was marked as a constructor.
 *
 * Next, we print out some common methods and the methods corresponding
 * to functions that are not marked as constructors.
 *
 * Finally, we tell ctypes about the types of the arguments of the
 * constructor functions and the return types of those function returning
 * an isl object.
 */
void python_generator::print(const isl_class &clazz)
{
	// We do not generate the isl_ctx class here (yet).
	if (clazz.is_ctx())
		return;

	const string &name = clazz.name;
	string super;
	string p_name = type2python(name);
	set<FunctionDecl *>::iterator in;
	bool subclass = is_subclass(clazz.type, super);

	if (subclass && done.find(super) == done.end())
		print(classes[super]);
	done.insert(name);

	printf("\n");
	printf("class %s", p_name.c_str());
	if (subclass)
		printf("(%s)", type2python(super).c_str());
	printf(":\n");
	printf("    def __init__(self, *args, **keywords):\n");

	printf("        if \"ptr\" in keywords:\n");
	printf("            self.ctx = keywords[\"ctx\"]\n");
	printf("            self.ptr = keywords[\"ptr\"]\n");
	printf("            return\n");

	for (in = clazz.constructors.begin(); in != clazz.constructors.end();
	     ++in) {
		if (!constructorShouldBeNamed(clazz, *in))
			print_constructor(clazz, *in);
	}
	printf("        raise Error\n");
	printf("    def __del__(self):\n");
	printf("        if hasattr(self, 'ptr'):\n");
	printf("            isl.%s_free(self.ptr)\n", name.c_str());
	if (can_be_printed(clazz)) {
		printf("    def __str__(self):\n");
		printf("        p = isl.isl_printer_to_str(self.ctx);\n");
		printf("        p = isl.isl_printer_print_%s(p, self.ptr)\n",
		       p_name.c_str());
		printf("	ptr = isl.isl_printer_get_str(p);\n");
		printf("        res = str(cast(ptr, c_char_p).value)\n");
		printf("        libc.free(ptr)\n");
		printf("        return res\n");
		printf("    def __repr__(self):\n");
		printf("        return 'isl.%s(\"%%s\")' %% str(self)\n",
		       p_name.c_str());
	}
	printf("    def makePtr0(self):\n");
	printf("        ptr = self.ptr;\n");
	printf("        delattr(self, 'ptr');\n");
	printf("        return ptr;\n");

	for (in = clazz.constructors.begin(); in != clazz.constructors.end();
	     ++in) {
		if (constructorShouldBeNamed(clazz, *in))
			print_named_constructor(clazz, *in);
	}

	for (in = clazz.methods.begin(); in != clazz.methods.end(); ++in)
		print_method(clazz, *in, subclass, super);

	printf("\n");
	for (in = clazz.constructors.begin(); in != clazz.constructors.end();
	     ++in) {
		string fullname = (*in)->getName().str();
		printf("isl.%s.restype = c_void_p\n", fullname.c_str());
		printf("isl.%s.argtypes = [", fullname.c_str());
		for (int i = 0; i < (*in)->getNumParams(); ++i) {
			ParmVarDecl *param = (*in)->getParamDecl(i);
			QualType type = param->getOriginalType();
			if (i)
				printf(", ");
			if (is_isl_ctx(type))
				printf("Context");
			else if (is_isl_type(type))
				printf("c_void_p");
			else if (is_string(type))
				printf("c_char_p");
			else
				printf("c_int");
		}
		printf("]\n");
	}
	for (in = clazz.methods.begin(); in != clazz.methods.end(); ++in) {
		string fullname = (*in)->getName().str();
		QualType type = (*in)->getReturnType();
		if (is_isl_type(type))
			printf("isl.%s.restype = c_void_p\n", fullname.c_str());
		else if (is_string(type))
			printf("isl.%s.restype = POINTER(c_char)\n",
			       fullname.c_str());
	}
	printf("isl.%s_free.argtypes = [c_void_p]\n", name.c_str());
}

python_generator::python_generator(set<RecordDecl *> &types,
				   set<FunctionDecl *> &functions,
				   set<EnumDecl *> &enums)
    : generator(types, functions, enums), os(outputfile("isl.py"))
{
}

/* Generate a python interface based on the extracted types and functions.
 *
 * We print out each class in turn.  If one of these is a subclass
 * of some other class, it will make sure the superclass is printed out first.
 */
void python_generator::generate()
{
	map<string, isl_enum>::const_iterator ei;
	map<string, isl_class>::iterator ci;
	done.clear();

	for (ei = enums.begin(); ei != enums.end(); ++ei)
		print_enum(ei->second);

	for (ci = classes.begin(); ci != classes.end(); ++ci) {
		if (done.find(ci->first) == done.end())
			print(ci->second);
	}
}

/* The print* methods call "printf". Intercept these
 * calls here and forward the strings printed to
 * the output stream for the generated file.
 */
void python_generator::printf(const char *fmt, ...)
{
	va_list ap;
	const size_t size = 1024;
	char str[size];

	va_start(ap, fmt);
	int res = vsnprintf(str, size, fmt, ap);
	// When the buffer is too small, 'size' is returned (except
	// for some non-conforming versions of glibc, which return -1).
	if (res == -1 || res == size) {
		cerr << "python_generator::printf: not enough space in buffer" << endl;
		exit(1);
	}
	os << str;
}

