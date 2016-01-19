#include "isl_config.h"

#include <cctype>
#include <cstdio>
#include <sstream>

#include "generator.h"
#include "java.h"

static const string packagePath = "isl/";
static const string jniSrc = "isl_jni.c";
static const string commonHeader =
    "package isl;\n"
    "import isl.*;\n"
    "import java.lang.ref.ReferenceQueue;\n";

static string name2camelcase(const string &name, bool startUpper)
{
	bool mkUpper = startUpper;
	string javaname;
	for (string::const_iterator it = name.begin(); it != name.end(); ++it) {
		char c = *it;
		if (c == '_')
			mkUpper = true;
		else if (mkUpper) {
			javaname += toupper(c);
			mkUpper = false;
		} else
			javaname += c;
	}
	return javaname;
}

static string enumval2java(const isl_enum &enu, const string &valname)
{
	return name2camelcase(enu.name_without_enum(valname), true);
}

/* Drop the "isl_" initial part of the type name "name".
 */
static const string type2java(const string &name)
{
	assert(name.length() >= 4);
	return name2camelcase(name.substr(4), true);
}

static const string jniCallbackApplyMID(int nr_parms)
{
	return "JNICallback" + to_string(nr_parms) + "_apply_mid";
}

static const string jniIntCallbackApplyMID(int nr_parms)
{
	return "JNIIntCallback" + to_string(nr_parms) + "_apply_mid";
}


static const string jnifn(const string &name, const string &retty)
{
	string jniname;
	for (unsigned i=0; i<name.length(); ++i) {
		char ch = name[i];
		jniname += ch;
		if (ch == '_')
			jniname += '1';
	}

	return "JNIEXPORT " + retty + " JNICALL Java_isl_Impl_" + jniname
			+ "(JNIEnv *env, jclass theclass";
}

static const string jlong2islptr(const string &longExpr, const string &type) {
	return "((" + type + " *) (uintptr_t) " + longExpr + ")";
}

static const string islptr2jlong(const string &islptr) {
	return "((jlong) (uintptr_t) " + islptr + ")";
}

/* Get the representation of type "ty" on the C side of the
 * JNI interface.
 */
string java_generator::paramtype2jni_c(QualType ty) {
	if (is_isl_class(ty))
		return "jlong";
	else if (is_isl_ctx(ty))
		return "jlong";
	else if (is_string(ty))
		return "jstring";
	else if (is_isl_enum(ty))
		return "jint";
	else if (ty->isIntegerType())
		if (ty.getAsString().compare("int") == 0)
			return "jint";
		else
			return "jlong";
	else if (is_callback(ty))
		return "jobject";
	else if (is_string(ty))
		return "jstring";
	else if (ty->isVoidType())
		return "void";
	else if (ty->isFunctionPointerType())
		return "jobject";
	else if (ty->isPointerType()) {
		QualType targetty = ty->getPointeeType();
		if (targetty->isVoidType())
			return "jobject";
		else if (targetty->isIntegerType())
			return "jintArray";
		else if (is_isl_class(targetty))
			return "jlongArray";
	} else if (ty.getAsString().compare("double") == 0)
		return "jdouble";

	cerr << "Unhandled type in paramtype2jni: " << ty.getAsString() << endl;
	exit(1);
}

/* Get the parameter type of an argument of type "ty" of a Java function
 * representing a given isl function in the JNI interface.
 * When isBool is true, then a pointer to int argument is assumed to
 * denote a boolean output parameter.
 * When wrapperTypes is true, a wrapper class (e.g. "Integer") is returned
 * instead of the underlying primitive type (e.g. "int").
 */
string java_generator::paramtype2jni(QualType ty, bool wrapperTypes,
				     bool isBool)
{
	string type;
	if (is_isl_ctx(ty)) {
		type = "long";
	} else if (is_isl_result_argument(ty)) {
		type = "long[]";
	} else if (is_isl_class(ty)) {
		type = "long";
	} else if (is_isl_enum(ty)) {
		type = "int";
	} else if (is_string(ty)) {
		type = "String";
	} else if (ty->isFunctionPointerType()) {
		const FunctionProtoType *ft =
				ty->getPointeeType()->getAs<FunctionProtoType>();
		unsigned nArgs =
				ft->getNumArgs() - 1;  // drop "void *user" argument
		ostringstream os;
		bool intCB = ft->getReturnType()->isIntegerType();
		os << (intCB ? "JNIIntCallback" : "JNICallback") << dec << nArgs;
		type = os.str();
	} else if (ty->isPointerType()) {
		if (ty->getPointeeType().getAsString().compare("int") == 0)
			type = isBool ? "boolean[]" : "int[]";
		else
			type = "long";
	} else if (ty->isVoidType())
		type = "void";
	else if (ty->isIntegerType()) {
		if (ty.getAsString().compare("long") == 0)
			type = wrapperTypes ? "Long" : "long";
		else
			type = wrapperTypes ? "Integer" : "int";
	} else if (ty.getAsString().compare("double") == 0)
		type = wrapperTypes ? "Double" : "double";
	else {
		cerr << "Unsupported argument type: " << ty.getAsString()
		     << endl;
		exit(1);
	}

	return type;
}

string java_generator::rettype2jni_c(QualType ty)
{
	return paramtype2jni_c(ty);
}

string java_generator::paramtype2jni(const ParmVarDecl *decl)
{
	return paramtype2jni(decl->getOriginalType(), false);
}

string java_generator::paramtype2java(const ParmVarDecl *decl)
{
	return paramtype2java(decl->getOriginalType(), false);
}

/* Get the return type of the Java function representing a given isl
 * function in the Java interface.
 */
string java_generator::rettype2jni(const FunctionDecl *method)
{
	return paramtype2jni(method->getReturnType());
}

/* Get the Java type corresponding to a given parameter type
 * of an isl function.
 * When wrapperTypes is true, a wrapper class (e.g. "Integer") is
 * returned instead of the underlying primitive type (e.g. "int").
 */
string java_generator::paramtype2java(QualType type, bool wrapperTypes,
				      bool isBool)
{
	if (is_isl_ctx(type)) {
		return "Ctx";
	} else if (is_isl_type(type)) {
		return javaTypeName(type);
	} else if (is_isl_result_argument(type)) {
		return javaTypeName(type->getPointeeType()) + "[]";
	} else if (type->isPointerType()) {
		QualType ptype = type->getPointeeType();
		if (ptype->isFunctionType()) {
			const FunctionProtoType *ft =
			    ptype->getAs<FunctionProtoType>();
			unsigned nArgs =
			    ft->getNumArgs() - 1;  // drop "void *user" argument
			ostringstream os;
			bool nonVoidCB = !ft->getReturnType()->isIntegerType();
			os << (nonVoidCB ? "Callback" : "VoidCallback") << dec
			   << nArgs << "<";
			for (unsigned i = 0; i < nArgs; ++i)
				os << (i > 0 ? "," : "")
				   << paramtype2java(ft->getArgType(i), true);
			if (nonVoidCB)
				os << ","
				   << paramtype2java(ft->getReturnType(), true);
			os << ">";
			return os.str();
		}
	}

	return paramtype2jni(type, wrapperTypes, isBool);
}

/* Get the return type of the Java method corresponding
 * to the given isl function.
 */
string java_generator::rettype2java(const FunctionDecl *method)
{
	QualType retty = method->getReturnType();
	if (is_isl_bool(retty)) {
		return "boolean";
        }
	return paramtype2java(retty);
}

static const char *keywords[] = {"void", 0};

string java_generator::methodname2java(const isl_class &clazz,
				       const string &methodname)
{
	const string cname = clazz.name_without_class(methodname);
	string jname = name2camelcase(cname, false);
	for (const char **p = keywords; *p; ++p) {
		if (jname == *p) {
			jname += "_";
			break;
		}
	}
	return jname;
}

string java_generator::isl_ptr(const string &classname,
			       const string &expression, bool is_takes)
{
	ostringstream os;

	if (is_takes) {
		if (classname.compare("isl_printer") == 0)
			os << "(" << expression << ").makePtr0()";
		else {
			os << "Impl." << classname << "_copy(("
			   << expression << ").ptr)";
		}
	} else {
		os << "(" << expression << ").ptr";
	}

	return os.str();
}

string java_generator::javaTypeName(QualType ty)
{
	return type2java(extract_type(ty));
}

void java_generator::print_additional_val_methods(ostream &os)
{
	os << "    // Additional convenience methods" << endl
	   << "    public static Val fromBigInteger(Ctx ctx, "
	      "java.math.BigInteger i) { return readFromStr(ctx, "
	      "i.toString()); }" << endl
	   << "    public static Val fromLong(Ctx ctx, long l) { return "
	      "readFromStr(ctx, Long.toString(l)); }" << endl
	   << "    public static Val fromInt(Ctx ctx, int i) { return "
	      "readFromStr(ctx, Integer.toString(i)); }" << endl
	   << "    public java.math.BigInteger getNum() {" << endl
	   << "        return new "
	      "java.math.BigInteger(toString().split(\"/\")[0]);" << endl
	   << "    }" << endl << "    public java.math.BigInteger getDen() {"
	   << endl << "       String[] s = toString().split(\"/\");" << endl
	   << "       return new java.math.BigInteger(s.length == 2 ? s[1] : "
	      "\"1\");" << endl << "    }" << endl;
}

void java_generator::print_additional_ctx_methods(ostream &os)
{
	os << "    final void freeIslC() {" << endl
	   << "        Reference<? extends Object> ref = null;" << endl
	   << "        while ((ref = this.refQ.poll()) != null) {" << endl
	   << "            DLnkdPhntmRef phRef = (DLnkdPhntmRef) ref;" << endl
	   << "            phRef.freeCPtr();" << endl
	   << "            phRef.remove();" << endl
	   << "        }" << endl
	   << "    }" << endl
	   << "    private RuntimeException lastException = null;" << endl
	   << "    void setException(RuntimeException e) {" << endl
	   << "        if (lastException != null)" << endl
	   << "            System.err.println(\"isl bindings warning: "
	      "two exceptions in a row; exception not handled:\\n\" +"
	   << endl << "                lastException.getMessage());" << endl
	   << "        lastException = e;" << endl
	   << "	   }" << endl
	   << "    void checkError() {" << endl
	   << "        RuntimeException e = lastException;" << endl
	   << "        lastException = null;" << endl
	   << "        if (Impl.isl_ctx_last_error(ptr) != 0) { // "
	      "0 == isl_error_none" << endl
	   << "            Impl.isl_ctx_reset_error(ptr);" << endl
	   << "            e = new IslException(\"isl error\", e);"
	   << endl
	   << "        }" << endl
	   << "        if (e != null)" << endl
	   << "            throw e;" << endl
	   << "     }" << endl;
}

/* Construct a wrapper for a callback argument (at position "arg").
 * Assign the wrapper to "cb".  We assume here that a function call
 * has at most one callback argument.
 */
void java_generator::print_callback(ostream &os, QualType type,
				    const string &arg)
{
	const FunctionProtoType *fn = type->getAs<FunctionProtoType>();
	unsigned n_arg = fn->getNumArgs();

	bool has_result;
	string res_ty, err_res, ok_res;

	QualType t = fn->getReturnType();
	if (is_isl_class(t)) {
		has_result = true;
		res_ty = "long";
		err_res = "0L";
		ok_res = "SHOULD_NEVER_BE_USED";
	} else if (t->isIntegerType()) {
		has_result = false;
		res_ty = "int";
		err_res = "-1";
		ok_res = "0";
	} else {
		cerr << "Error: unhandled result type for callback: "
		     << t.getAsString() << endl;
		exit(1);
	}

	const string cbClass((has_result ? "JNICallback" : "JNIIntCallback") + to_string(n_arg-1));
	os << "            final Ctx _ctx = this.ctx;" << endl
	   << "            " << cbClass << " cb = new " << cbClass << "() {" << endl
	   << "                public " << res_ty << " apply(";
	for (unsigned i = 0; i < n_arg - 1; ++i) {
		assert(is_isl_type(fn->getArgType(i)));
		if (i > 0)
			os << ", ";
		os << "long cb_arg" << dec << i;
	}
	os << ") {" << endl;
	os << "                    " << res_ty << " res = " << err_res << ";"
	   << endl;
	os << "                    try {" << endl;
	os << "                        " << (has_result ? "res = " : "") << arg
	   << ".apply(";
	for (unsigned i = 0; i < n_arg - 1; ++i) {
		if (i > 0)
			os << ", ";
		os << "new " << type2java(extract_type(fn->getArgType(i)))
		   << "(_ctx, cb_arg" << dec << i << ")";
	}
	os << ")" << (is_isl_class(t) ? ".ptr" : "") << ";" << endl;
	if (!has_result)
		os << "                        res = " << ok_res << ";" << endl;
	os << "                    } catch (RuntimeException e) {" << endl
	   << "                        _ctx.setException(e);" << endl
	   << "                    }" << endl
	   << "                    return res;" << endl
	   << "                }" << endl
	   << "            };" << endl;
}

void java_generator::prepare_argument(ostream &os, const ParmVarDecl *param)
{
	QualType type = param->getOriginalType();
	const string &name = param->getNameAsString();
	if (is_callback(type)) {
		print_callback(os, type->getPointeeType(), name);
	} else if (is_isl_result_argument(type)) {
		type = type->getPointeeType();
		string javaTyName = javaTypeName(type);
		os << "            assert " << name << " == null || " << name
		   << ".length == 1;" << endl
		   << "        long[] _" << name << " = " << name
		   << " == null ? null : new long[1];" << endl;
	} else if (is_unsigned(type)) {
		os << "            assert " << name << " >= 0;" << endl;
	} else if (is_isl_class(type)) {
		// Make sure the isl object is of the right type,
		// i.e., it matches the compile time type of the
		// parameter (an actual argument for, e.g., isl_union_set
		// could be an isl_set at runtime).
		os << "            " << name << " = " << name << ".as"
		   << javaTypeName(type) << "();" << endl;
	}
}

void java_generator::print_argument(ostream &os, ParmVarDecl *param)
{
	const string &name = param->getNameAsString();
	QualType type = param->getOriginalType();
	if (is_callback(type)) {
		os << "cb";
	} else if (is_isl_result_argument(type)) {
		os << "_" << name;
	} else if (is_isl_enum(type))
		os << name << ".value";
	else if (is_isl_class(type)) {
		os << isl_ptr(extract_type(type), name, takes(param));
	} else if (is_string(type)) {
		os << name;
	} else {
		os << name;
	}
}

void java_generator::handle_result_argument(ostream &os, const string &ctx,
					    const ParmVarDecl *param)
{
	const string &name = param->getNameAsString();
	QualType type = param->getOriginalType();
	if (is_isl_result_argument(type)) {
		const string javaTyName = javaTypeName(type->getPointeeType());
		os << "        if (" << name << " != null)" << endl
		   << "            " << name << "[0] = new " << javaTyName
		   << "(" << ctx << ", _" << name << "[0]);" << endl;
	}
}

void java_generator::handle_enum_return(ostream &os, const string &res,
					const isl_enum &enu)
{
	os << "        switch(" << res << ") {" << endl;
	map<string, int>::const_iterator it;
	// TODO: it->second is not unique! (isl_dim_set == isl_dim_out)
	for (it = enu.values.begin(); it != enu.values.end(); ++it) {
		os << "        case " << it->second << ": return "
		   << type2java(enu.name) << "." << enumval2java(enu, it->first)
		   << ";" << endl;
	}
	os << "        default: throw new IllegalStateException"
	   << "(\"No enum constant in " << type2java(enu.name)
	   << " for value \" + (" << res << ") + \"?\");" << endl << "        }"
	   << endl;
}

void java_generator::handle_return(ostream &os, const FunctionDecl *method,
				   const string &resVar)
{
	QualType rettype = method->getReturnType();
	string fullname = method->getName();
	if (rettype->isVoidType()) {
		return;
	} else if (is_isl_class(rettype)) {
		string type;
		type = type2java(extract_type(method->getReturnType()));
		os << "            if (" << resVar << " == 0L)" << endl
		   << "                throw new IslException(\"" << fullname
		   << " returned a NULL pointer.\");" << endl
		   << "            return new " << type << "(this.ctx, "
		   << resVar << ");" << endl;
	} else if (is_isl_enum(rettype)) {
		handle_enum_return(os, resVar, find_enum(rettype));
	} else if (is_isl_bool(rettype)) {
		os << "            if (" << resVar << " == -1)" << endl
		   << "                throw new IslException(\"" << fullname
		   << " returned isl_error.\");" << endl << "        return "
		   << resVar << " != 0;" << endl;
	} else {
		os << "            return " << resVar << ";" << endl;
	}
}

/* Print a java method corresponding to the C function "method".
 * "subclass" is set if the method belongs to a class that is a subclass
 * of some other class ("super").
 *
 * If the function has a callback argument, then it also has a "user"
 * argument.  Since Java has closures, there is no need for such
 * a user argument in the Java interface, so we simply drop it.
 * We also create a wrapper ("cb") for the callback.
 *
 * For methods with callbacks (which we assume to return 0 or -1) we
 * set the return type of the Java method and the return type of the
 * callback to "void" since errors should be signaled through exceptions.
 *
 * If the function consumes a reference, then we pass it a copy of
 * the actual argument.
 */
void java_generator::print_method(ostream &os, isl_class &clazz,
				  FunctionDecl *method, bool subclass,
				  string super)
{
	string p_name = type2java(clazz.name);
	string fullname = method->getName();
	string cname = methodname2java(clazz, fullname);
	int num_params = method->getNumParams();

	int drop_user = has_user_pointer(method) ? 1 : 0;
	bool is_void = method->getReturnType()->isVoidType();

	os << "    public " << rettype2java(method) << " " << cname << "(";
	for (int i = 1; i < num_params - drop_user; ++i) {
		if (i > 1)
			os << ", ";
		ParmVarDecl *param = method->getParamDecl(i);
		os << (is_callback(param->getOriginalType()) ? "final " : "")
		   << paramtype2java(param) << " " << param->getNameAsString();
	}
	os << ") {" << endl;

	if (!is_void) {
		os << "        " << rettype2jni(method) << " res;" << endl;
	}

	const string ctx = clazz.is_ctx() ? "this" : "this.ctx";
	os << "        synchronized(" << ctx << ") {" << endl;

	// Look for already collected java wrapper objects and free connected
	// isl structure.
	os << "            " << ctx << ".freeIslC();" << endl;

	// Declare variable 'self' which represents 'this' but
	// with the required isl type (i.e., possibly a super type
	// of the actual class).
	os << "            " << p_name << " self = this.as" << p_name << "();"
	   << endl;

	for (int i = 1; i < num_params - drop_user; ++i) {
		ParmVarDecl *param = method->getParamDecl(i);
		prepare_argument(os, param);
	}
	if (!is_void)
		os << "        res = ";
	os << "Impl." << fullname << "("
	   << isl_ptr(clazz.name, "self", takes(method->getParamDecl(0)));
	for (int i = 1; i < num_params - drop_user; ++i) {
		ParmVarDecl *param = method->getParamDecl(i);
		os << ", ";
		print_argument(os, param);
	}

	os << ");" << endl;

	for (int i = 1; i < num_params - drop_user; ++i) {
		const ParmVarDecl *param = method->getParamDecl(i);
		handle_result_argument(os, "this.ctx", param);
	}
	os << "            " << ctx << ".checkError();" << endl;
	handle_return(os, method, "res");
	os << "        }" << endl
	   << "    }" << endl;

	print_method_jni(method);
}

void java_generator::print_method_jni(FunctionDecl *method) {
	string fullname = method->getName();
	int num_params = method->getNumParams();

	int drop_user = has_user_pointer(method) ? 1 : 0;
	bool is_void = method->getReturnType()->isVoidType();

	QualType retty = method->getReturnType();

	ostream &impl_os = outputfile(packagePath + "Impl.java");
	impl_os << "    static native " << rettype2jni(method) << " " << fullname << "(";
	for (int i = 0; i < num_params-drop_user; ++i) {
		const ParmVarDecl *param = method->getParamDecl(i);
		if (i > 0)
			impl_os << ", ";
		impl_os << paramtype2jni(param) << " "
			<< param->getNameAsString();
	}
	impl_os << ");" << endl;

	ostream &c_os = outputfile(jniSrc);

	int callback = -1;
	for (int i = 0; i < num_params-drop_user; ++i) {
		const ParmVarDecl *param = method->getParamDecl(i);
		QualType ty = param->getOriginalType();
		if (is_callback(ty)) {
			const FunctionProtoType *fn = ty->getPointeeType()->getAs<FunctionProtoType>();
			int num_cbparms = fn->getNumParams();
			QualType retty = fn->getReturnType();
			c_os << retty.getAsString() << " " << fullname << "_callback"
			     << dec << i << "(";
			for (int j=0; j<num_cbparms; ++j) {
				if (j > 0)
					c_os << ", ";
				c_os << fn->getArgType(j).getAsString() << " arg" << dec << j;
			}
			c_os << ") {" << endl
			     << "    struct callbackinfo *cbinfo = (struct callbackinfo *)arg" << dec << (num_cbparms-1) << ";" << endl
			     << "    JNIEnv *env = cbinfo->env;" << endl
			     << "    jobject cb = cbinfo->cb;" << endl;

			string sig = "(";
			for (int j=0; j<num_cbparms-1; ++j) {
				string argty = extract_type(fn->getArgType(j));
				c_os << "    jlong ptr" << dec << j << " = " << islptr2jlong(string("arg") + to_string(j)) << ';' << endl;
				sig += "J";
			}
			string cb_mid;
			if (retty->isIntegerType())
				cb_mid = jniIntCallbackApplyMID(num_cbparms-1);
			else if (is_isl_class(retty))
				cb_mid = jniCallbackApplyMID(num_cbparms-1);
			sig += retty->isIntegerType() ? ")I" : ")J";
			c_os << "    if (" << cb_mid << " == NULL) {" << endl
			     << "      jclass clz = (*env)->GetObjectClass(env, cb);" << endl
			     << "      " << cb_mid << " = (*env)->GetMethodID(env, clz, \"apply\", \"" << sig << "\");" << endl
			     << "    }" << endl;

			c_os << "    ";
			if (retty->isIntegerType())
				c_os << "jint res = (*env)->CallIntMethod(env, cb, " << cb_mid;
			else if (is_isl_class(retty))
				c_os << "jlong res = (*env)->CallLongMethod(env, cb, " << cb_mid;
			for (int j=0; j<num_cbparms-1; ++j)
				c_os << ", ptr" << dec << j;
			c_os << ");" << endl;

			if (retty->isIntegerType())
				c_os << "    return res;" << endl;
			else if (is_isl_class(retty))
				c_os << "    return "
				     << jlong2islptr("res", extract_type(retty))
				     << ';' << endl;
			c_os << "}" << endl;
			callback = i;
		}
	}

	c_os << jnifn(fullname, rettype2jni_c(retty));
	for (int i = 0; i < num_params-drop_user; ++i) {
		const ParmVarDecl *param = method->getParamDecl(i);
		c_os << ", " << paramtype2jni_c(param->getOriginalType()) << " "
		     << param->getNameAsString();
	}
	c_os << ") {" << endl;
	for (int i = 0; i < num_params-drop_user; ++i) {
		const ParmVarDecl *param = method->getParamDecl(i);
		QualType ty = param->getOriginalType();
		const string &pname = param->getNameAsString();
		const string argName = string("arg") + to_string(i);
		if (is_isl_class(ty) || is_isl_ctx(ty)) {
			string c_type = is_isl_ctx(ty) ? "isl_ctx" : extract_type(ty);
			c_os << "    " << c_type << " *" << argName << " = "
			     << jlong2islptr(pname, c_type) << ';' << endl;
		} else if (is_string(ty)) {
			c_os << "    const char *" << argName << " = (*env)->GetStringUTFChars(env, "
			     << pname << ", NULL);" << endl;
		} else if (is_callback(ty)) {
			c_os << "    // There are two memory leaks here: we never free 'cbinfo' and" << endl
			     << "    // we never release the global reference to '" << pname << "'." << endl
			     << "    struct callbackinfo *cbinfo = (struct callbackinfo *)malloc(sizeof(struct callbackinfo));" << endl
			     << "    cbinfo->env = env;" << endl
			     << "    cbinfo->cb  = (*env)->NewGlobalRef(env, " << pname << ");" << endl;
		} else if (ty->isPointerType()) {
			QualType targetty = ty->getPointeeType();
			if (is_isl_class(targetty)) {
				string islType(extract_type(targetty));
				c_os << "    " << islType << " *_" << argName << ';' << endl
				     << "    " << islType << " **" << argName << " = "
				     << "(*env)->IsSameObject(env, " << pname << ", NULL) ? NULL : &_"
				     << argName << ';' << endl;
			} else if (targetty->isIntegerType()) {
				c_os << "    int " << argName << "[1];" << endl;
			}
		} else {
			c_os << "    int " << argName << " = " << pname << ";" << endl;
		}
	}
	c_os << "    ";
	if (!is_void)
		c_os << retty.getAsString() << " res = ";
	c_os << fullname << "(";
	for (int i = 0; i < num_params-drop_user; ++i) {
		if (i > 0)
			c_os << ", ";
		if (i == callback) {
			c_os << fullname << "_callback" << dec << i;
		} else {
			c_os << "arg" << dec << i;
		}
	}
	if (callback >= 0)  // set user arg for callback to "cbinfo"
		c_os << ", cbinfo";
	else if (drop_user)  // set user arg without callback (e.g. for isl_id_alloc) to NULL
		c_os << ", NULL";
	c_os << ");" << endl;
	if ((callback >= 0) && (fullname.find("ast") == string::npos)) {
		c_os << "    (*env)->DeleteGlobalRef(env, cbinfo->cb);" << endl
		     << "    free(cbinfo);" << endl;
	}
	for (int i = 0; i < num_params-drop_user; ++i) {
		const ParmVarDecl *param = method->getParamDecl(i);
		QualType ty = param->getOriginalType();
		const string &pname = param->getNameAsString();
		const string argName = string("arg") + to_string(i);
		if (is_string(ty)) {
			c_os << "    (*env)->ReleaseStringUTFChars(env, " << pname
			     << ", " << argName << ");" << endl;
		} else if (ty->isPointerType()) {
			QualType targetty = ty->getPointeeType();
			if (is_isl_class(targetty)) {
				c_os << "    if (" << argName << " != NULL)" << endl
				     << "        (*env)->SetLongArrayRegion(env, " << pname
				     << ", 0, 1, (jlong *)" << argName << ");" << endl;
			} else if (targetty->isIntegerType()) {
				c_os << "    jint j_" << argName << " = " << argName << "[0];" << endl
				     << "    (*env)->SetIntArrayRegion(env, " << pname
				     << ", 0, 1, &j_" << argName << ");" << endl;
			}
		}
	}
	if (is_isl_class(retty)) {
		c_os << "    return " << islptr2jlong("res") << ';' << endl;
	} else if (is_string(retty)) {
		c_os << "    jobject jstr = (*env)->NewStringUTF(env, res);" << endl;
		if (gives(method))
			c_os << "    free(res);" << endl;
		c_os << "    return jstr;" << endl;
	} else if (!is_void) {
		c_os << "    return res;" << endl;
	}
	c_os << "}" << endl;
}

/* Print part of the constructor for this isl_class.
 *
 * When 'asNamedConstructor' is true, generate a static
 * method with a name matching the isl function name
 * (e.g., Set.readFromStr(...)); otherwise, generate
 * a constructor (e.g., Set.Set(...)).
 * To avoid ambiguties, only a few constructor functions
 * can be represented by constructors of the class (because
 * several constructors can have the same argument list, e.g.
 * isl_set_universe and isl_set_empty).
 */
void java_generator::print_constructor(ostream &os, isl_class &clazz,
				       FunctionDecl *cons,
				       bool asNamedConstructor)
{
	const string fullname = cons->getName();
	const string cname = methodname2java(clazz, fullname);
	const string jclass = type2java(clazz.name);
	int ctxArg = -1, ctxSrc = -1;
	int num_params = cons->getNumParams();
	int drop_user = has_user_pointer(cons) ? 1 : 0;
	int is_ctx = clazz.name.compare("isl_ctx") == 0;
	string super;
	bool subclass = is_subclass(clazz.type, super);

	if (!asNamedConstructor)
		os << "    // " << fullname << endl;
	os << "    public ";
	if (asNamedConstructor)
		os << "static ";
	os << type2java(clazz.name);
	if (asNamedConstructor)
		os << " " << cname;
	os << "(";

	// Check if there is an argument which provides us with
	// the isl context.
	for (int i = 0; i < num_params-drop_user; ++i) {
		QualType ty = cons->getParamDecl(i)->getOriginalType();
		if (is_isl_ctx(ty))
			ctxArg = i;
		else if (is_isl_class(ty))
			ctxSrc = i;
	}

	// When there is no isl class argument that can give us the
	// isl context, there must be an explicit isl context argument
	// (except when the class is "isl_ctx").
	if (!is_ctx && ctxSrc == -1 && ctxArg == -1) {
		cerr << "Cannot generate binding for '" << fullname
		     << "':" << endl << "  no context argument and no argument "
					"to take the context from." << endl;
		exit(1);
	}

	bool firstArg = true;
	for (int i = 0; i < num_params-drop_user; ++i) {
		if (i == ctxArg && ctxSrc >= 0)  // drop context argument
			continue;

		ParmVarDecl *param = cons->getParamDecl(i);
		if (!firstArg)
			os << ", ";
		else
			firstArg = false;
		const string &pname = param->getNameAsString();
		os << paramtype2java(param) << " " << pname;
	}

	os << ") {" << endl;

	string ctx;
	if (!is_ctx) {
		if (ctxSrc >= 0)
			ctx = cons->getParamDecl(ctxSrc)->getNameAsString() + ".ctx";
		else
			ctx = cons->getParamDecl(ctxArg)->getNameAsString();
	}

	if (subclass && !asNamedConstructor) {
		os << "        super(" << ctx << ", 0);" << endl;
	}

	os << "        synchronized(" << (is_ctx ? "Ctx.class" : ctx) << ") {" << endl;

	if (!is_ctx) {
		// Look for already collected java wrapper objects and free connected
		// isl structure.
		os << "            " << ctx << ".freeIslC();" << endl;
	}

	for (int i = 0; i < num_params-drop_user; ++i) {
		if (i == ctxArg && ctxSrc >= 0)
			continue;

		ParmVarDecl *param = cons->getParamDecl(i);
		prepare_argument(os, param);
	}

	os << "            ";
	if (asNamedConstructor)
		os << "long ";
	os << "ptr = Impl." << fullname << '(';
	for (int i = 0; i < num_params-drop_user; ++i) {
		if (i > 0)
			os << ", ";

		if (i == ctxArg) {
			os << ctx << ".ptr";
		} else {
			ParmVarDecl *param = cons->getParamDecl(i);
			print_argument(os, param);
		}
	}
	os << ");" << endl;

	os << "            if (ptr == 0L)" << endl
	   << "                throw new IslException(\"" << fullname
	   << " returned a NULL pointer.\");" << endl;

	for (int i = 0; i < num_params-drop_user; ++i) {
		const ParmVarDecl *param = cons->getParamDecl(i);
		handle_result_argument(os, ctx, param);
	}

	if (!is_ctx)
		os << "            " << ctx << ".checkError();" << endl;

	if (asNamedConstructor) {
		os << "            return new " << jclass << "(";
		if (!is_ctx)
			os << ctx + ", ";
		os << "ptr);" << endl;
	}
	os << "        }" << endl
	   << "    }" << endl;
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
void java_generator::print_class(isl_class &clazz)
{
	const string &name = clazz.name;
	string super;
	string p_name = type2java(name);
	set<FunctionDecl *>::iterator in;
	bool subclass = is_subclass(clazz.type, super);
	bool is_ctx = clazz.is_ctx();

	// We do not free objects of classes that have in-place update
	// (e.g., isl_band). These values exist only in dependence of
	// parent objects and are freed when the parent object goes away.
	bool must_be_freed = !is_inplace(clazz);

	ostream &os = outputfile(packagePath + p_name + ".java");
	os << commonHeader;
	if (is_ctx)
		os << "import java.lang.ref.Reference;" << endl;
	os << "public class " << p_name;
	if (subclass)
		os << " extends " << type2java(super);
	os << " {" << endl;
	if (!subclass) {
		if (is_ctx)
			os << "    final ReferenceQueue<Object> refQ = new ReferenceQueue<Object>();" << endl
			   << "    final DLnkdPhntmRef refList = DLnkdPhntmRef.createListDelims();" << endl;
		else
			os << "    final protected Ctx ctx;" << endl
			   << "    final protected DLnkdPhntmRef ref;" << endl;
		os << "    protected long ptr;" << endl;
		if (is_ctx)
			os << "    " << p_name << "(long cPtr) {" << endl;
		else
			os << "    " << p_name << "(Ctx ctx, long cPtr) {" << endl;
		os << "        assert cPtr != 0L;" << endl;
		if (!is_ctx) {
			os << "        this.ctx = ctx;" << endl;
			if (must_be_freed)
				os << "        this.ref = createPhRef(ctx, cPtr);" << endl
				   << "        this.ref.setPointer(cPtr);" << endl
				   << "        this.ref.insertAfter(ctx.refList);" << endl;
			else
				os << "        this.ref = null;" << endl;
		}
		os << "        this.ptr = cPtr;" << endl
		   << "    }" << endl;
		if (!is_ctx)
		   os << "    public Ctx getCtx() { return ctx; }" << endl;
	} else { // Ctx is not a subclass
		os << "    " << p_name << "(Ctx ctx, long cPtr) {" << endl
		   << "        super(ctx, cPtr);" << endl
		   << "    }" << endl;
	}

	if (!is_ctx)
		os << "    long makePtr0() { " << endl
		   << "        long p = this.ptr;" << endl
		   << "        this.ptr = 0L;" << endl
		   << "        if (this.ref.ptr != 0L)" << endl
		   << "            this.ref.remove();" << endl
		   << "        this.ref.ptr = 0L;" << endl
		   << "        return p;" << endl
		   << "    }" << endl;

	for (in = clazz.constructors.begin(); in != clazz.constructors.end();
	     ++in) {
		// TODO: Unnamed constructors do not work with the phantom references
//		print_constructor(os, clazz, *in, false);
		print_constructor(os, clazz, *in, true);
		print_method_jni(*in);
	}
	for (in = clazz.named_constructors.begin(); in != clazz.named_constructors.end();
	     ++in) {
		print_constructor(os, clazz, *in, true);
		print_method_jni(*in);
	}

	if (must_be_freed) {
		if (is_ctx)
			os << "    protected void finalize() {" << endl
			   << "        synchronized(this) {" << endl
			   << "            this.freeIslC();" << endl
			   << "            Impl." << name << "_free(ptr);" << endl
			   << "        }" << endl
			   << "    }" << endl;
		else
			os << "    private static final class FinalDLnkdPhntmRef extends DLnkdPhntmRef {" << endl
			   << "        FinalDLnkdPhntmRef(Object referent, ReferenceQueue<? super Object> refQ) {" << endl
			   << "            super(referent, refQ);" << endl
			   << "        }" << endl
			   << "        void freeCPtr() {" << endl
			   << "            Impl." << name << "_free(ptr);" << endl
			   << "        }" << endl
			   << "    }" << endl
			   << "    protected DLnkdPhntmRef createPhRef(Ctx ctx, long cPtr) {" << endl
			   << "        return new FinalDLnkdPhntmRef(this, ctx.refQ);" << endl
			   << "    }" << endl;
	}

	if (can_be_printed(clazz)) {
		os << "    public String toString() {" << endl
		   << "        Printer p = Printer.toStr(this.ctx);" << endl
		   << "        p = p.print" << p_name << "(this);" << endl
		   << "        return p.getStr();" << endl << "    }" << endl;
	}

	// Print methods to convert the runtime type to any
	// of the super classes. E.g., for BasicSet we have
	// asBasicSet(), asSet() and asUnionSet().
	// Print a method to convert to the own type and walk
	// up the superclass relation to print methods for all
	// superclasses of the current type.
	os << "    public " << p_name << " as" << p_name
	   << "() { return this; }" << endl;
	if (subclass) {
		isl_class *superclass = &classes[super];
		while (superclass) {
			string supername = type2java(superclass->name);
			os << "    public " << supername << " as" << supername
			   << "() {" << endl
			   << "        return " << supername << ".from" << p_name
			   << "(this);" << endl
			   << "    }" << endl;
			string supersuper;
			if (is_subclass(superclass->type, supersuper)) {
				superclass = &classes[supersuper];
			} else {
				superclass = 0;
			}
		}
	}
	os << endl;


	for (in = clazz.methods.begin(); in != clazz.methods.end(); ++in)
		print_method(os, clazz, *in, subclass, super);

	os << "\n";
	if (name.compare("isl_val") == 0)
		print_additional_val_methods(os);

	if (name.compare("isl_ctx") == 0)
		print_additional_ctx_methods(os);

	os << "}" << endl;

	bool has_copy = name.compare("isl_schedule") != 0 && name.compare("isl_printer") != 0
			&& !clazz.is_ctx();

	// Add isl_* functions to Impl interface.
	ostream &impl_os = outputfile(packagePath + "Impl.java");
	impl_os << "    static native void " << name << "_free(long islobject);" << endl;
	if (has_copy) {
		impl_os << "    static native long "
			<< name << "_copy(long islobject);" << endl;
	}

	ostream &c_os = outputfile(jniSrc);
	c_os << jnifn(name + "_free", "void") << ", jlong ptr)" << "{" << endl
	     << "    " << name << " *p = " << jlong2islptr("ptr", name) << ';' << endl
	     << "    " << name << "_free(p);" << endl
	     << "}" << endl;

	if (has_copy) {
		c_os << jnifn(name + "_copy", "jlong") << ", jlong ptr)" << '{' << endl
		     << "    " << name + " *p = " << jlong2islptr("ptr", name) << ';' << endl
		     << "    p = " << name << "_copy(p);" << endl
		     << "    return " << islptr2jlong("p") << ';' << endl
		     << "}" << endl;
	}
}

void java_generator::generate()
{
	generateClasses();
	generateEnums();
}

void java_generator::generateClasses()
{
	const char *isl_includes[] =
	{ "aff", "aff_type", "arg", "ast_build", "ast", "ast_type",
	  "band", "constraint", "ctx", "flow", "hash",
	  "id", "id_to_ast_expr", "id_to_pw_aff", "ilp", "list",
	  "local_space", "lp", "map", "map_to_basic_set", "map_type",
	  "mat", "multi", "obj", "options", "point", "polynomial",
	  "polynomial_type", "printer", "schedule", "set", "set_type",
	  "space", "stdint", "stream", "union_map", "union_map_type",
	  "union_set", "union_set_type", "val", "vec", "vertices"
	};

	ostream &c_os = outputfile(jniSrc);
	c_os << "#include <jni.h>" << endl;
	for (unsigned i=0; i<sizeof(isl_includes)/sizeof(*isl_includes); ++i) {
		c_os << "#include <isl/" << isl_includes[i] << ".h>" << endl;
	}

	for (unsigned nArgs = 1; nArgs <= 3; ++nArgs) {
		ostringstream intfname_oss;
		intfname_oss << "Callback" << dec << nArgs;
		const string intfname = intfname_oss.str();
		ostringstream args_oss, tys_oss, jni_args_oss;
		for (unsigned i = 1; i <= nArgs; ++i) {
			if (i > 1) {
				args_oss << ", ";
				jni_args_oss << ", ";
				tys_oss << ",";
			}
			args_oss << "ArgTy" << dec << i << " arg" << dec << i;
			jni_args_oss << "long arg" << dec << i;
			tys_oss << "ArgTy" << dec << i;
		}
		const string args = args_oss.str();
		const string jni_args = jni_args_oss.str();
		const string tys = tys_oss.str();

		{
			ostream &os = outputfile(packagePath + string("Void") +
						 intfname + string(".java"));
			os << commonHeader << "public interface Void"
			   << intfname << "<" << tys << "> {" << endl
			   << "    void apply(" << args << ");" << endl << "}"
			   << endl;
		}

		{
			ostream &os =
			    outputfile(packagePath /* + string("Ex") */ +
				       intfname + string(".java"));
			os << commonHeader << "public interface " << intfname
			   << "<" << tys << ",RetTy> {" << endl
			   << "    RetTy apply(" << args << ");" << endl << "}"
			   << endl;
		}

		{
			ostream &os =
			    outputfile(packagePath + string("JNI") +
				       intfname + string(".java"));
			os << commonHeader << "public interface JNI" << intfname
			   << " {" << endl
			   << "    long apply(" << jni_args << ");" << endl << "}"
			   << endl;
		}

		{
			ostream &os =
			    outputfile(packagePath + string("Int") +
				       intfname + string(".java"));
			os << commonHeader << "public interface Int" << intfname
			   << "<" << tys << "> {" << endl
			   << "    int apply(" << args << ");" << endl << "}"
			   << endl;
		}

		{
			ostream &os =
			    outputfile(packagePath + string("JNIInt") +
				       intfname + string(".java"));
			os << commonHeader << "public interface JNIInt" << intfname
			   << " {" << endl
			   << "    int apply(" << jni_args << ");" << endl << "}"
			   << endl;
		}
	}

	ostream &os_impl = outputfile(packagePath + "Impl.java");
	os_impl << commonHeader << "class Impl {" << endl
		<< "    static { System.loadLibrary(\"isl_jni\"); }" << endl
		<< "    static native int isl_ctx_last_error(long ctx);" << endl
		<< "    static native void isl_ctx_reset_error(long ctx);" << endl;

	ostream &os_c = outputfile(jniSrc);
	os_c << "struct callbackinfo {" << endl
	     << "    JNIEnv *env;" << endl
	     << "    jobject cb;" << endl
	     << "};" << endl;

	map<string, isl_class>::iterator ci;
	for (int i = 1; i <= 3; ++i) {
		os_c << "static jmethodID " << jniCallbackApplyMID(i) << " = NULL;" << endl;
		os_c << "static jmethodID " << jniIntCallbackApplyMID(i) << " = NULL;" << endl;
	}

	/*
	os_c << jnifn("isl_ctx_alloc", "jobject") << ") {"
	     << "    isl_ctx *ctx = isl_ctx_alloc();" << endl
	     << "    jobject octx;" << endl
	     << jniMkPtr("ctx", "isl_ctx", "octx")
	     << "    return octx;" << endl
	     << "}" << endl;
	*/
	os_c << jnifn("isl_ctx_last_error", "jint") << ", jlong lctx) {" << endl
	     << "    isl_ctx *ctx = " << jlong2islptr("lctx", "isl_ctx") << ';' << endl
	     << "    return isl_ctx_last_error(ctx);" << endl
	     << "}" << endl;
	os_c << jnifn("isl_ctx_reset_error", "void") << ", jlong lctx) {" << endl
	     << "    isl_ctx *ctx = " << jlong2islptr("lctx", "isl_ctx") << ';' << endl
	     << "    isl_ctx_reset_error(ctx);" << endl
	     << "}" << endl;

	{
		ostream &os = outputfile(packagePath + "IslException.java");
		os << commonHeader
		   << "public class IslException extends RuntimeException {"
		   << endl
		   << "    public IslException(String msg) { super(msg); }"
		   << endl << "    public IslException(String msg, Throwable "
			      "cause) { super(msg, cause); }" << endl << "}"
		   << endl;
	}

	for (ci = classes.begin(); ci != classes.end(); ++ci) {
		print_class(ci->second);
	}

	os_impl << "}" << endl;
}

void java_generator::print_enum(const isl_enum &enu)
{
	const string e_name = type2java(enu.name);
	ostream &os = outputfile(packagePath + e_name + ".java");
	os << commonHeader;
	os << "public enum " << e_name << " {" << endl;
	map<string, int>::const_iterator it;
	for (it = enu.values.begin(); it != enu.values.end(); ++it) {
		os << "    " << enumval2java(enu, it->first) << "(" << it->second
		   << ")," << endl;
	}
	os << "    ;" << endl << "    int value;" << endl << "    " << e_name
	   << "(int value) { this.value = value; }" << endl << "}" << endl;
}

void java_generator::generateEnums()
{
	map<string, isl_enum>::iterator ei;
	for (ei = enums.begin(); ei != enums.end(); ++ei)
		print_enum(ei->second);
}

java_generator::java_generator(set<RecordDecl *> &types,
			       set<FunctionDecl *> &functions,
			       set<EnumDecl *> &enums)
    : generator(types, functions, enums)
{
}
