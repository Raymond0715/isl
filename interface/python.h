#include "generator.h"

using namespace std;
using namespace clang;

void generate_python(set<RecordDecl *> &types, set<FunctionDecl *> functions);
