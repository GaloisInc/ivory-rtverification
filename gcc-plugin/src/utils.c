#include <gcc-plugin.h>
#include <gimple.h>
#include <tree.h>

bool is_instrumented_variable(tree var)
{
    if (var != NULL_TREE && DECL_P(var)) {
        tree attrlist = DECL_ATTRIBUTES(var);
        tree attr = lookup_attribute("instrument", attrlist);

        if  (attr != NULL_TREE) {
            return true;
        }
    }
    return false;
}
