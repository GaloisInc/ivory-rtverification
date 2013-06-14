#include <gcc-plugin.h>
#include <coretypes.h>
#include <diagnostic.h>
#include <gimple.h>
#include <tree.h>
#include <tree-flow.h>
#include <tree-pass.h>
#include "utils.h"

#define FN_NAME IDENTIFIER_POINTER(DECL_ASSEMBLER_NAME(current_function_decl))

int plugin_is_GPL_compatible;

extern void print_gimple_stmt(FILE *, gimple, int, int);
extern void print_generic_stmt(FILE *, tree, int);
extern void register_attribute(const struct attribute_spec *attr);

/* Attribute handler callback */
static tree handle_instrument_attribute(tree *node, tree name, tree args,
        int flags, bool *no_add_attrs)
{
#ifdef DEBUG
    fprintf(stderr, "Found attribute\n");
    fprintf(stderr, "\t*node = ");
    print_generic_stmt(stderr, *node, 0);
    fprintf(stderr, "\tname = ");
    print_generic_stmt(stderr, name, 0);
    fprintf(stderr, "\targs = ");
    print_generic_stmt(stderr, args, 0);
#endif

    return NULL_TREE;
}

/* Attribute definition */
static struct attribute_spec instrument_attr = {
    "instrument",                   /* name */
    1,                              /* min_len */
    1,                              /* max_len */
    false,                          /* decl_req */
    false,                          /* type_req */
    false,                          /* fn_type_req */
    handle_instrument_attribute,    /* handler */
    false                           /* affects_type_identity */
};

/* Plugin callback called during attribute registration.
Registered with register_callback(plugin_name, PLUGIN_ATTRIBUTES, register_attributes, NULL)
*/
static void register_attributes(void *event_data, void *data)
{
#ifdef DEBUG
    fprintf(stderr, "Registering attribute `instrument'\n");
#endif
    register_attribute(&instrument_attr);
}

static struct plugin_info instrument_assignments_plugin_info =
{
    .version = "0.1",
    .help = "",
};

static struct plugin_gcc_version myplugin_ver =
{
    .basever = "4.7",
};

static bool instrument_assignments_plugin_gate(void)
{
    return true;
}

// create the function call to a `record_assignment' function and insert it
// before the given statement
static void insert_instrumentation_fn(gimple curr_stmt, tree var_id,
                                      tree new_value)
{
    // build function declaration
    tree proto = build_function_type_list(
            void_type_node,      /* return type */
            integer_type_node,   /* first arg's type */
            const_ptr_type_node, /* second arg's type */
            NULL_TREE);
    tree decl = build_fn_decl("record_assignment", proto);

    // build the function call with the new value tree and the variable id tree
    // and insert it before the statement that was passed as the first argument
    gimple call = gimple_build_call(decl, 2, var_id, new_value);
    gimple_stmt_iterator gsi = gsi_for_stmt(curr_stmt);
    gsi_insert_before(&gsi, call, GSI_NEW_STMT);
}

static unsigned int instrument_assignments_plugin_exec(void)
{
#ifdef DEBUG
    fprintf(stderr, "* Inspecting function `%s'\n", FN_NAME);
#endif

    basic_block bb;
    FOR_EACH_BB(bb) {
        gimple_stmt_iterator gsi;
        for (gsi = gsi_start_bb(bb) ; !gsi_end_p(gsi) ; gsi_next(&gsi)) {
            gimple curr_stmt = gsi_stmt(gsi);
            tree lhs = gimple_get_lhs(curr_stmt);

            // We only care about assignments to “real” variables – i.e. not
            // variable versions that were created as part of the transformation
            // to SSA form.
            if (gimple_code(curr_stmt) == GIMPLE_ASSIGN
             && lhs != NULL_TREE && TREE_CODE(lhs) != SSA_NAME && DECL_P(lhs)) {

                tree attrlist = DECL_ATTRIBUTES(lhs);
                tree attr = lookup_attribute("instrument", attrlist);

                // the princess is in another castle
                if (attr == NULL_TREE) continue;

                // read the variable id that was passed to the `instrument'
                // attribute
                const_tree arg = TREE_VALUE(TREE_VALUE(attr));
                tree var_id = build_int_cst(NULL_TREE, tree_low_cst (arg, 1));

#ifdef DEBUG
                fprintf(stderr, "  > found assignment to instrumented variable `%s': \n\t", get_name(lhs));
                print_gimple_stmt(stderr, curr_stmt, 0, 0);
                fprintf(stderr, "\tbase address of `%s' is %p\n", get_name(lhs), get_base_address(lhs));
#endif

                // insert our instrumentation function before the current
                // statement and pass along the rhs (i.e. the new value)
                tree rhs = gimple_op(curr_stmt, 1);
                insert_instrumentation_fn(curr_stmt, var_id, rhs);
            }
        }
    }
#ifdef DEBUG
    fprintf(stderr, "\n");
#endif

    return 0;
}

static struct gimple_opt_pass instrument_assignments_plugin_pass =
{
    {
        GIMPLE_PASS,                                      // type
        "instrument assignments plugin",                  // name
        instrument_assignments_plugin_gate,               // gate
        instrument_assignments_plugin_exec,               // execute
        NULL,                                             // sub
        NULL,                                             // next
        0,                                                // static_pass_number
        TV_NONE,                                          // tv_id
        PROP_gimple_any,                                  // properties_required
        0,                                                // properties_provided
        0,                                                // properties_destroyed
        0,                                                // todo_flags_start
        TODO_update_ssa|TODO_verify_ssa|TODO_cleanup_cfg, // todo_flags_finish
    }
};

int plugin_init(struct plugin_name_args *info, struct plugin_gcc_version *ver)
{
    if (strncmp(ver->basever, myplugin_ver.basever, strlen("X.Y"))) {
      fprintf(stderr, "Your compiler: %s\n", ver->basever);
      fprintf(stderr, "Sorry, you need to use GCC 4.7.\n");
      return -1;
    }

#ifdef DEBUG
    fprintf(stderr, "Initializing plugin\n");
#endif

    struct register_pass_info pass;
    pass.pass = &instrument_assignments_plugin_pass.pass;
    pass.reference_pass_name = "ssa";
    pass.ref_pass_instance_number = 1;
    pass.pos_op = PASS_POS_INSERT_AFTER;

    // Tell gcc we want to be called after the first SSA pass
    register_callback("instrument_plugin", PLUGIN_PASS_MANAGER_SETUP, NULL, &pass);
    register_callback("instrument_plugin", PLUGIN_INFO, NULL, &instrument_assignments_plugin_info);
    register_callback("instrument_plugin", PLUGIN_ATTRIBUTES, register_attributes, NULL);

    return 0;
}
