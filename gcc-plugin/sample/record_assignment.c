#include <stdio.h>
#include <assert.h>

#define HIST_LENGTH 128
#define MAX_VARS 10

static int var_history[MAX_VARS][HIST_LENGTH];
static int indexes[MAX_VARS] = {0};

void record_assignment(const int var_id, const void *new_value)
{
  int ix = indexes[var_id];
  var_history[var_id][ix] = (int) new_value;

  int new_ix = ix + 1 % HIST_LENGTH;
  indexes[var_id] = new_ix;

  fprintf(stderr,
          "Variable with id %d modified. New value = %d. History:\n",
          var_id,
          (int) new_value);

  for (unsigned int i = 0; i < new_ix; i++) {
    fprintf(stderr,
            "\tvar_history[%d][%d] = %d\n",
            var_id,
            i,
            var_history[var_id][i]);
  }
}
