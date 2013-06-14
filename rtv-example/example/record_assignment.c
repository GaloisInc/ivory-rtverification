#include "instrumented.h"

void record_assignment(int var_id, void* value) {
  append_to_history(var_id, value);
}
