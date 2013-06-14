#include "monitored_task.h"
#include "checker_task.h"

int main(void) {
  while(1) {
    update();
    verify_updates();
  }
  return 0;
}
