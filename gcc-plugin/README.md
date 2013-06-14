% Instrumentation Plugin for GCC 4.7

# Overview

This plugin registers an "instrument" attribute that can be set on variables
that should be monitored for changes and inserts a call to the function
`record_assignment` whenever an assignment to such a variable is found.

# Building

Issuing `make` will build `instrument_plugin.so`. This requires a GCC version
that supports building plugins. You can check if your GCC build supports this by
running the following command:

    $ gcc -print-file-name=plugin

This should return a directory, *not* simply the word "plugin".

If it does not, you may need the fedora package 'gcc-plugin-devel' or similar
for your system.

To test that everything works, you can run `make test` which compiles a simple
sample program with the plugin active.

# "instrument" attribute

The "instrument" attribute can be set on variables you want to monitor. It takes
one argument: the name by which you will identify the variable. Example:

```c
    int t __attribute__((instrument(0))); // instrument `t' and assign it id 0
```

Whenever an assignment to a variable with this attribute is found, a call to the
function `record_assignment(int,void*)` is inserted. This function is passed
the id you assigned to the variable in the `instrument` attribute and the new
value about to be assigned to the variable (as a `void*` argument).

# `record_assignment`

When linking the final program that was compiled with this plugin, the
`record_assignment` function must be provided and this function has the
responsibility of casting the `void *` to the right type based on which variable
was assigned to. The following is an example implementation:

```c
    void record_assignment(const int var_id, const void * new_val)
    {
      switch (var_id) {
      case 0:
        int new_value = (int)new_val;
        printf("The variable with id %d will now be set to %d\n", var_id, new_value);
        break;
      default:
        printf("Unknown id %d\n", var_id);
        assert(false);
    }
```
