object_t eval(object_t obj, object_t env, object_t func_env);
void init_eval();
#ifdef DEBUG
extern object_t debug_stack;
void print_debug_stack();
#endif

extern object_t t;
extern object_t nil;
