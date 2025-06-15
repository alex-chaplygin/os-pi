object_t eval(object_t obj, object_t env, object_t func_env);
object_t funcall(object_t fun, object_t args);
object_t atom(object_t obj1);
object_t eq(object_t p1, object_t p2);
object_t error_func(object_t args);
void init_eval();
#ifdef DEBUG
extern object_t debug_stack;
void print_debug_stack();
#endif

extern object_t t;
extern object_t nil;
