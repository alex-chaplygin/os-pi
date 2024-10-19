#define MAX_TAGBODY_SIZE 10

object_t eval(object_t obj, object_t env, object_t func_env);
void init_eval();

extern object_t t;
extern object_t nil;
