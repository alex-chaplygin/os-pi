object_t eval(object_t obj, object_t env);
object_t funcall(object_t fun, object_t args);
object_t atom(object_t obj1);
object_t eq(object_t p1, object_t p2);
object_t error_func(object_t args);
object_t apply(object_t fun, object_t args);
object_t call_form(func0_t f, object_t args, int nary, int args_count, int count);
object_t lisp_eval(object_t args);
void init_eval();
#ifdef DEBUG
extern object_t debug_stack;
void print_debug_stack();
#endif

extern object_t t;
extern object_t nil;
/// точка вычисления меток tagbody
extern continuation_t *tagbody_buffers;
///текущий индекс-буфер для tagbody
extern int tb_index_buf;
/// точка вычисления меток catch
extern catch_t *catch_buffers;
/// текущий индекс-буфер для catch
extern int ct_index_buf;
/// текущее окружение
extern object_t current_env;
/// окружение для пользовательских функций
extern object_t func_env;
