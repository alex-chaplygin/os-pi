// Первый элемент списка
#define FIRST(o) o->u.pair->left

// Второй элемент списка
#define SECOND(o) o->u.pair->right->u.pair->left

// Третий элемент списка
#define THIRD(o) o->u.pair->right->u.pair->right->u.pair->left

// Хвост списка
#define TAIL(o) o->u.pair->right

object_t *eval(object_t *obj);
int is_lambda(object_t *list);
void init_eval();
