// Первый элемент списка
#define FIRST(o) (GET_PAIR(o)->left)

// Второй элемент списка
#define SECOND(o) (GET_PAIR(GET_PAIR(o)->right)->left)

// Третий элемент списка
#define THIRD(o) (GET_PAIR(GET_PAIR(GET_PAIR(o)->right)->right)->left)

// Хвост списка
#define TAIL(o) (GET_PAIR(o)->right)

object_t *eval(object_t *obj, object_t *env);
void init_eval();
