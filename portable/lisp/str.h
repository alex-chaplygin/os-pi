#define MAX_ITOA_STR 15

object_t concat(object_t list);
object_t print_object(object_t obj);
object_t intern(object_t arg);
object_t string_size(object_t str);
object_t str_char(object_t str, object_t index);
object_t symbol_name(object_t symbol);
object_t symbol_function(object_t list);
object_t subseq(object_t str, object_t start_index, object_t end_index);
object_t int_to_str(object_t number);
object_t code_char(object_t code);
object_t char_code(object_t code);
object_t make_string(object_t size, object_t char_obj);
object_t sets(object_t str, object_t index, object_t char_obj);
object_t gensym();

void init_strings();
