#define STACK_SIZE (64 * 1024)
#define FRAME_SIZE 1024

// Элемент стека CATCH
typedef struct {
    object_t label; // метка CATCH
    int addr; // абсолютный адрес конца блока CATCH
    object_t frame_reg; // значение регистра кадра активации в момент вызова CATCH
    object_t *stack_top; // указатель на вершину стека в момент вызова CATCH
} catch_t;

//Функция инициализации виртуальной машины
void vm_init(int *prog_mem, int prog_size,
			  object_t *const_mem, int const_c, int glob_var_c);

//Функция запуска виртуальной машины
void vm_run();
void vm_dump();

// операции
void const_inst();
void jmp_inst();
void jnt_inst();
void alloc_inst();
void global_ref_inst();
void global_set_inst();
void local_ref_inst();
void local_set_inst();
void deep_ref_inst();
void deep_set_inst();
void push_inst();
void pack_inst();
void reg_call_inst();
void return_inst();
void fix_closure_inst();
void save_frame_inst();
void set_frame_inst();
void restore_frame_inst();
void prim_inst();
void nprim_inst();
void halt();
void prim_closure();
void nprim_closure();
void catch_inst();
void throw_inst();
void vm_garbage_collect();
