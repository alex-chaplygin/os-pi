#include <stdio.h>
#include "objects.h"
#include "symbols.h"
#include "eval.h"
#include "parser.h"


object_t *intern(object_t *list)
{

}
void init_strings()
{
    register_func("INTERN", intern);
}
