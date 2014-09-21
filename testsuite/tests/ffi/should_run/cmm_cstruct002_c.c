//
// Test alignment and pointer support
//

#include <assert.h>

typedef struct S {
    char c;
    int i;
    int* p;
} S;

static int s_int = 4;

S c_test(int i) {
    assert(i == 1);
    S s;
    s.c = 3;
    s.i = i * 2;
    s.p = &s_int;
    return s;
}
