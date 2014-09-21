//
// Test nested structure support
//

#include <assert.h>

typedef struct S1 {
    int i1;
    int i2;
} S1;

typedef struct S {
    char c;
    S1 s1;
} S;

S c_test(int i) {
    assert(i == 1);
    S s;
    s.c = i;
    s.s1.i1 = i * 2;
    s.s1.i2 = i * 3;
    return s;
}
