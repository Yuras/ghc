//
// Test float on windows x86
// passing in register
//

#include <assert.h>

typedef struct S {
    char c;
    float f;
} S;

S c_test(int i) {
    assert(i == 1);
    S s;
    s.c = 3;
    s.f = (float)i * 2;
    return s;
}
