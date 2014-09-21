//
// Test float and double support
//

#include <assert.h>

typedef struct S {
    char c;
    float f;
    double d;
} S;

S c_test(int i) {
    assert(i == 1);
    S s;
    s.c = i;
    s.f = (float)i + 0.4;
    s.d = (double)i + 0.5;
    return s;
}
