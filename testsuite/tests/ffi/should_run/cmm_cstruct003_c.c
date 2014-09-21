//
// passign on stack on x86_64
//

#include <assert.h>

typedef struct S {
    int i1;
    int i2;
    int i3;
    int i4;
    int i5;
} S;

S c_test(int i) {
    assert(i == 1);
    S s;
    s.i1 = i * 1;
    s.i2 = i * 2;
    s.i3 = i * 3;
    s.i4 = i * 4;
    s.i5 = i * 5;
    return s;
}
