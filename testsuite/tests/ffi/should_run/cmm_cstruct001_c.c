//
// Corner case: struct with one field
//

#include <assert.h>

typedef struct S {
    int i;
} S;

S c_test(int i) {
    assert(i == 1);
    S s;
    s.i = i * 2;
    return s;
}
