
#include <assert.h>

struct S {
	int i1;
};


struct S c_test(int i1, int i2, int i3, int i4, int i5)
{
	assert(i1 == 1);
	assert(i2 == 2);
	assert(i3 == 3);
	assert(i4 == 4);
	assert(i5 == 5);
	struct S s;
	s.i1 = 1;
	return s;
}
