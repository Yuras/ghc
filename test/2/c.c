
#include <assert.h>

struct S {
	int i1;
	int i2;
	int i3;
	int i4;
	int i5;
	int i6;
};

struct S c_test(int i)
{
	assert(i == 1);
	struct S s;
	s.i1 = 1;
	s.i2 = 2;
	s.i3 = 3;
	s.i4 = 4;
	s.i5 = 5;
	s.i6 = 6;
	return s;
}
