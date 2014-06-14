
#include <assert.h>

struct S {
	int i1;
	char i2;
	char i3;
	int i4;
};

struct S c_test(int i)
{
	assert(i == 1);
	struct S s;
	s.i1 = 1;
	s.i2 = 2;
	s.i3 = 3;
	s.i4 = 4;
	return s;
}
