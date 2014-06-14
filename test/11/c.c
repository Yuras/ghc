
#include <assert.h>

struct S {
	int i1;
	int i2;
	float f1;
	float f2;
};

struct S c_test(int i)
{
	assert(i == 1);
	struct S s;
	s.i1 = 1;
	s.i2 = 2;
	s.f1 = 3.3;
	s.f2 = 4.4;
	return s;
}
