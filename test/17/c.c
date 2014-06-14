
#include <assert.h>

struct S {
	int i;
	float f;
	double d;
};


struct S c_test(int i)
{
	assert(i == 1);
	struct S s;
	s.i = i;
	s.f = (float)i * 2;
	s.d = (double)i * 3;
	return s;
}
